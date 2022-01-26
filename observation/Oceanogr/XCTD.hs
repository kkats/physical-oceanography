{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- |
-- XCTD data handling
--
module Oceanogr.XCTD
(isXCTDall, readXCTD, processXCTD, outputExc)
where

import Oceanogr.CTD
import Oceanogr.CtoS (ctos)
import Oceanogr.CTD.Z (zOnP)
import Oceanogr.LPF (lpfFIR)
import Oceanogr.GSWtools (gsw_p_from_z, gsw_sa_from_sp, gsw_ct_from_t, gsw_rho, gsw_grav)

import Control.Monad (when)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8       as B
import Data.UnixTime
import GHC.Float (double2Float, float2Double)
import Numeric.IEEE (nan)
import System.FilePath ((</>))
import Text.Printf
import System.IO
-- import Debug.Trace

isXCTDall :: FilePath -> Bool
isXCTDall f = f /= "." && f /= ".." && length f > 4 && let suffix = dropWhile (/= '.') f
                                                        in suffix == ".ALL"

-- Process XCTD *.ALL following Uchida et al. (2011)
--
processXCTD :: Maybe Float -- depth correction (CTDdepth = XCTDdepth * (1 + dcorr))
            -> Maybe Float -- thermal bias (CTDtemp = XCTDtemp + tbias)
            -> Maybe Float -- salt bias (CTDsalt = XCTDtemp + sbias)
            -> CTDdata -- input
            -> IO CTDdata
processXCTD dcorr tbias sbias xctd = do
    let d0' = ctdP xctd  -- depth (i.e. must be compiled without -DPRESSURE)
        d0  = case dcorr of
                Nothing -> d0'
                Just dc -> V.map (* (1.0 + dc)) d0'
        t0' = ctdT xctd
        t0  = case tbias of
                Nothing -> t0'
                Just tb -> V.map (+ tb) t0'
        c0  = ctdDO xctd  -- conductivity in DO's place
        s0  = ctdS xctd  -- tentatively used for pressure calculation
        lon = float2Double . stnLongitude . ctdStation $ xctd
        lat = float2Double . stnLatitude . ctdStation $ xctd
     -- 1. Delete first 32 as initial transient
        d1 = V.drop 32 d0
        t1 = V.drop 32 t0
        c1 = V.drop 32 c0
        s1 = V.drop 32 s0
     -- 2. Delete after bottom contact (conductivity jump > 5)
        n2 = 1 + (V.length .  V.takeWhile (\x -> abs x < 5) $  V.zipWith (-) (V.tail c1) c1)
        d2 = V.map float2Double $ V.take n2 d1
        t2 = V.map float2Double $ V.take n2 t1
        c2 = V.map float2Double $ V.take n2 c1
        s2 = V.map float2Double $ V.take n2 s1
     -- 3. LPF with Hamming 19
        t3 = V.fromList . lpfFIR 19 . V.toList $ t2
        c3 = V.fromList . lpfFIR 19 . V.toList $ c2
        s3 = V.fromList . lpfFIR 19 . V.toList $ s2
     -- 4. pressure
        (t4, s4, d4) = V.unzip3 $ V.filter (\(t', s', d') -> (not . isNaN $ t' + s' + d'))
                                $ V.zip3 t3 s3 d2
    p4 <- pressure (lon, lat) t4 s4 d4
     -- 5. conductivity temporal mismatch
    let c4 = (V.fromList [nan,nan]) V.++
              V.zipWith (\c1' c2' -> 0.9 * c1' + 0.1 * c2') (V.tail c3) c3
                                                    -- Eq (2) Uchida et al. (2011)
     -- 6. salinity
        (p5, t5, s5) = V.unzip3 $ V.map (\(p', t', c') -> (p', t', ctos (p' / 1.0e5) (t' * 1.00024) c')) -- IPTS-68
                                $ V.filter (\(p', t', c') -> (not . isNaN $ p' + t' + c'))
                                $ V.zip3 p4 t3 c4
        s6 = case sbias of
                Nothing -> s5
                Just sb -> V.map (+ float2Double sb) s5
     -- 7. 1 dbar binning
        raw = CTDdata (ctdStation xctd) (V.map double2Float . V.map (/ 1.0e4) $ p5) (V.map double2Float t5)
                                        (V.map double2Float s6) (V.map double2Float c4) V.empty V.empty V.empty
        pgrid' = V.fromList [0 .. 2200] :: V.Vector Float
        tgrid' = zOnP T raw pgrid'
        sgrid' = zOnP S raw pgrid'
        cgrid' = zOnP DO raw pgrid'
        (pgrid, tgrid, sgrid, cgrid) = V.unzip4
                                     $ V.filter (\(p',t',s',_) -> (not . isNaN $ p' + t' + s'))
                                     $ V.zip4 pgrid' tgrid' sgrid' cgrid'

    return $ CTDdata (ctdStation xctd) pgrid tgrid sgrid cgrid V.empty V.empty V.empty

--
-- hydrostatic pressure as in (3) of Uchida et al. (2011)
--
-- no NaN's exist in inputs
--
pressure :: (Double, Double) -- ^ longitude, latitude
         -> V.Vector Double -- ^ temperature
         -> V.Vector Double -- ^ salinity
         -> V.Vector Double -- ^ depth
         -> IO (V.Vector Double) -- ^ pressure
pressure (lon, lat) t s d = do
    (rho, g) <- V.unzip `fmap` (V.mapM (\(t0, s0, d0) -> do
                                    p0 <- gsw_p_from_z (negate d0) lat -- tentative
                                    sa <- gsw_sa_from_sp s0 p0 lon lat
                                    ct <- gsw_ct_from_t sa t0 p0
                                    r  <- gsw_rho sa ct p0
                                    g  <- gsw_grav lat p0
                                    return (r, g))
                                $  V.zip3 t s d)
    let pressure' :: Int -> Double
        pressure' 1  = V.head rho * V.head g * V.head d
        pressure' i2 = let i1 = i2 - 1
                           i0 = i2 - 2
                        in (rho V.! i1 + rho V.! i0) * (g V.! i1 + g V.! i0) * (d V.! i1 - d V.! i0) / 4
    return $ V.scanl1' (+) . V.map pressure' $ V.fromList [1 .. (V.length d)]
--
-- XCTD output *.CTD, *.ALL
--
readXCTD :: B.ByteString -> FilePath -> IO CTDdata
readXCTD expo fname = do

    c <- B.readFile fname -- file is so small that we rely on laziness
    let ll = B.lines c
        ww = split . head $ ll
        latitude  = deg2min (ww !! 4)
        longitude = deg2min (ww !! 5)
        stnnbr    = B.filter (/= ' ') . head $ ww -- serial
        cast      = si (ww !! 1) -- "material number"
        date      = ww !! 2
        time      = B.take 4 (ww !! 3)
        body      = tail ll
        maxn      = length body

    -- data
    prs' <- VM.replicate maxn nan :: IO (VM.IOVector Float)
    tem' <- VM.replicate maxn nan :: IO (VM.IOVector Float)
    sal' <- VM.replicate maxn nan :: IO (VM.IOVector Float)
    con' <- VM.replicate maxn nan :: IO (VM.IOVector Float)

    let go :: Int -> IO ()
        go n
            | n >= maxn = return ()
            | otherwise = let w0 = split (body !! n)
                              d  = head w0 
                              t  = w0 !! 1
                              o  = w0 !! 2
                              s  = w0 !! 3 
                           in do
#define PRESSURE
#ifdef PRESSURE
                            p <- gsw_p_from_z (negate . float2Double . sf $ d) (float2Double latitude)
                            VM.write prs' n (double2Float p) -- pressure
#else
                            VM.write prs' n (sf d)           -- depth
#endif
                            VM.write tem' n (sf t)
                            VM.write con' n (sf o)
                            VM.write sal' n (sf s)
                            go (n+1)
    go 0

    prs <- V.unsafeFreeze prs'
    tem <- V.unsafeFreeze tem'
    con <- V.unsafeFreeze con'
    sal <- V.unsafeFreeze sal'

    let (prs_, tem_, con_, sal_) = V.unzip4
                               $ V.filter (\(p0,t0,c0,s0) -> any (not . isNaN) [p0,t0,c0,s0])
                               $ V.zip4 prs tem con sal


    when (V.null prs_ || V.null tem_ || V.null sal_) $ error ("Empty P/T/S? (" ++ fname ++ ")")

    let station   = Station (stnnbr,cast) longitude latitude nan (formTime date time) expo

    return $ CTDdata station prs_ tem_ sal_ con_ V.empty V.empty V.empty -- keep conductivity in DO's place

--
-- parsers
--
deg2min :: B.ByteString -> Float
deg2min d = let (degr, minu) = B.break (== '-') d
                hemi         = B.last minu
                minu'        = sf . B.tail . B.init $ minu
                degr'        = sf degr
             in if (hemi == 'S' || hemi == 'W')
                  then negate $ degr' + minu' / 60
                  else degr' + minu' / 60

-- split at comma
-- comma at the end of line is allowed
split :: B.ByteString -> [B.ByteString]
split l0 = split' l0 []
  where
    split' :: B.ByteString -> [B.ByteString] -> [B.ByteString]
    split' i0 o0
       | B.null i0 = reverse o0
       | otherwise = let (i1, o1) = B.break (== ',') i0
                      in if B.null o1 then reverse (i1:o0) -- comma at the end of line
                                      else split' (B.tail o1) (i1:o0)

--
-- output in Exchange format
--
outputExc :: String -- ^ EXPOCODE
          -> String -- ^ SECTION
          -> FilePath -- ^ output directory
          -> CTDdata
          -> IO ()
outputExc expo sect odir xctd = do
    let (stn, cast) = stnCast . ctdStation $ xctd
        slen        = if B.length stn > 5 then 5 else B.length stn
        stn'        = replicate (5 - slen) '0' ++ B.unpack stn
        fname       = odir </> printf "%s_%s_%05d_xctd.csv" expo stn' cast
        gx :: Float -> Float
        gx a = if isNaN a then -999 else a
        gf :: Float -> Int
        gf a = if isNaN a then 4 else 2
-- https://dornsife.usc.edu/assets/sites/463/docs/WOCE_QualityFlags.pdf
-- 2 "acceptable measurement"
-- 4 "bad measurement"
        date = formatUnixTimeGMT "%Y%m%d" (stnTime . ctdStation $ xctd)
        time = formatUnixTimeGMT "%H%M" (stnTime . ctdStation $ xctd)

    withFile fname WriteMode $ \h -> do
            hPutStrLn h "NUMBER_HEADERS = 10"
            hPutStrLn h $ "EXPOCODE = " ++ expo
            hPutStrLn h $ "SECT_ID = " ++ sect
            B.hPutStrLn h $ "STNNBR = " `B.append` stn
            hPutStrLn h $ "CASTNO = " ++ show cast
            hPutStrLn h $ "DATE = " ++ B.unpack date
            hPutStrLn h $ "TIME = " ++ B.unpack time
            hPrintf h "LATITUDE = %9.4f\n" (stnLatitude . ctdStation $ xctd)
            hPrintf h "LONGITUDE = %9.4f\n" (stnLongitude . ctdStation $ xctd)
            hPrintf h "DEPTH = %.0f\n" (stnDepth . ctdStation $ xctd)
            hPutStrLn h $ "CTDPRS,CTDPRS_FLAG_W,CTDTMP,CTDTMP_FLAG_W,CTDSAL,CTDSAL_FLAG_W"
            hPutStrLn h $ "DBAR,,ITS-90,,PSS-78,,"
            V.mapM_ (\(p',t',s') -> hPrintf h "%10.1f,%d,%10.3f,%d,%10.3f,%d\n"
                                        (gx p') (gf p') (gx t') (gf t') (gx s') (gf s'))
                    $ V.filter (\(p',t',s') -> (not . isNaN $ p'+ t'+s'))
                    $ V.zip3 (ctdP xctd) (ctdT xctd) (ctdS xctd)
            hPutStrLn h "END_DATA"
