{-# LANGUAGE Rank2Types #-}
-- |
-- WOCE climatology
--
module Oceanogr.WGHC (WGHC(..), readWGHCraw, wghc2bin,
                      longitude, latitude, depth) where
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Array.Repa hiding (map, (++), Source)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString.Char8 (double, signed, decimal)
import qualified Data.ByteString.Char8       as B
import Data.Char (isSpace)
import Data.Conduit
import qualified Data.Conduit.List           as CL
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import GHC.Float (double2Float)
import Oceanogr.BinaryIO (appendVecF, writeVecF)
import Numeric.IEEE (nan)
import System.IO (openFile, IOMode(ReadMode), hIsEOF, hClose)
import System.FilePath ((</>))
-- import Debug.Trace

dataDir :: FilePath
dataDir = "/local/data/WGHC/WGHCGRD/"

dataFiles :: [FilePath]
dataFiles = ["wghc0000-0595",
             "wghc0600-1195",
             "wghc1200-1795",
             "wghc1800-2395",
             "wghc2400-2995",
             "wghc3000-3595"]

small, eps :: Float
small = 0.1 -- max error in depth, longitude, latitude
eps   = 1.0e-6 -- for detecting missing values

depth, longitude, latitude :: V.Vector Float
longitude = V.fromList [0,0.5 .. 359.5]
latitude  = V.fromList [(-80),(-79.5) .. 90]
depth     = V.fromList $ [0,10,20,30,40,50,75,100,125,150,175,200,250,300,350,400,500,600,
                          700,800,900,1000,1100,1200,1300,1400] ++ [1500,1750 .. 6000]
im, jm, km :: Int
im = V.length longitude
jm = V.length latitude
km = V.length depth

-- data from one grid
-- comments are mostly copied from read_wghc_climatology.f
data WGHC1 = WGHC1 Float --  longitude
                   Float --  latitude
                   Float --  depth (m) of the k-th level
                   Float --  ETOPO5 bottom depth
                   -- [1]
                   Int   --  number of gridded levels
                   Float --  radius of the influence bubble (km)
                   Float --  decorrelation length scale (km)
                   Float --  mixed layer depth (m), defined as depth where vertical density gradient >= 0.005 kg/m4
                   -- [2]
                   Float --  pressure (dbar)
                   Float --  temperature in situ (deg C)
                   Float --  potemtial temperature (deg C)
                   Float --  salinity (PSU?)
                   -- [3]
                   Float --  dissolved oxygen (ml/L)
                   Float --  silicate (umol/kg)
                   Float --  nitrate (umol/kg)
                   Float --  phosphate (umol/kg)
                   -- [4]
                   Float --  gamma-n (kg/m3)
                   Float --  sigma 0 (kg/m3)
                   Float --  sigma 2 (kg/m3)
                   Float --  sigma 4 (kg/m3)
                   -- [5]
                   Float --  relative optimum interpolation error for T, Theta & S
                   Float --  relative optimum interpolation error for Oxygen
                   Float --  relative optimum interpolation error for Silicate
                   Float --  relative optimum interpolation error for Nitrate
                   Float --  relative optimum interpolation error for Phosphate
                   Int   --  actual number of observations used for the optimal interpolation of T, Theta & S
                   Int   --  actual number of observations used for the optimal interpolation of Oxygen
                   Int   --  actual number of observations used for the optimal interpolation of Silicate
                   Int   --  actual number of observations used for the optimal interpolation of Nitrate
                   Int   --  actual number of observations used for the optimal interpolation of Phosphate
                   Float --  temperature standard deviation  from the mean (within the influence radius = radcor)  
                   Float --  salinity    standard deviation  from the mean (within the influence radius = radcor)  
                   Float --  oxygen      standard deviation  from the mean (within the influence radius = radcor)  
                   Float --  silicate    standard deviation  from the mean (within the influence radius = radcor)  
                   Float --  nitrate     standard deviation  from the mean (within the influence radius = radcor)  
                   Float --  phosphate   standard deviation  from the mean (within the influence radius = radcor)  

-- data from whole grids
                  -- [1]
data WGHC = WGHC {getnbrlev  :: V.Vector Int,
                  getradbub  :: V.Vector Float,
                  getradcor  :: V.Vector Float,
                  getdepthml :: V.Vector Float,
                  -- [2]
                  getpres    :: V.Vector Float,
                  gettemp    :: V.Vector Float,
                  getptem    :: V.Vector Float,
                  getsalt    :: V.Vector Float,
                  -- [3]
                  getdoxy    :: V.Vector Float,
                  getsili    :: V.Vector Float,
                  getntra    :: V.Vector Float,
                  getpsha    :: V.Vector Float,
                  -- [4]
                  getgamn    :: V.Vector Float,
                  getsig0    :: V.Vector Float,
                  getsig2    :: V.Vector Float,
                  getsig4    :: V.Vector Float,
                  -- [5]
                  geterr1    :: V.Vector Float,
                  geterr2    :: V.Vector Float,
                  geterr3    :: V.Vector Float,
                  geterr4    :: V.Vector Float,
                  geterr5    :: V.Vector Float,
                  getlev1    :: V.Vector Int,
                  getlev2    :: V.Vector Int,
                  getlev3    :: V.Vector Int,
                  getlev4    :: V.Vector Int,
                  getlev5    :: V.Vector Int,
                  getvar1    :: V.Vector Float,
                  getvar2    :: V.Vector Float,
                  getvar3    :: V.Vector Float,
                  getvar4    :: V.Vector Float,
                  getvar5    :: V.Vector Float,
                  getvar6    :: V.Vector Float
                }
--
-- Because 'sink' needs access to V.Vector's in WGHC
-- it is defined within IO ()
--
readWGHCraw :: IO WGHC
readWGHCraw = do
    let len2 = im * jm
        len3 = len2 * km

    -- [1]
    nbrlev <- VM.replicate len2 0   :: IO (VM.IOVector Int)
    radbub <- VM.replicate len2 0   :: IO (VM.IOVector Float)
    radcor <- VM.replicate len2 0   :: IO (VM.IOVector Float)
    depthml<- VM.replicate len2 0   :: IO (VM.IOVector Float)
    -- [2]
    pres   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    temp   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    ptem   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    salt   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    -- [3]
    doxy   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    sili   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    ntra   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    psha   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    -- [4]
    gamn   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    sig0   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    sig2   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    sig4   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    -- [5]
    err1   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    err2   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    err3   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    err4   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    err5   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    lev1   <- VM.replicate len3 0   :: IO (VM.IOVector Int)
    lev2   <- VM.replicate len3 0   :: IO (VM.IOVector Int)
    lev3   <- VM.replicate len3 0   :: IO (VM.IOVector Int)
    lev4   <- VM.replicate len3 0   :: IO (VM.IOVector Int)
    lev5   <- VM.replicate len3 0   :: IO (VM.IOVector Int)
    var1   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    var2   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    var3   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    var4   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    var5   <- VM.replicate len3 nan :: IO (VM.IOVector Float)
    var6   <- VM.replicate len3 nan :: IO (VM.IOVector Float)

    -- parse and output using Conduit
    let sink :: ConduitT B.ByteString Void (ResourceT IO) ()
        sink = do
            s' <- await
            case s' of
                Nothing -> return ()
                Just s  -> if (Prelude.length . B.words) s == 4 -- 2D data
                             then do
                                  c <- read3D                   -- 3D data
                                  fillWGHCs s c
                                  sink
                             else sink

        read3D :: forall o. ConduitT B.ByteString o (ResourceT IO) [B.ByteString]
        read3D = do
            ss' <- await
            case ss' of
                Nothing -> return []
                Just ss -> if (Prelude.length .  B.words) ss == 4
                             then leftover ss >> return []
                             else read3D >>= \cc -> return (ss:cc)

        fillWGHCs ::   B.ByteString -- ^ first line (header)
                    -> [B.ByteString] -- ^ rest (body)
                    -> ConduitT B.ByteString Void (ResourceT IO) ()
        fillWGHCs header body
          = let fillWGHC :: WGHC1 -> ConduitT B.ByteString Void (ResourceT IO) ()
                fillWGHC (WGHC1 x  y  z  _
                                n  rb rc d     -- [1]
                                p  t  pt s     -- [2]
                                o  si na pa    -- [3]
                                g  s0 s2 s4    -- [4]
                                e1 e2 e3 e4 e5 -- [5]
                                l1 l2 l3 l4 l5
                                v1 v2 v3 v4 v5 v6)
                  = unless (isNaN (x + y + z))
                         $ let ijk = findGrid x longitude >>= \i0 -> -- Maybe monad
                                     findGrid y latitude  >>= \j0 ->
                                     findGrid z depth     >>= \k0 ->
                                     return (i0, j0, k0)
                            in case ijk of
                                 Nothing -> error "fillWGHC(): data out of range"
                                 Just (ii,jj,kk) -> do
                                   let ind2 = toIndex (ix2 jm im) (ix2 jj ii)
                                       ind3 = toIndex (ix3 km jm im) (ix3 kk jj ii)
                                   -- [1]
                                   liftIO $ VM.write nbrlev  ind2 n
                                   liftIO $ VM.write radbub  ind2 rb
                                   liftIO $ VM.write radcor  ind2 rc
                                   liftIO $ VM.write depthml ind2 d
                                   -- [2]
                                   liftIO $ VM.write pres    ind3 p
                                   liftIO $ VM.write temp    ind3 t
                                   liftIO $ VM.write ptem    ind3 pt
                                   liftIO $ VM.write salt    ind3 s
                                   -- [3]
                                   liftIO $ VM.write doxy    ind3 o
                                   liftIO $ VM.write sili    ind3 si
                                   liftIO $ VM.write ntra    ind3 na
                                   liftIO $ VM.write psha    ind3 pa
                                   -- [4]
                                   liftIO $ VM.write gamn    ind3 g
                                   liftIO $ VM.write sig0    ind3 s0
                                   liftIO $ VM.write sig2    ind3 s2
                                   liftIO $ VM.write sig4    ind3 s4
                                   -- [5]
                                   liftIO $ VM.write err1    ind3 e1
                                   liftIO $ VM.write err2    ind3 e2
                                   liftIO $ VM.write err3    ind3 e3
                                   liftIO $ VM.write err4    ind3 e4
                                   liftIO $ VM.write err5    ind3 e5
                                   liftIO $ VM.write lev1    ind3 l1
                                   liftIO $ VM.write lev2    ind3 l2
                                   liftIO $ VM.write lev3    ind3 l3
                                   liftIO $ VM.write lev4    ind3 l4
                                   liftIO $ VM.write lev5    ind3 l5
                                   liftIO $ VM.write var1    ind3 v1
                                   liftIO $ VM.write var2    ind3 v2
                                   liftIO $ VM.write var3    ind3 v3
                                   liftIO $ VM.write var4    ind3 v4
                                   liftIO $ VM.write var5    ind3 v5
                                   liftIO $ VM.write var6    ind3 v6
                                   return ()

             in mapM_ fillWGHC $ parseGrid header body

    runResourceT . runConduit $ source .| fileRead .| sink
    
    -- [1]
    nbrlev_  <- V.unsafeFreeze nbrlev
    radbub_  <- V.unsafeFreeze radbub
    radcor_  <- V.unsafeFreeze radcor
    depthml_ <- V.unsafeFreeze depthml
    -- [2]
    pres_   <- V.unsafeFreeze pres
    temp_   <- V.unsafeFreeze temp
    ptem_   <- V.unsafeFreeze ptem
    salt_   <- V.unsafeFreeze salt
    -- [3]
    doxy_   <- V.unsafeFreeze doxy
    sili_   <- V.unsafeFreeze sili
    ntra_   <- V.unsafeFreeze ntra
    psha_   <- V.unsafeFreeze psha
    -- [4]
    gamn_   <- V.unsafeFreeze gamn
    sig0_   <- V.unsafeFreeze sig0
    sig2_   <- V.unsafeFreeze sig2
    sig4_   <- V.unsafeFreeze sig4
    -- [5]
    err1_   <- V.unsafeFreeze err1
    err2_   <- V.unsafeFreeze err2
    err3_   <- V.unsafeFreeze err3
    err4_   <- V.unsafeFreeze err4
    err5_   <- V.unsafeFreeze err5
    lev1_   <- V.unsafeFreeze lev1
    lev2_   <- V.unsafeFreeze lev2
    lev3_   <- V.unsafeFreeze lev3
    lev4_   <- V.unsafeFreeze lev4
    lev5_   <- V.unsafeFreeze lev5
    var1_   <- V.unsafeFreeze var1
    var2_   <- V.unsafeFreeze var2
    var3_   <- V.unsafeFreeze var3
    var4_   <- V.unsafeFreeze var4
    var5_   <- V.unsafeFreeze var5
    var6_   <- V.unsafeFreeze var6
    return $ WGHC nbrlev_ radbub_ radcor_ depthml_ -- [1]
                  pres_   temp_   ptem_   salt_    -- [2]
                  doxy_   sili_   ntra_   psha_    -- [3]
                  gamn_   sig0_   sig2_   sig4_    -- [4]
                  err1_   err2_   err3_   err4_  err5_ -- [5]
                  lev1_   lev2_   lev3_   lev4_  lev5_
                  var1_   var2_   var3_   var4_  var5_ var6_

---
--- Conduit
---
source :: ConduitT () FilePath (ResourceT IO) ()
source = CL.sourceList $ map (dataDir </>) dataFiles

-- read each file under Conduit with bracketP()
fileRead :: ConduitT FilePath B.ByteString (ResourceT IO) ()
fileRead = awaitForever $ \fname ->
                bracketP (openFile fname ReadMode)
                         hClose
                         loop
    where
        loop h = do
            eof <- liftIO $ hIsEOF h
            unless eof
               $ liftIO (B.hGetLine h) >>= yield >> loop h
---
--- parser etc.
--- 
findGrid :: Float -> V.Vector Float -> Maybe Int
findGrid z = V.findIndex (\x -> abs (x - z) < small)

prepstring :: B.ByteString -> B.ByteString
prepstring s0 = let s1 = B.dropWhile isSpace s0
                    s2 = B.takeWhile (not . isSpace) s1
                 in if B.length s2 > 1 && B.last s2 == '.' -- cannot cope with "123."
                      then B.init s2
                      else s2

ff :: Float -> B.ByteString -> Float
ff missing s0 = let x1 = either (const nan) double2Float (parseOnly double (prepstring s0))
                 in if (not . isNaN $ missing) && abs (x1 - missing) < eps
                      then nan
                      else x1

fi :: B.ByteString -> Int
fi s0 = let p :: Either String Int
            p = parseOnly (signed decimal) (prepstring s0)
         in either (const 0) fromIntegral p

parseGrid :: B.ByteString -> [B.ByteString] -> [WGHC1]
parseGrid header body
    -- 3033 format(i2,1x,3f6.0)
  = let nbrlev  = fi $ B.take 2 header
        radbub  = ff nan $ (B.take 6 . B.drop 3) header
        radcor  = ff nan $ (B.take 6 . B.drop 9) header
        depthml = ff nan $ B.drop 15 header
     in map (parseBody nbrlev radbub radcor depthml) body

parseBody :: Int -> Float -> Float -> Float -> B.ByteString -> WGHC1
parseBody nbrlev radbub radcor depthml ll
    -- 5030  format(2f6.1,3f6.0,3f10.6,
    --      *f8.3,f8.2,2f8.3,
    --      *5f5.2,5i4,
    --      *6f8.3,4f10.6)   
    -- 2f6.1 (xgrid,ygrid)
  = let long = ff nan  $ B.take 6 ll
        lati = ff nan  $ (B.take 6 . B.drop 6) ll
    -- 3f6.0 (etdep,par(1,2))
        etdp = ff (-9) $ (B.take 6 . B.drop 12) ll
        dept = ff (-9) $ (B.take 6 . B.drop 18) ll
        pres = ff (-9) $ (B.take 6 . B.drop 24) ll
    -- 3f10.6 (par(3,4,5)
        temp = ff (-9) $ (B.take 10 . B.drop 30) ll
        ptem = ff (-9) $ (B.take 10 . B.drop 40) ll
        salt = ff (-9) $ (B.take 10 . B.drop 50) ll
    -- f8.3  (par(6))
        doxy = ff (-9) $ (B.take 8 . B.drop 60) ll
    -- f8.2   (par(7))
        sili = ff (-9) $ (B.take 8 . B.drop 68) ll
    -- 2f8.3  (par(8),par(9))
        ntra = ff (-9) $ (B.take 8 . B.drop 76) ll
        psha = ff (-9) $ (B.take 8 . B.drop 84) ll
    -- 5f5.2 (error(ipa,k),ipa=1,5)
        err1 = ff (-9) $ (B.take 5 . B.drop 92) ll
        err2 = ff (-9) $ (B.take 5 . B.drop 97) ll
        err3 = ff (-9) $ (B.take 5 . B.drop 102) ll
        err4 = ff (-9) $ (B.take 5 . B.drop 107) ll
        err5 = ff (-9) $ (B.take 5 . B.drop 112) ll
    -- 5i4    (levels(ipa,k),ipa=1,5)
        lev1 = fi $ (B.take 4 . B.drop 117) ll
        lev2 = fi $ (B.take 4 . B.drop 121) ll
        lev3 = fi $ (B.take 4 . B.drop 125) ll
        lev4 = fi $ (B.take 4 . B.drop 129) ll
        lev5 = fi $ (B.take 4 . B.drop 133) ll
    -- 6f8.3 (var(ipa,k),ipa=1,6)
        var1 = ff (-9) $ (B.take 8 . B.drop 137) ll
        var2 = ff (-9) $ (B.take 8 . B.drop 145) ll
        var3 = ff (-9) $ (B.take 8 . B.drop 153) ll
        var4 = ff (-9) $ (B.take 8 . B.drop 161) ll
        var5 = ff (-9) $ (B.take 8 . B.drop 169) ll
        var6 = ff (-9) $ (B.take 8 . B.drop 177) ll
    -- 4f10.6 (par(ipa,k),ipa=10,13)
        gamn = ff (-9) $ (B.take 10 . B.drop 185) ll
        sig0 = ff (-9) $ (B.take 10 . B.drop 195) ll
        sig2 = ff (-9) $ (B.take 10 . B.drop 205) ll
        sig4 = ff (-9) $ B.drop 215 ll
     in WGHC1 long   lati   dept   etdp
              nbrlev radbub radcor depthml  -- [1]
              pres   temp   ptem   salt     -- [2]
              doxy   sili   ntra   psha     -- [3]
              gamn   sig0   sig2   sig4     -- [4]
              err1   err2   err3   err4   err5 -- [5]
              lev1   lev2   lev3   lev4   lev5
              var1   var2   var3   var4   var5   var6


--
-- | convert raw WGHC into binary, which can be viewed with GrADS (WGHC.ctl)
--
wghc2bin :: FilePath -> IO ()
wghc2bin fname = do
    w <- readWGHCraw
    writeVecF fname $ V.map (fromIntegral :: Int -> Float) $ getnbrlev w
    appendVecF fname $ getradbub w
    appendVecF fname $ getradcor w
    appendVecF fname $ getdepthml w
    appendVecF fname $ getpres w
    appendVecF fname $ gettemp w
    appendVecF fname $ getptem w
    appendVecF fname $ getsalt w
    appendVecF fname $ getdoxy w
    appendVecF fname $ getsili w
    appendVecF fname $ getntra w
    appendVecF fname $ getpsha w
    appendVecF fname $ getgamn w
    appendVecF fname $ getsig0 w
    appendVecF fname $ getsig2 w
    appendVecF fname $ getsig4 w
    appendVecF fname $ geterr1 w
    appendVecF fname $ geterr2 w
    appendVecF fname $ geterr3 w
    appendVecF fname $ geterr4 w
    appendVecF fname $ geterr5 w
    appendVecF fname $ V.map (fromIntegral :: Int -> Float) $ getlev1 w
    appendVecF fname $ V.map (fromIntegral :: Int -> Float) $ getlev2 w
    appendVecF fname $ V.map (fromIntegral :: Int -> Float) $ getlev3 w
    appendVecF fname $ V.map (fromIntegral :: Int -> Float) $ getlev4 w
    appendVecF fname $ V.map (fromIntegral :: Int -> Float) $ getlev5 w
    appendVecF fname $ getvar1 w
    appendVecF fname $ getvar2 w
    appendVecF fname $ getvar3 w
    appendVecF fname $ getvar4 w
    appendVecF fname $ getvar5 w
    appendVecF fname $ getvar6 w
