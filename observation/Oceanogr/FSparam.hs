{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
--
-- | Finescale parameterisation after Polzin et al. (2014) doi:10.1002/2013JC008979
--   Assuming pressure = z (depth).
--
module Oceanogr.FSparam (
Segment(..), inSegment,
FSPout(..), 
shearPSD, strainPSD,
ladcpNoise,
fsp,
gnuplotscript,
gridSizeOf
) where

import Oceanogr.CTD
import Oceanogr.LADCP
import Oceanogr.PSD (psd, psdvel)
import Oceanogr.LeastSquare (lsFit3)
import Oceanogr.GSW (gsw_f)
import Oceanogr.GSWtools (gsw_nsquared)

import qualified Data.Vector.Unboxed as V
import Control.Monad (when, mapM_)
import Data.List (sortOn, zip5)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro (sortBy)
import GHC.Float (float2Double)
import Numeric.IEEE (nan)
import System.IO (withFile, Handle, stderr, hPutStrLn)
import Text.Printf (hPrintf, printf)

-- import Debug.Trace

data Segment = Segment {
                sbound :: Float, -- shallow bound [dbar] (<= including)
                dbound :: Float  -- deep bound [dbar]    (< not including)
                }

data FSPout = FSPout {
                getN2mean:: Double, -- ^ mean squared buoyancy frequency [1/s2]
                getN1mean :: Double, -- ^ mean buoyancy frequency [1/s]
                getEhat   :: Double, -- ^ nondimensional gradient spectral level
                getRw     :: Double, -- ^ shear to strain ratio
                getMc     :: Double, -- ^ transition vertical wave number [1/m]
                getEps    :: Double, -- ^ epsilon
                getUD     :: Int     -- ^ =1 for CW>CCW; =-1 for CCW>CW; =0 for undefinable(##)
}

inSegment :: (V.Unbox a) => Segment -> V.Vector Float -> V.Vector a -> V.Vector a
inSegment s z = V.map snd . V.filter (\(z',_) -> sbound s <= z' && z' < dbound s) . V.zip z

--
-- Background buoyancy frequency
-- Smoothed by 2nd order polynomial fit
--
bfreqBG :: Segment -> CTDdata 
        -> V.Vector Double -- ^ neutral density
        -> IO (V.Vector Double, V.Vector Double, V.Vector Double) -- ^ p, n2, n2fit
bfreqBG seg ctd gamman = do
    let pall = ctdP ctd
        p    = inSegment seg pall pall
        i    = inSegment seg pall (V.fromList [0 .. (V.length pall)]) -- [0..] leaks!
        g    = inSegment seg pall gamman

    when (V.null i) $ error "bfeqBG: Segment out of bound"

    -- sort to "stabilise" -- better to do this for entire profile but too slow (nlogn)
    gs' <- V.unsafeThaw (V.zip i g)
    Data.Vector.Algorithms.Intro.sortBy (comparing snd) gs'
    gs  <- V.unsafeFreeze gs'

    let idx  = V.toList . V.map fst $ gs
        ct   = map (float2Double . (ctdCT ctd V.!)) idx
        sa   = map (float2Double . (ctdSA ctd V.!)) idx
        -- do NOT sort pressure
        pp   = V.toList . V.map float2Double $ p
        n    = length idx
        lat' = replicate n (float2Double . stnLatitude . ctdStation $ ctd)

    (n2, pmid) <- gsw_nsquared sa ct pp lat' n -- (-g/rho)(drho/dz)


    --
    -- n2 = p1 x^2 + p2 x + p3 + error
    --
    let x          = map (subtract $ 0.5 * (head pmid + last pmid)) pmid
        x2         = map (^2) x
        (c', _, _) = lsFit3 n2 x2 x (replicate (n-1) 1)
        c          = concat c'
        n2fit      = zipWith (+) (map (* head c) x2) (map (\x0 -> x0 * (c !! 1) + last c) x)

    return (V.fromList pmid, V.fromList n2, V.fromList n2fit)

---
--- grid interval (with 1% fluctuation allowed)
---
gridSizeOf :: (V.Unbox a, RealFloat a, Show a) => V.Vector a -> a
gridSizeOf x
    = let dxs = V.zipWith (-) (V.tail x) x
          dx  = V.head dxs
          fluc = V.map (\x' -> abs (x' - dx) / dx) dxs
       in if V.any (> 0.01 * dx) fluc
            then error "gridSizeOf: uneven grid"
            else dx
---
--- strain
---
strainPSD :: Double          -- ^ mean squared buoyancy frequency
          -> Double          -- ^ vertical grid size
          -> V.Vector Double -- ^ n2 deviation  (= n2 - n2fit)
          -> IO (V.Vector Double,  --  frequency
                 V.Vector Double,  --  strain spectrum
                 (Double, Double), --  95 % conf.interval
                 V.Vector Double)  -- ^ (freq, strain spec, conf int, strain profile)
strainPSD n2m dp n2' = do

    let strain = V.map (/ n2m) n2'

    (freq', pow', c') <- psd 1 2 (V.toList strain)
    -- (freq', pow', c') <- psd 1 1 (V.toList strain)

    let freq = V.map (* (2 * 3.14159265 / dp)) . V.fromList $ freq'
        pow  = V.map (/ (2 * 3.14159265 / dp)) . V.fromList $ pow'
        c   = (head c', last c')

    return (freq, pow, c, strain)

---
--- shear
---
shearPSD :: Segment   -- ^ define depths
         -> LADCPdata -- ^ input
         -> IO (V.Vector Double, --  frequency
                V.Vector Double, --  KE
                V.Vector Double, --  rotary CW
                V.Vector Double, --  rotary CCW
                (Double, Double)) -- ^ (freq, KE, CW, CCW, conf.interval)
shearPSD seg ladcp = do
    let z' = inSegment seg (ladcpZ ladcp) (ladcpZ ladcp)
        u' = inSegment seg (ladcpZ ladcp) (ladcpU ladcp)
        v' = inSegment seg (ladcpZ ladcp) (ladcpV ladcp)
        z  = V.map float2Double z'
        u  = V.toList . V.map float2Double $ u'
        v  = V.toList . V.map float2Double $ v'
        dz = gridSizeOf z

    when (V.null z') $ error "shearPSD: Segment out of bound"

    (f', ke', cw', ccw', cc') <- psdvel 1 2 u v
    -- (f', ke', cw', ccw', cc') <- psdvel 1 1 u v

    let f   = V.map (* (2 * 3.14159265 / dz)) . V.fromList $ f'
        ke  = V.map (/ (2 * 3.14159265 / dz)) . V.fromList $ ke'
        cw  = V.map (/ (2 * 3.14159265 / dz)) . V.fromList $ cw'
        ccw = V.map (/ (2 * 3.14159265 / dz)) . V.fromList $ ccw'
        cc  = (head cc', last cc')
        ts  = toShear f

    return (f, ts ke, ts cw, ts ccw, cc)

--
toShear :: V.Vector Double -- ^ frequency, wavenuber
        -> V.Vector Double -> V.Vector Double
toShear = V.zipWith (\f x -> f^2 * x)

--
-- LADCP noise model (43) in shear space
--
theta2 :: Double
theta2 = (3.2e-2)^2
ladcpNoise :: Segment
           -> LADCPdata
           -> V.Vector Double -- ^ vertical wave number
           -> V.Vector Double
ladcpNoise seg ladcp m
    = let ndat = inSegment seg (ladcpZ ladcp) (ladcpN ladcp)
          -- n    = av . V.map fromIntegral $ ndat
          n    = max 1 (V.minimum . V.map fromIntegral $ ndat) -- conservative
          dzr  = float2Double . ladcpDzr $ ladcp
          z    = inSegment seg (ladcpZ ladcp) (ladcpZ ladcp)
          dz   = float2Double . gridSizeOf $ z
          term1 = V.map sinc . V.map (* (dzr / 2)) $ m -- Polzin's sinc(x) = our sinc(pi x)
          term2 = V.map sinc . V.map (* (dz / 2)) $ m
          noise = V.zipWith (\t1 t2 -> theta2 * t1^6 * t2^2
                                    / (2 * 3.14159265 * n / (2.0 * dz))) term1 term2
       in toShear m noise
--
-- Correct smoothing as suggeted by Polzin et al. (2002)
--
correctLADCPspec :: Double -- ^ dzt
                 -> Double -- ^ dzr
                 -> Double -- ^ d
                 -> Double -- ^ dz
                 -> V.Vector Double -- ^ vertical wave number
                 -> V.Vector Double
correctLADCPspec dzt dzr d' dz m
    = let st = V.map sinc . V.map (* (dzt / 2)) $ m
          sr = V.map sinc . V.map (* (dzr / 2)) $ m
          sz = V.map sinc . V.map (* (dz / 2)) $ m
          sc = V.map sinc . V.map (* (d' / 2)) $ m
          s1 = V.zipWith (*) (V.map (^2) st) (V.map (^2) sr)
          s2 = V.map (^2) sz
          s3 = V.zipWith (*) (V.map (^4) sr) (V.map (^2) sz)
          s4 = V.map (^2) sc
       in V.zipWith4 (\a b c e -> 1.0 / (a * b * c * e)) s1 s2 s3 s4
correctCTDspec :: Double -- ^ dz
               -> V.Vector Double -- ^ vertical wave number
               -> V.Vector Double
correctCTDspec dz
    = V.map (\s' -> 1.0 / s') . V.map (^2) . V.map sinc . V.map (* (dz / 2))

--
-- "a high wave number limit representing a transition into wave-breaking phenomena"
--  evaluated as (20)
--
transition :: (V.Vector Double, V.Vector Double) -- ^ shear (freq, spectra)
            -> Double                            -- ^ (mean) buoyancy freq squared
            -> Either Int Double                 -- ^ Left 1 if not reached;
                                                 -- Left (-1) if too large
transition (f', p') bf2
  = let (f, p) = V.unzip $ V.filter (\(f0,_) -> f0 > 1.0e-9) $ V.zip f' p' -- remove zero frequency
        df     = V.zipWith (-) (V.tail f) f
        -- (*2) in Eq.(20) is cancelled by (*0.5) in the trapezoid area
        area   = V.zipWith (*) df $ V.zipWith (+) (V.init p) (V.tail p) -- trapezoid
        -- area1  = 2 * V.head f * V.head p -- assuming flat power below first frequency
        area1 = 0 -- no power contained whatever longer than the bin
        integrated = V.scanl' (+) area1 area
        threshold  = 2.0 * 3.1419265 * bf2 / 10
        lessthan   = V.filter (<= threshold) integrated
     in if V.null lessthan
          then Left (-1) -- already too big (wavebreaking at larger scales)
          else if V.length lessthan == V.length integrated
                 then Left 1 -- does not break
                 else let i = V.length lessthan - 1
                          r = (threshold - integrated V.! i) 
                               / (integrated V.! (i+1) - integrated V.! i)
                       in Right $ r * (f V.! (i+1) - f V.! i) + f V.! i
                                                        -- linear interpolation

--
-- Shear to strain ratio (29) as evaluated by (average shear) / (average strain)
-- where the average are taken between (m1, m2)
--
shear2strain :: Double                             -- ^ mean buoy freq squared
             -> (V.Vector Double, V.Vector Double) -- ^ shear (freq, spectra)
             -> (V.Vector Double, V.Vector Double) -- ^ strain (freq, spectra)
             -> (Double, Double)                   -- ^ wave number range
             -> Maybe Double
shear2strain n2mean (fh, ph) (ft, pt) (m1, m2)
    = let fph = V.filter (\(f0,_) -> m1 <= f0 && f0 <= m2) $ V.zip fh ph
          fpt = V.filter (\(f0,_) -> m1 <= f0 && f0 <= m2) $ V.zip ft pt
       in if V.null fph || V.null fpt
            then Nothing
            else Just $ (av . V.map snd $ fph ) / (av . V.map snd $ fpt) / n2mean

--
-- Nondimensional gradient spectral level \hat{E} (33) as evaluated by (36)
--
specLevel :: Double -- ^ mean buoyancy freq in [1/s]
            -> (V.Vector Double, V.Vector Double) -- ^ shear (freq, spectra)
            -> (Double, Double)                   -- ^ integration limits (m1, m2)
            -> Double
specLevel bf (f, p) (m1, m2)
    = let pGM = shearGM bf f
          (_, pint, pGMint) = V.unzip3
                               $ V.filter (\(f0,_,_) -> m1 <= f0 && f0 <= m2)
                               $ V.zip3 f p pGM
       in V.sum pint / V.sum pGMint -- no need to multiply `dm'
--
-- Garrett-Munk (1976) shear spectrum (m^2 E^{GM}_k(m)) after (23) and (24)
--
shearGM :: Double          -- ^ mean buoyancy freq in [1/s]
        -> V.Vector Double -- anguar freq
        -> V.Vector Double
shearGM bf
    = let n0 = 2 * 3.14159265 * 3 / (60 * 60) :: Double -- 3 [cph]
          e0 = 3.0e-3                           :: Double -- [m^2 s^{-2}]
          m0 = 4 * 3.14159265 / 1300            :: Double -- [1/m], mode 4
          ms = m0 * (bf / n0)
          c  = (bf / n0) * e0 * 2 / 3.14159265
       in V.map (\m' -> m'^2 * 0.75 * c * ms / (ms^2 + m'^2))

--
-- misc.
--

-- averaging
av :: V.Vector Double -> Double
av x = V.sum x / (fromIntegral . V.length $ x)

-- sinc() as in https://wiki.haskell.org/Sinc_function
epsilon :: RealFloat a => a
epsilon = encodeFloat 1 (fromIntegral $ 1-floatDigits epsilon)
{- Boosted from Boost http://www.boost.org/boost/math/special_functions/sinc.hpp -}
sinc :: (RealFloat a) => a -> a
sinc x =
   if abs x >= taylor_n_bound
     then sin x / x
     else 1 - x^2/6 + x^4/120
 where
  taylor_n_bound = sqrt $ sqrt epsilon

--
-- Finescale parameterisation (40), diagnosis output if Handle exists
--
calcEps :: Double -- ^ latitude 
        -> FSPout -- ^ other parameters
        -> FSPout
calcEps lat (FSPout n2mean n1mean ehat rw mc _ ud)
    = let n0   = 2 * 3.14159265 * 3 / (60 * 60) :: Double -- [3cph]
          f0   = abs $ gsw_f 32.5 :: Double -- coriolis
          f    = abs $ gsw_f lat
          h    = 3 * (rw + 1) / (4 * rw) * sqrt(2 / (rw - 1))
                   * (acosh (n1mean / f) / acosh (n0 / f0))
          --
          -- Ijichi and Hibiya (2015)
          --
          l0   = 2 * acosh (n0 / f0) / 3.14159265
          muGM = 2 * acosh (n1mean / f) / 3.14159265
          l1   = 2 * muGM^2
          l2   = logBase 3 (2 * muGM)
          h'   = if rw <= 9
                   then 3 * (rw + 1) / (4 * rw) * (l1 / l0) * rw ** (-l2)
                   else 3 * (rw + 1) / (4 * rw) * (1 / l0) * sqrt(2 / (rw - 1))
          --
          prod = 8e-10 * (f / f0) * (n2mean / n0^2) * ehat^2 * h'
       in FSPout n2mean n1mean ehat rw mc (prod * 0.83) ud -- 0.83 = 1 - Rf -- (4)(5)

--
-- CW or CCW dominant?
--
isUpDown :: V.Vector Double  -- ^ freq
         -> V.Vector Double -- ^ CW spectrum
         -> V.Vector Double -- ^ CCW spectrum
         -> (Double, Double) -- ^ 95 % conf.interval
         -> (Double, Double) -- ^ integration limits (m1, m2)
         -> Int
isUpDown f cw ccw c (m1, m2)
    = let (_, gcw, gccw) = V.unzip3
                         $ V.filter (\(f0,_,_) -> m1 <= f0 && f0 <= m2)
                         $ V.zip3 f cw ccw
          n              = fromIntegral (V.length gcw) :: Double
          (clow, cup)    = if fst c > snd c -- lint suggested "uncurry (>) c"
                             then (snd c / n, fst c / n)
                             else (fst c / n, snd c / n) -- reduced conf int by addition
          compWithC :: Double -> Double -> Int
          compWithC a b
            | a * clow > b * cup = 1
            | a * cup < b * clow = -1
            | otherwise          = 0
       in compWithC (V.sum gcw) (V.sum gccw)

---
--- Fine scale parameterisation
---
fsp :: CTDdata
        -> V.Vector Double -- ^ neutral density
        -> LADCPdata
        -> Segment
        -> Maybe Double -- ^ upper shear wavenumber
        -> Maybe Handle -- ^ for data output
        -> IO (Maybe FSPout)
fsp ctd gamman ladcp seg nf h' =
 let u = inSegment seg (ladcpZ ladcp) (ladcpU ladcp)
     v = inSegment seg (ladcpZ ladcp) (ladcpV ladcp)
     z = inSegment seg (ladcpZ ladcp) (ladcpZ ladcp)
     e = inSegment seg (ladcpZ ladcp) (ladcpEV ladcp)
     n = inSegment seg (ladcpZ ladcp) (ladcpN ladcp)
     dzLADCP = float2Double $ gridSizeOf z
     lat = float2Double . stnLatitude . ctdStation  $ ctd
     g    = inSegment seg (ctdP ctd) gamman
  in
   if V.any isNaN g || V.any isNaN v
   then hPutStrLn stderr "fsp: gap(s) in data" >> return Nothing
   else do
    -- background stratification
    (p, n2, n2fit) <- bfreqBG seg ctd gamman

    let n2mean = av n2
        n1mean = av (V.map sqrt . V.filter (> 0) $ n2) -- sorting removes most of negatives, but not all
        n2'    = V.zipWith (-) n2 n2fit
        dp     = gridSizeOf p

    -- strain spectrum
    (stf, stp', stc, strain) <- strainPSD n2mean dp n2'
    let stp = V.zipWith (*) stp' (correctCTDspec dp stf)

    -- shear spectrum
    (shf, shke', shcw', shccw', shc) <- shearPSD seg ladcp
    let correctme = V.zipWith (*) (correctLADCPspec (float2Double $ ladcpDzt ladcp)
                                                    (float2Double $ ladcpDzr ladcp)
                                                     9.0 dzLADCP shf)
        shke = correctme shke'
        shcw = correctme shcw'
        shccw = correctme shccw'
    -- noise spectrum
        noise = ladcpNoise seg ladcp shf

    -- output for visual inspection if valid Handle is given (**)
    -- frequency is not angular in output
    when (isJust h') $ 
        let h = fromJust h'
         in do
            mapM_ (\(f',p') -> hPrintf h "%8.4f%12.3e%12.3e%12.3e\n" (f' / (2 * 3.14159265))
                                (n2mean * p') (n2mean * p' * fst stc) (n2mean * p' * snd stc))
                $ sortOn fst $ zip (V.toList stf) (V.toList stp)
            hPrintf h "\n\n"
            mapM_ (\(f',k',c',w',n') -> hPrintf h "%8.4f%12.3e%12.3e%12.3e%12.3e%12.3e%12.3e\n"
                                        (f' / (2 * 3.14159265))
                                        k' (k' * fst shc) (k' * snd shc) c' w' n')
                $ sortOn (\(z0,_,_,_,_) -> z0)
                $ zip5 (V.toList shf) (V.toList shke) (V.toList shcw) (V.toList shccw)
                       (V.toList noise)
            hPrintf h "\n\n"
            V.mapM_ (uncurry (hPrintf h "%8.1f%12.5f\n")) $ V.zip p strain
            hPrintf h "\n\n"
            -- gnuplot complains if all n's are equal
            let n1 = if V.all (== V.head n) n
                       then (V.head n + 1) `V.cons` V.tail n
                       else n
            V.mapM_ (\(z',u',v',e',n') -> hPrintf h "%8.2f%10.3f%10.3f%10.3f%8d\n" z' u' v' e' n')
                $ V.zip5 z u v e n1

    -- transition
    let tr = transition (shf, shke) n2mean
    -- upper frequency free from noise
        uf :: Either Int Double -> Maybe Double -> Maybe Double
        uf (Left _) Nothing  = Nothing
        uf (Right a) Nothing = Just a
        uf (Left 1) (Just b) = Just b
        uf (Left (-1)) (Just _) = Nothing
        uf (Right a) (Just b) = Just $ min a b
        uf _ _ = error "FSparam:uf"
        uu = uf tr nf
---
--- "Good" vertical scale
---
--- (0) Wavenumber resolution of shear
        df = V.minimum (V.head shf `V.cons` V.zipWith (-) (V.tail shf) shf) - 1.0e-6
--- (1) Shear spectra are greater than (2 x LADCP noise level)
        f0 = V.map (\(f',_,_) -> f') . V.filter (\(_,k',n') -> k' >= 2 * n')
                                      $ V.zip3 shf shke noise
--- (2) frequencies lower than upperlimit (transition  ... if exists)
        f1 = case uu of
                Nothing   -> f0
                Just uf' -> V.filter (< uf') f0
--- (3) zero frequency does not contribute
        f2 = V.filter (> df) f1
--- (4) if some good frequencies are left
    if V.null f2
      then return (Just $ FSPout n2mean n1mean nan nan nan nan 0)
      else
        -- let range = (V.head f2 - 1.0e-6, V.last f2 + 1.0e-6)
        let range = (df, V.last f2 + df) -- large scale is noise free
            ud    = isUpDown shf shcw shccw shc range
            rw    = fromMaybe nan $ shear2strain n2mean (shf, shke) (stf, stp) range
            ehat  = specLevel n1mean (shf, shke) range
            mc    = fromMaybe nan uu
         in if rw > 1
              then return $ Just $ calcEps lat (FSPout n2mean n1mean ehat rw mc nan ud)
              else return $ Just $ FSPout n2mean n1mean ehat rw mc nan ud

---
--- Example gnuplot script for plotting the diagnosis above (**)
---
gnuplotscript :: Segment -- ^ depths
               -> String -- ^ figure title
               -> Maybe Double -- ^ transition wavenumber
               -> FilePath -- ^ data filename
               -> [String]
gnuplotscript seg label tr dfile = a ++ b ++ [d c] ++ e
  where
    a = ["set multiplot",
         "set size 0.75, 0.75",
-- spectra
         "set size 0.75, 0.75",
         "set origin 0.25, 0.25",
         "set logscale x",
         "set logscale y",
         "set xrange [0.002:0.5]",
         "set grid",
         "set title \"" ++ label ++ "\"",
         "set key right top"]
    b = case tr of
          Nothing -> []
          Just _  -> ["set parametric",
                      "set trange [1e-7:1e-2]"]
    c = "plot '" ++ dfile ++ "' index 0 using 1:2 title 'Ep' with line linetype 1 linewidth 4,"
         ++ " '' i 0 u 1:3 notitle w l lt 1 lw 1,"
         ++ " '' i 0 u 1:4 notitle w l lt 1 lw 1,"
         ++ " '' i 1 u 1:2 t 'Ek'  w l lt 2 lw 4,"
         ++ " '' i 1 u 1:3 notitle w l lt 2 lw 1,"
         ++ " '' i 1 u 1:4 notitle w l lt 2 lw 1,"
         ++ " '' i 1 u 1:5 t 'CW'  w l lt 3 lw 2,"
         ++ " '' i 1 u 1:6 t 'CCW' w l lt 4 lw 2,"
    d = case tr of
          Nothing -> (++ " '' i 1 u 1:(2*$7) t '2xNoise' w l lt 6 lw 3")
          Just tr'-> (++ " '' i 1 u 1:(2*$7) t '2xNoise' w l lt 6 lw 3," ++ show (tr' / (2 * 3.14159265)) ++ ",t notitle w l lt 6")
--  density
    e = ["set size 0.25, 1",
         "set origin 0, 0",
         "set nologscale x",
         "set nologscale y",
         "set notitle",
         "set xrange [-3:3]",
         printf "set yrange [%.0f:%0.f] reverse" (sbound seg) (dbound seg),
         "plot '" ++ dfile ++ "' index 2 using 2:1 title '' with line",
-- velocities
         "set size 0.45, 0.25",
         "set origin 0.25, 0",
         "set xrange [*:*]",
         printf "set yrange [%.0f:%0.f] reverse" (sbound seg) (dbound seg),
         "plot '" ++ dfile ++ "' index 3 using 2:1 notitle '' with line lt 1,"
         ++ "     ''             i     3 u     3:1 notitle w    l    lt 2,"
         ++ "     ''             i     3 u     4:1 notitle w    l    lt 3 lw 3",
-- data num
         "set size 0.3, 0.25",
         "set origin 0.7, 0",
         printf "set yrange [%.0f:%0.f] reverse" (sbound seg) (dbound seg),
         "set xrange [*:*]",
         "set noytics",
         "plot '" ++ dfile ++ "' index 3 using 5:1 notitle with line lt 3",
         "set nomultiplot",
         "pause -1"]
