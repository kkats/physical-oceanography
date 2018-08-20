--
-- | Finescale parameterisation after Polzin et al. (2014) doi:10.1002/2013JC008979
--   Assuming pressure as z (distance)
--
module Oceanogr.FSparam (
Segment(..), inSegment,
FSPout(..), 
shearPSD, strainPSD,
ladcpNoise,
fsp,
gnuplotscript
) where

import Oceanogr.CTD
import Oceanogr.LADCP
import Oceanogr.PSD (psd, psdvel)
import Oceanogr.LeastSquare (lsFit3)
import Oceanogr.GammaN
import Oceanogr.GSW (linearInterp1, gsw_f)
import Oceanogr.GSWtools (gsw_nsquared)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad (zipWithM_, when, mapM_)
import Data.List (sortBy, zip5)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Ord (comparing)
import Data.Vector.Algorithms.Intro (sortBy)
import GHC.Float (float2Double)
import Numeric.IEEE (nan)
import System.IO (withFile, IOMode(..), Handle)
import Text.Printf (hPrintf, printf)

-- import Debug.Trace

data Segment = Segment {
                sbound :: Float, -- shallow bound [dbar] (<= including)
                dbound :: Float  -- deep bound [dbar]    (< not including)
                }

data FSPout = FSPout {
                n2mean :: Double, -- ^ mean squared buoyancy frequency [1/s2]
                n1mean :: Double, -- ^ mean buoyancy frequency [1/s]
                ehat   :: Double, -- ^ nondimensional gradient spectral level
                rw     :: Double, -- ^ shear to strain ratio
                mc     :: Double, -- ^ transition vertical wave number [1/m]
                eps    :: Double  -- ^ epsilon
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
        ct   = map float2Double . map ((ctdCT ctd) V.!) $ idx
        sa   = map float2Double . map ((ctdSA ctd) V.!) $ idx
        -- do NOT sort pressure
        pp   = V.toList . V.map float2Double $ p
        n    = length idx
        lat' = replicate n (float2Double . stnLatitude . ctdStation $ ctd)

    (n2, pmid) <- gsw_nsquared sa ct pp lat' n

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
gridSizeOf :: (V.Unbox a, RealFloat a) => V.Vector a -> a
gridSizeOf x
    = let dxs = V.zipWith (-) (V.tail x) x
          dx  = V.head dxs
          fluc = V.map (\x' -> abs (x' - dx) / dx) dxs
       in if V.any (> 0.01 * dx) fluc
            then error $ "gridSizeOf: uneven grid"
            else dx
---
--- strain
---
strainPSD :: Segment -- ^ define depths
          -> CTDdata
          -> V.Vector Double -- ^ neutral density
          -> Double          -- ^ mean squared buoyancy frequency
          -> Double          -- ^ vertical grid size
          -> V.Vector Double -- ^ n2 deviation  (= n2 - n2fit)
          -> IO (V.Vector Double, -- ^ frequency
                 V.Vector Double, -- ^ strain spectrum
                 (Double, Double), -- ^ 95 % conf.interval
                 V.Vector Double)  -- ^ strain profile
strainPSD seg ctd gamman n2mean dp n2' = do

    let strain = V.map (/ n2mean) n2'

    (freq', pow', c') <- psd 3 2 (V.toList strain)

    let freq = V.map (/ dp) . V.fromList $ freq'
        pow  = V.map (* dp) . V.fromList $ pow'
        c   = (head c', last c')

    return (freq, pow, c, strain)

---
--- shear
---
shearPSD :: Segment   -- ^ define depths
         -> LADCPdata
         -> IO (V.Vector Double, -- ^ frequenc
                V.Vector Double, -- ^ KE
                V.Vector Double, -- ^ rotary CW
                V.Vector Double, -- ^ rotary CCW
                (Double, Double)) -- ^ 95 % conf.interval
shearPSD seg ladcp = do
    let z' = inSegment seg (ladcpZ ladcp) (ladcpZ ladcp)
        u' = inSegment seg (ladcpZ ladcp) (ladcpU ladcp)
        v' = inSegment seg (ladcpZ ladcp) (ladcpV ladcp)
        z  = V.map float2Double z'
        u  = V.toList . V.map float2Double $ u'
        v  = V.toList . V.map float2Double $ v'
        dz = gridSizeOf z

    when (V.null z') $ error "shearPSD: Segment out of bound"

    -- (f', ke', cw', ccw', cc') <- psdvel 1 2 u v -- better low wavenumber coverage but noisy
    (f', ke', cw', ccw', cc') <- psdvel 2 1 u v

    let f   = V.map (/ dz) . V.fromList $ f'
        ke  = V.map (* dz) . V.fromList $ ke'
        cw  = V.map (* dz) . V.fromList $ cw'
        ccw = V.map (* dz) . V.fromList $ ccw'
        cc  = (head cc', last cc')
        ts  = toShear f

    return (f, ts ke, ts cw, ts ccw, cc)

--
toShear :: V.Vector Double -- ^ frequency, wavenuber
        -> V.Vector Double -> V.Vector Double
toShear = V.zipWith (\f x -> (2.0 * 3.14159265 * f)^2 * x)

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
          noise = V.zipWith (\t1 t2 -> theta2 * t1^6 * t2^2 / (n / (2.0 * dz))) term1 term2
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
correctLADCPspec dzt dzr d dz m
    = let st = V.map sinc . V.map (* (dzt / 2)) $ m
          sr = V.map sinc . V.map (* (dzr / 2)) $ m
          sz = V.map sinc . V.map (* (dz / 2)) $ m
          sd = V.map sinc . V.map (* (d / 2)) $ m
          s1 = V.zipWith (*) (V.map (^2) st) (V.map (^2) sr)
          s2 = V.map (^2) sz
          s3 = V.zipWith (*) (V.map (^4) sr) (V.map (^2) sz)
          s4 = V.map (^2) sd
       in V.zipWith4 (\a b c d -> 1.0 / (a * b * c * d)) s1 s2 s3 s4
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
            -> Double                            -- (mean) buoyancy freq squared
            -> Maybe Double                      -- Nothing if not reached
transition (f, p') bf2
  = let av2 :: Double -> Double -> Double
        av2 a b = (a + b) / 2
        n          = V.length f
        dflast     = V.last f - f V.! (n-2)
        dfhead     = f V.! 1 - V.head f
        fmid       = V.zipWith av2 f (V.tail f)
        df         = dfhead `V.cons` (V.zipWith (-) (V.tail fmid) fmid) `V.snoc` dflast
        p          = V.map (*2) p'
        integrated = V.scanl1' (+) $ V.zipWith (*) df p
        threshold  = 2.0 * 3.1419265 * bf2 / 10
        lessthan   = V.filter (< threshold) integrated
     in if V.length lessthan == n
          then Nothing
          else let i = V.length lessthan - 1
                   r = (threshold - integrated V.! i) 
                        / (integrated V.! (i+1) - integrated V.! i)
                in Just $ r * (f V.! (i+1) - f V.! i) + f V.! i -- linear interpolation

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
        -> V.Vector Double -- freq
        -> V.Vector Double
shearGM bf
    = let n0 = 3.0 / (60 * 60)       :: Double -- 3 [cph]
          e0 = 3.0e-3                :: Double -- [m^2 s^{-2}]
          m0 = 4 * 3.14159265 / 1300 :: Double -- [1/m], mode 4
          ms = m0 * bf / n0
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
calcEps lat (FSPout n2mean n1mean ehat rw mc _)
    = let n0   = 3 / (60 * 60)    :: Double -- 3cph
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
          l2   = log (2 * muGM) / log 3
          h'   = if rw <= 9
                   then 3 * (rw + 1) / (4 * rw) * (l1 / l0) * rw ** (-l2)
                   else 3 * (rw + 1) / (4 * rw) * (1 / l0) * sqrt(2 / (rw - 1))
          --
          prod = 8e-10 * (f / f0) * (n2mean / n0^2) * ehat^2 * h'
       in FSPout n2mean n1mean ehat rw mc (prod * 0.83) -- 0.83 = 1 - Rf -- (4)(5)


fsp :: CTDdata
        -> V.Vector Double -- ^ neutral density
        -> LADCPdata
        -> Segment
        -> Maybe Handle -- ^ for data output
        -> IO FSPout
fsp ctd gamman ladcp seg h' = do
    let u = inSegment seg (ladcpZ ladcp) (ladcpU ladcp)
        v = inSegment seg (ladcpZ ladcp) (ladcpV ladcp)
        z = inSegment seg (ladcpZ ladcp) (ladcpZ ladcp)
        e = inSegment seg (ladcpZ ladcp) (ladcpEV ladcp)
        n = inSegment seg (ladcpZ ladcp) (ladcpN ladcp)
        dzLADCP = float2Double $ gridSizeOf z
        lat = float2Double . stnLatitude . ctdStation  $ ctd

    -- background stratification
    (p, n2, n2fit) <- bfreqBG seg ctd gamman
    let n2mean = av n2
        n1mean = av (V.map sqrt . V.filter (> 0) $ n2) -- sorting removes most of negatives, but not all
        n2'    = V.zipWith (-) n2 n2fit
        dp     = gridSizeOf p

    -- strain spectrum
    (stf, stp', stc, strain) <- strainPSD seg ctd gamman n2mean dp n2'
    let stp = V.zipWith (*) stp' (correctCTDspec dp stf)

    -- shear spectrum
    (shf, shke', shcw', shccw', shc) <- shearPSD seg ladcp
    let correctme = V.zipWith (*) (correctLADCPspec (float2Double $ ladcpDzt ladcp)
                                                    (float2Double $ ladcpDzr ladcp)
                                                     9.0 dzLADCP stf)
        shke = correctme shke'
        shcw = correctme shcw'
        shccw = correctme shccw'
    -- noise spectrum
        noise = ladcpNoise seg ladcp shf
    -- transition
        tr = transition (shf, shke) n2mean

    -- output for visual inspection if valid Handle is given (**)
    when (isJust h') $ 
        let h = fromJust h'
         in do
            -- strain (stp) -> potential energy (n2mean * stp)
            mapM_ (\(f',p') -> hPrintf h "%8.4f%12.3e%12.3e%12.3e\n" f' (n2mean * p')
                                (n2mean * p' * fst stc) (n2mean * p' * snd stc))
                $ Data.List.sortBy (comparing fst) $ zip (V.toList stf) (V.toList stp)
            hPrintf h "\n\n"
            mapM_ (\(f',k',c',w',n') -> hPrintf h "%8.4f%12.3e%12.3e%12.3e%12.3e%12.3e%12.3e\n"
                                        f' k' (k' * fst shc) (k' * snd shc) c' w' n')
                $ Data.List.sortBy (comparing $ \(z,_,_,_,_) -> z)
                $ zip5 (V.toList shf) (V.toList shke) (V.toList shcw) (V.toList shccw)
                       (V.toList noise)
            hPrintf h "\n\n"
            V.mapM_ (\(p', g') -> hPrintf h "%8.1f%12.5f\n" p' g') $ V.zip p strain
            hPrintf h "\n\n"
            V.mapM_ (\(z',u',v',e',n') -> hPrintf h "%8.2f%10.3f%10.3f%10.3f%8d\n" z' u' v' e' n')
                $ V.zip5 z u v e n

---
--- "Good" vertical scale
---
--- (1) Shear spectra are greater than (2 x LADCP noise level)
    let f0 = V.map (\(f',_,_) -> f') . V.filter (\(_,k',n') -> k' >= 2 * n')
                                      $ V.zip3 shf shke noise
--- (2) frequencies lower than mc (trasition  .. if exists)
        f1 = case tr of
                Nothing  -> f0
                Just tr' -> V.filter (< tr') f0
--- (3) zero frequency does not contribute
        f2 = V.filter (> 1.0e-6) f1
--- (4) if some good frequencies are left

    if V.null f2
      then return (FSPout n2mean n1mean nan nan nan nan)
      else do
        -- strain spectrum interpolated onto shf
        stpI <- linearInterp1 stf stp f2
        case stpI of
            Left e   -> error e
            Right pI -> let range = (V.head f2 - 1.0e-6, V.last f2 + 1.0e-6)
                            rw    = fromMaybe nan $ shear2strain n2mean (shf, shke) (f2, pI) range
                            ehat  = specLevel n1mean (shf, shke) range
                            mc    = fromMaybe nan tr
                         in if rw > 1
                              then return $ calcEps lat (FSPout n2mean n1mean ehat rw mc nan)
                              else return (FSPout n2mean n1mean ehat rw mc nan)

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
         "set key right bottom"]
    b = case tr of
          Nothing -> []
          Just _  -> ["set parametric",
                      "set trange [1e-7:1e-2]"]
    c = "plot '" ++ dfile ++ "' index 0 using 1:2 title 'Ep' with line linetype 1 linewidth 4,"
         ++ " '' i 0 u 1:3 notitle w l lt 1 lw 1,"
         ++ " '' i 0 u 1:4 notitle w l lt 1 lw 1,"
         ++ " '' i 1 u 1:2 t 'Ek'  w l lt 3 lw 4,"
         ++ " '' i 1 u 1:3 notitle w l lt 3 lw 1,"
         ++ " '' i 1 u 1:4 notitle w l lt 3 lw 1,"
         ++ " '' i 1 u 1:5 t 'CW'  w l lt 5 lw 2,"
         ++ " '' i 1 u 1:6 t 'CCW' w l lt 4 lw 2,"
    d = case tr of
          Nothing -> (++ " '' i 1 u 1:(2*$7) t '2xNoise' w l lt 2 lw 3")
          Just tr'-> (++ " '' i 1 u 1:(2*$7) t '2xNoise' w l lt 2 lw 3," ++ show tr' ++ ",t notitle w l lt 6")
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
         "plot '" ++ dfile ++ "' index 3 using 2:1 notitle '' with line lt 5,"
         ++ "     ''             i     3 u     3:1 notitle w    l    lt 4,"
         ++ "     ''             i     3 u     4:1 notitle w    l    lt 1 lw 3",
-- data num
         "set size 0.3, 0.25",
         "set origin 0.7, 0",
         printf "set yrange [%.0f:%0.f] reverse" (sbound seg) (dbound seg),
         "set xrange [0:100]",
         "set noytics",
         "plot '" ++ dfile ++ "' index 3 using 5:1 notitle with line lt 3",
         "set nomultiplot",
         "pause -1"]
