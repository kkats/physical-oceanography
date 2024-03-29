{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- |
-- Power Spectrum Density using FFT
--
--
module Oceanogr.PSD (psd, psdvel, psd', psdvel') where
import Oceanogr.LeastSquares

import qualified Data.Vector.Unboxed as V
import Control.Applicative   (liftA3)
import Control.Monad         (when, forM)
import Data.Array.CArray     (createCArray, Array, elems)
import Data.Complex          (realPart, imagPart, magnitude)
import Foreign.Storable      (pokeElemOff)
import System.IO

import DSP.Window            (bartlett) -- no Welch?
import Math.FFT

-- The package's chiSquared can only cope with integral DoF
-- import Statistics.Distribution            (quantile)
-- import Statistics.Distribution.ChiSquared (chiSquared)
import Numeric.SpecFunctions (invIncompleteGamma)

-- After Statistics.Distribution.ChiSquared.quantile
quantileChiSquared :: Double -> Double -> Double
quantileChiSquared ndof p
    | p == 0         = 0
    | p == 1         = 1/0
    | p > 0 && p < 1 = 2 * invIncompleteGamma (ndof / 2) p
    | otherwise      = error $ "p must be in [0,1] range: p=" ++ show p
    
--
-- | Use Bartlett (P.554, NR).
-- This will increase eDOF by 50% (see http://researchmap.jp/jomv665bq-44320/#_44320)
--
window :: Int -> Array Int Double
window = bartlett

dofWindow :: Double
dofWindow = 1.5

--
-- | psd with sectioning
--
--   Real input only.
--   Input time series has unit of [a].
--   Output frequency is frequency in [1/s], *not* angular frequency 2\pi f.
--   Output power spectrum density is in [a^2 s].
--   When input is `differentiated', multiply by (2\pi f), not by f.
--   All calculated, assuming input time interval dt=1.
--   If not, frequency be multiplied by (1/dt) and power by dt.
--

--
-- Confidence interval by sectioning and frequency binning
-- Zero padding after Minobe (3.15) http://www.sci.hokudai.ac.jp/~minobe/data_anal/chap3.pdf
--
ci :: Double -- ^ confidence interval
    -> Int -- ^ number of sections
    -> Int -- ^ number of frequency binning
    -> Int -- ^ length of one section
    -> Int -- ^ number of zero padding
    -> (Double, Double)
ci confi nb ns len nz =
    let dof :: Double
        dof = 2 * fromIntegral nb
                * (fromIntegral ns - 1 + fromIntegral len / fromIntegral (len + nz))
                * dofWindow
        dn  = (1.0 - confi) / 2
        up  = 1.0 - (1.0 - confi) / 2
        cc  = map ((\c' -> dof / c') . quantileChiSquared dof) [up, dn]
     in (cc !! 0, cc !! 1)

slicer :: Int -- ^ number of sections
       -> Int -- ^ required section (1 (one ... not zero!) for the first section)
       -> V.Vector Double -- ^ input
       -> (V.Vector Double, Int) -- ^ output and number of zero padding
slicer ns i x = let n = V.length x
                    k' = n `quot` ns -- length of one section
                    (k, nz) = if n `rem` ns == 0 -- number of zero padding
                                then (k', 0)
                                else (k'+1, ns - n `rem` ns)
                 in if i < ns then (V.slice ((i-1)*k) k x, nz)
                              else let from = (i - 1) * k
                                       to   = n - 1
                                       zlen = to - from + 1
                                    in (V.slice from zlen x V.++ V.replicate nz 0, nz)
psd = psd' 0.95
psd' :: Double                 -- ^ confidence interval
    -> Int                     -- ^ number of sections
    -> Int                     -- ^ number of freq binning
    -> V.Vector Double         -- ^ input
    -> IO (V.Vector Double, V.Vector Double, (Double, Double)) -- ^ frequency, power, 95% confidence interval
psd' confi ns nb x = do
    let (_, nz) = slicer ns 1 x
        k  = (V.length x - nz) `div` ns
        k2 = if even k then k `div` 2 + 1 else (k+1) `div` 2 -- see (***)

    when (nz /= 0) $ hPutStrLn stderr ("Oceanogr.PSD.psd: zero padding " ++ show nz)

    pow'' <- forM [1 .. ns] $ \i -> let (xin, _) = slicer ns i x
                                     in psdWindow (detrend xin)

    -- section averaging
    -- try to be strict with monad
    pow' <- forM [0 .. (k2-1)] $ \i 
                -> sum `fmap` forM [1 .. ns] (\j -> (pow'' !! (j-1)) `V.indexM` i)

    let f   = map (\i -> fromIntegral i / fromIntegral k) [0 .. (k2-1)]
        pow = map (/ fromIntegral ns) pow'

    -- frequency binning (P.552, NR)
        (ff, pp) = unzip $ binAverage nb f pow
        cc       = ci confi nb ns ((V.length x - nz) `div` ns) nz

    -- power diagnostics
    --when (length ff < 2) $ error "PSD.psd: time series too short"
    --let df = ff !! 1 - head ff
    --    outPower = df * sum pp
    --    inPower = (sum . map (^2) $ x') / fromIntegral (length x')
    --printf "in = %g, out = %g\n" inPower outPower
    return (V.fromList ff, V.fromList pp, cc)

psdvel = psdvel' 0.95
psdvel' :: Double                                               -- ^ confidence interval
       -> Int                                                   -- ^ number of sections
       -> Int                                                   -- ^ number of freq binning
       -> V.Vector Double -> V.Vector Double                    -- ^ input
       -> IO (V.Vector Double, V.Vector Double, V.Vector Double, V.Vector Double, (Double, Double))
                -- ^ frequcney, power of ke, p.of cw, p.of ccw, 95% confidence interval
psdvel' confi ns nb u v = do
    let (_, nz) = slicer ns 1 u
        k  = (V.length u - nz) `div` ns
        k2 = if even k then k `div` 2 + 1 else (k+1) `div` 2 -- see (***)

    when (nz /= 0) $ hPutStrLn stderr ("PSD.psdvel: zero padding " ++ show nz)

    pow'' <- forM [1 .. ns] $ \i -> let (uin, _) = slicer ns i u
                                        (vin, _) = slicer ns i v
                                     in psdWindowVel (detrend uin) (detrend vin)

    (ke'', cw'', ccw'') <- unzip3 `fmap` forM [0 .. (k2-1)] (\i -> do
                        (ke2, cw2, ccw2) <- unzip3 `fmap` forM [1 .. ns] (\j ->
                                                let (ke0, cw0, ccw0) = pow'' !! (j - 1)
                                                 in liftA3 (,,) (ke0 `V.indexM` i)
                                                                (cw0 `V.indexM` i)
                                                                (ccw0  `V.indexM` i))
                        return (sum ke2, sum cw2, sum ccw2))

    let f = map (\i -> fromIntegral i / fromIntegral k) [0 .. (k2-1)]
        ke' = map (/ fromIntegral ns) ke''
        cw' = map (/ fromIntegral ns) cw''
        ccw' = map (/ fromIntegral ns) ccw''

        (ff, ke) = unzip $ binAverage nb f ke'
        cw  = map snd $ binAverage nb f cw'
        ccw = map snd $ binAverage nb f ccw'

        cc       = ci confi nb ns ((V.length u - nz) `div` ns) nz

    return (V.fromList ff, V.fromList ke, V.fromList cw, V.fromList ccw, cc)

-- | use least squares fit to detrend
detrend :: V.Vector Double -> V.Vector Double
detrend x = let n  = V.length x
                n2 = n `div` 2
                a1 = V.fromList $ map fromIntegral $ take n [negate n2, 1-negate n2 ..]

                ((b0, b1), _, _) = lsFit2 x a1 Nothing
             in V.zipWith (\x0 a1' -> x0 - a1' * b0 -  b1) x a1

binAverage :: Int -> [Double] -> [Double] -> [(Double, Double)]
binAverage _ [] _ = []
binAverage _ _ [] = []
binAverage n f p = let (fa, fb) = splitAt n f
                       (pa, pb) = splitAt n p
                    in (sum fa / fromIntegral (length fa),
                        sum pa / fromIntegral (length pa)):binAverage n fb pb
                            -- '"summed (not averaged)" (P.552)' will not conserve power

--
-- | Windowing before FFT
-- see sec 13.4 (pp.554--) of NR for background (including why (1/10)-cosine is not used)
--
psdWindow :: V.Vector Double -> IO (V.Vector Double)
psdWindow x =
    let n   = V.length x
        h   = window (n-1)
        -- h  = listArray (0,n-1) $ repeat 1 :: Array Int Double -- no window
        he  = V.fromList $ elems h
        dim = V.sum $ V.map (^2) he
        y   = V.zipWith (*) he x
        nm  = fromIntegral n / dim -- normalise by (13.4.11)
     in V.map (* nm) `fmap` psdRaw y

psdWindowVel :: V.Vector Double -> V.Vector Double
            -> IO (V.Vector Double, V.Vector Double, V.Vector Double)
psdWindowVel u v =
    let n  = V.length u
        h  = window (n-1)
        -- h  = listArray (0,n-1) $ repeat 1 :: Array Int Double  -- no window
        he = V.fromList $ elems h
        dim = V.sum $ V.map (^2) he
        yu  = V.zipWith (*) he u
        yv  = V.zipWith (*) he v
        nm  = fromIntegral n / dim -- normalise by (13.4.11)
     in psdRawVel yu yv >>= \(ke', cw', ccw') -> let ke  = V.map (* nm) ke'
                                                     cw  = V.map (* nm) cw'
                                                     ccw = V.map (* nm) ccw'
                                                  in return (ke, cw, ccw)
                
--
-- | use FFT to calculate psd -- no frills
--
psdRaw :: V.Vector Double -> IO (V.Vector Double)
psdRaw x = do
    let n = V.length x

    a <- createCArray (0, n-1) $ \ptr -> V.zipWithM_ (pokeElemOff ptr)
                                                     (V.fromList [0 .. (n-1)]) x
    let fa   = elems $ dftRC a
        pow' = unfolddftRC n . map (^2) . map magnitude $ fa
        pow  = map (* (1 / fromIntegral n)) pow'
                                 -- normalise by  (1 * duration / n^2)
                                 -- https://researchmap.jp/jovy4q1ti-44320/#_44320
    return $ V.fromList pow

unfolddftRC :: Int       -- ^ length of input to dftRC
            -> [Double]  -- ^ power (*not* original coefficients)
            -> [Double]
unfolddftRC n x
    = let (_, nb) = if even n then (n `div` 2 + 1, n `div` 2 - 1) -- (***)
                              else ((n+1) `div` 2, (n-1) `div` 2)
          doubler :: Int -> Double -> Double
          doubler m x'
            | 1 <= m && m <= nb = 2.0 * x'
            | otherwise         = x'
       in zipWith doubler [0 .. (n-1)] x

psdRawVel :: V.Vector Double -> V.Vector Double                   -- ^ u, v
        -> IO (V.Vector Double, V.Vector Double, V.Vector Double) -- ^ ke, cw, ccw
psdRawVel u v = do
    let n = V.length u
    a <- createCArray (0, n-1) $ \ptr -> V.zipWithM_ (pokeElemOff ptr)
                                            (V.fromList [0 .. (n-1)]) u
    b <- createCArray (0, n-1) $ \ptr -> V.zipWithM_ (pokeElemOff ptr)
                                            (V.fromList [0 .. (n-1)]) v

    let fa = elems $ dftRC a
        fb = elems $ dftRC b
        ar = map realPart fa
        ai = map imagPart fa
        br = map realPart fb
        bi = map imagPart fb

        ke'  = zipWith (+) (zipWith (+) (map (^2) ar) (map (^2) ai))
                           (zipWith (+) (map (^2) br) (map (^2) bi))
        cw'  = zipWith (+) (map (^2) $ zipWith (+) ar bi)
                           (map (^2) $ zipWith (-) br ai) -- / 2 is in normalisation below
        ccw' = zipWith (+) (map (^2) $ zipWith (-) ar bi)
                           (map (^2) $ zipWith (+) br ai)
        ke   = map (* (1 / fromIntegral n)) (unfolddftRC n ke') -- normalise (see psdRaw)
        cw   = map (* (0.5 / fromIntegral n)) (unfolddftRC n cw')
        ccw  = map (* (0.5 / fromIntegral n)) (unfolddftRC n ccw')

    return (V.fromList ke, V.fromList cw, V.fromList ccw)

{-
import System.Random.MWC     (withSystemRandom, asGenIO)
import System.Random.MWC.Distributions (standard)
import Debug.Trace (trace)
--
-- TEST
--
len :: Int
len = 5000

dt :: Double
dt = 1 / 1000

nsec, nbin :: Int
nsec = 13
nbin = 7


testFFT :: IO()
testFFT = do

    e <- withSystemRandom . asGenIO $ \gen -> VU.replicateM len (standard gen)
                                                            :: IO (VU.Vector Double)

    let t = map (\i -> (fromIntegral i :: Double) * dt) [0 .. (len-1)]
        x = map (\t' -> sin(2*pi*50*t') + sin(2*pi*120*t')) t
        y = zipWith (\x' e' -> x' + 2 * e') x (VU.toList e)

    h <- openFile "test.in" WriteMode
    zipWithM_ (\t' y' -> hPrintf h "%15.6f%15.6f\n" t' y') t y
    hClose h

    -- p <- VU.toList `fmap` psdRaw y
    -- let f = map (\i -> fromIntegral i / (dt * fromIntegral len)) [0 .. (len-1)]
    (f, p, c) <- psd nsec nbin y

    g <- openFile "test.out" WriteMode
    hPutStrLn g $ "#" ++ show c
    zipWithM_ (\f' p' -> hPrintf g "%15.6f%15.6f\n" (f'/dt)  p') f p
    hClose g
-}
