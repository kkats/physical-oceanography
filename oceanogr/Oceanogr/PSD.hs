-- |
-- Power Spectrum Density using FFT
--
--
module Oceanogr.PSD (psd) where
import Oceanogr.LeastSquare           (lsFit2dangerous)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Control.Monad         (when, zipWithM_, forM)
import Data.Array            (Array, elems)
import Data.Array.CArray     (createCArray, toForeignPtr)
import Foreign.Storable      (pokeElemOff)
import System.IO
-- for test
-- import System.Random.MWC     (withSystemRandom, asGenIO)
-- import System.Random.MWC.Distributions (standard)
-- import Text.Printf
-- import Debug.Trace (trace)

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
psd :: Int                     -- ^ number of sections
    -> Int                     -- ^ number of freq binning
    -> [Double]                -- ^ input
    -> IO ([Double], [Double], [Double]) -- ^ frequency, power, 95% confidence interval
psd ns nb x = do
    let n  = length x
        k' = n `quot` ns -- length of 1 section
        (k, nz)  = if n `rem` ns == 0  -- number of zero-padding
                     then (k',  0)
                     else (k'+1, ns - n `rem` ns)
        k2 = if even k then k `div` 2 else (k+1) `div` 2
        xv = VU.fromList x

    when (nz /= 0) $ hPutStrLn stderr ("PSD.psd: zero padding " ++ show nz ++ " data")

    ans <- forM [1 .. ns] (\i -> do
                let xin = if i < ns then VU.toList $ VU.slice ((i-1)*k) k xv
                                    else let from = (i - 1) * k
                                             to   = n - 1
                                             size = to - from + 1
                                          in if (k-size) /= nz -- just checking
                                               then error "?"
                                               else VU.toList (VU.slice from size xv)
                                                 ++ replicate nz 0 -- zero padding
                psdWindow $ detrend xin
                )

    -- section averaging
    -- try to be strict with monad
    pow' <- forM [0 .. (k2-1)] (\i -> do
                    s <- forM [1 .. ns] (\j -> (ans !! (j-1)) `VU.indexM` i)
                    return $ sum s
                    )

    let f   = map (\i -> fromIntegral i / fromIntegral k) [0 .. (k2-1)]
        pow = map (/ fromIntegral ns) pow'

    -- frequency binning (P.552, NR)
    let (ff, pp) = unzip $ binAverage nb f pow

        -- Minobe (3.15) http://www.sci.hokudai.ac.jp/~minobe/data_anal/chap3.pdf
        -- for zero-padding
        dof :: Double
        dof = 2 * fromIntegral nb
                * (fromIntegral ns - 1 + fromIntegral k / fromIntegral (k + nz))
                * dofWindow

        conf = map ((\c' -> dof / c') . quantileChiSquared dof) [0.975, 0.025]

    return (ff, pp, conf)


-- | use least squares fit to detrend
detrend :: [Double] -> [Double]
detrend x = let n  = length x
                n2 = n `div` 2
                a1, a2 :: [Double]
                a1 = map fromIntegral $ take n [negate n2, 1-negate n2 ..]
                a2 = replicate n 1
                (b', _, _) = lsFit2dangerous x a1 a2 -- last argument is dummy
                b  = concat b'
             in zipWith3 (\x' a1' a2' -> x' - a1' * (b !! 0) - a2' * (b !! 1)) x a1 a2


binAverage :: Int -> [Double] -> [Double] -> [(Double, Double)]
binAverage _ [] _ = []
binAverage _ _ [] = []
binAverage n f p = let (fa, fb) = splitAt n f
                       (pa, pb) = splitAt n p
                    in (sum fa / fromIntegral (length fa), sum pa):binAverage n fb pb
                            -- "summed (not averaged)" (P.552)

--
-- | Windowing before FFT
-- see sec 13.4 (pp.554--) of NR for background (including why (1/10)-cosine is not used)
--
psdWindow :: [Double] -> IO (VU.Vector Double)
psdWindow x = do
    let n  = length x
        h  = window (n-1)
        he = elems h
        dim = sum $ map (**2) he
        y  = zipWith (*) he x

    pow' <- psdRaw y

    return $ VU.map (* (fromIntegral n / dim)) pow'  -- (13.4.11)

--
-- | use FFT to calculate psd -- no frills
--
psdRaw :: [Double] -> IO (VU.Vector Double)
psdRaw x = do
    let n = length x

    a <- createCArray (0, n-1) $ \ptr -> zipWithM_ (pokeElemOff ptr) [0..(n-1)] x

    let (n', ptr) = toForeignPtr $ dftRH a
        b = VS.unsafeFromForeignPtr0 ptr n'

    -- when (n /= n') $ error "inconsistency in length"

    let n2  = if even n then n `div` 2 else (n+1) `div` 2
        nsq = fromIntegral (n * n) :: Double
        pow'= map (\i -> if i == 0
                            then (b VS.! 0)**2 / nsq
                            else ((b VS.! i)**2 + (b VS.! (n-i))**2) / nsq) [0 .. (n2-1)]
        pow = if even n then pow' ++ [(b VS.! n2)**2 / nsq]
                        else pow'

    return $ VU.fromList pow


{-
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
