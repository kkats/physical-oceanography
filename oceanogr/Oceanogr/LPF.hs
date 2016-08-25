--
-- | Low pass filters
--
module Oceanogr.LPF (lpfFIR, lpfIIR) where

import DSP.Filter.FIR.FIR    (fir)
-- import DSP.Filter.IIR.IIR    (iir_df1)
import DSP.Filter.IIR.Design (butterworthLowpass)
import DSP.Window            (hamming)
import Numeric.IEEE          (nan)
import Numeric.LinearAlgebra (ident, linearSolve)

import qualified Data.Array                 as A
import qualified Numeric.LinearAlgebra.Data as V
import qualified Numeric.LinearAlgebra.Data as M

-- import Debug.Trace (trace)
--
-- | FIR: Hamming
--
lpfFIR :: Int -- ^ window size
       -> [Double] -- ^ input
       -> [Double]

lpfFIR wsize input = let wsize' = if wsize `mod` 2 == 0 then wsize + 1
                                                        else wsize
                         l2     = (wsize' - 1) `div` 2
                         window'= hamming wsize'
                         window = fmap (/ (sum $ A.elems window')) window'
                         output = fir window input
                      in replicate l2 nan ++ drop wsize' output ++ replicate (l2+1) nan
---
--- | IIR: Butterworth
---
lpfIIR :: Double -- ^ cutoff period
       -> Double -- ^ sampling period
       -> Double -- ^ filter sharpness S
                 --  at frequency (2pi/cutoff - 2pi/sampling)
                 --  pass (1.0 - S)
                 -- at frequency (2pi/cutoff + 2pi/sampling)
                 -- pass S
       -> [Double] -- ^ input
       -> (Int, [Double]) -- ^ (filter order, output)

lpfIIR cutoff sampling s x
    = let df = 1.0 / (sampling * fromIntegral (length x - 1))
          wp = (1.0 / cutoff - 5 * df) / (1.0 / cutoff)
          ws = (1.0 / cutoff + 5 * df) / (1.0 / cutoff)
          f  = butterworthLowpass (wp, s) (ws, s)
          bn = A.bounds $ fst f
          an = A.bounds $ snd f
          nfilt = 1 + max (snd bn - fst bn) (snd an - fst an)
          nfact = 3 * (nfilt - 1) -- length of edge transients

          -- extrapolated input (LL.86-90)
          beg = map (\w -> 2 * head x - w) $ reverse
                                            $ take nfact (tail x)
          end = map (\w -> 2 * last x - w) $ take nfact (tail $ reverse x)
          x'  = beg ++ x ++ end

          -- initial condition
          zi = map (* head x') $ concat $ M.toLists $ lpfInit f

          -- let's go
          o1  = iir_df2t f zi x'
          o2  = iir_df2t f zi $ reverse o1
          o2' = reverse $ drop nfact $ take (length o2 - nfact) o2

       in if nfact > length x then error "input too short"
                              else (nfilt, o2')

---
--- | local imprementation of Transposed Direct Form II after DSP.Filter.IIR.IIR.iir_df2t
--- with initial conditions
--- to go with matlab
---
iir_df2t :: (A.Array Int Double, A.Array Int Double) -- ^ (b,a)
        -> [Double] -- ^ initial condition
        -> [Double] -- ^ x[n]
        -> [Double] -- ^ y[n]

iir_df2t (b,a) z = iir'df2t (b,a) z'
    where z' = A.listArray (0, m-1) $ z ++ repeat 0
          m  = max (snd $ A.bounds b) (snd $ A.bounds a)

iir'df2t :: (A.Array Int Double, A.Array Int Double) -> A.Array Int Double
            -> [Double] -> [Double]
iir'df2t _     _ []     = []
iir'df2t (b,a) z (x:xs) = y : iir'df2t (b,a) z' xs
    where y  = b A.! 0 * x + z A.! 0 
          z0 = [b A.! i * x - a A.! i * y + z A.! i | i <- [1 .. m]]
          zn =  b A.! (m+1) * x - a A.! (m+1) * y
          z' = A.listArray (0, m) $ z0 ++ [zn]
          m  = snd $ A.bounds z -- = sizeof (a,b) -1

--
-- | Initialisation for filtfilt after
-- $(matlab)/toolbox/signal/signal/filtfilt.m
--
lpfInit :: (A.Array Int Double, A.Array Int Double) -- (b, a)
            -> M.Matrix Double
lpfInit (b, a)
    = let bn = A.bounds b
          an = A.bounds a 
          n  = 1 + max (snd bn - fst bn) (snd an - fst an)
          -- zero-padding (LL.72-73)
          av = V.fromList $ A.elems a ++ replicate (n - length (A.elems a)) 0
          bv = V.fromList $ A.elems b ++ replicate (n - length (A.elems b)) 0
          -- matrices (LL.77-84)
          z1 = (n-1) M.>< (n-1) $
               map negate (V.toList $ V.subVector 1 (n-1) av)
                 ++ concatMap (oneAt (n-1)) [1 .. (n-2)]
          z2 = ident (n-1) - M.tr z1
          z3 = M.cmap (* (bv `M.atIndex` 0)) (V.subVector 1 (n-1) av)
          z4 = M.asColumn $ V.subVector 1 (n-1) bv - z3
       in case linearSolve z2 z4 of
            Nothing -> error "lpfInit: lineaSolve failed"
            Just a  -> a

oneAt :: Int -> Int -> [Double] -- [0,0,1,0,..0] only n-th element is one
oneAt len m = replicate (m-1) 0 ++ [1] ++ replicate (len-m) 0


---
---
---
{-
testLPF :: IO (Int, [Double])
testLPF = do
    c <- readFile "tmp/a.dat" --- random numbers
    let input = map (\r -> read r:: Double) $ lines  c
    return $ lpfIIR 10 1 0.05 input
           -- in lpfFIR 5 input :: [Double]

testInit :: IO ()
testInit = do
--    let b = [5.206707589240917e-3,2.6033537946204582e-2,5.2067075892409165e-2, 
--             5.2067075892409165e-2,2.6033537946204582e-2,5.206707589240917e-3]
--        a = [-2.178370485801986,2.3146916969151534,-1.3222614573828673,
--             0.40415732404427346,-5.160243491886493e-2];
    let b = [7.553518321763703e-4,5.287462825234591e-3,1.5862388475703777e-2,
             2.6437314126172957e-2,2.6437314126172957e-2,1.5862388475703777e-2,
             5.287462825234591e-3,7.553518321763703e-4]
        a = [1.0,-2.9568612080801286,4.3640930721537226,-3.859994541378411,
             2.177302627188157,-0.7713917446608339,0.15782726744400852,
             -1.4290438147942756e-2]

        b' = A.listArray (1, length b) b
        a' = A.listArray (1, length a) a

    print $ lpfInit (b', a')
-}
