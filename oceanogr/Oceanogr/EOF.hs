--
-- | EOF solver
--
--  Empirical Orthogonal Functions -- purely sequential (despite use of Repa!)
--
module Oceanogr.EOF (
        doEOF, doEOFeigOnly)
where
import Data.Array.Repa as Repa hiding ((++))

import Numeric.Statistics.PCA (pca, pcaTransform)

import qualified Data.Array.IArray           as A
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Numeric.LinearAlgebra.Data  as V

import Control.Monad                    (forM_) -- , forM)
import Data.Packed.Repa                 (repaToVector, matrixToRepa)
import GHC.Float                        (float2Double, double2Float)
import Numeric.IEEE                     (nan)
-- import Numeric.GSL.Statistics           (variance)

-- import Text.Printf                      (printf)
-- import Debug.Trace (trace)


oceanMask :: Array U DIM2 Float -- ^ matrix (space idx, time idx) masked by NaN's
         ->  (Int, U.Vector Int) -- ^ (number of ocean grids, ocean indices)
oceanMask input
    = let lm' = foldS (+) 0 $ transpose input -- land mask
          lm  = computeUnboxedS $ Repa.map (\x -> if isNaN x then 0 else 1 :: Int) lm'
          nocean = sumAllS lm
          lmvec = U.indexed $ Repa.toUnboxed lm
          oivec = fst $ U.unzip $ U.filter (\(_,n) -> n == 1) lmvec
       in (nocean, oivec)

doEOF :: Array U DIM2 Float -- ^ matrix (space idx, time idx) masked by NaN's
        -> Int              -- ^ # modes to return
        -> IO (Array U DIM2 Float,            --  EOF (mode idx, space idx)
               A.Array Int (V.Vector Double), --  time series (mode idx (1 .. n), ts vector)
               [Double])                      --  modal contributions, NOT eigenvalues
doEOF input nmodes = do

    let (Z:._:.ns) = extent input
        (nocean, oivec) = oceanMask input
        arr  = A.array (1, nocean)
                [(n, repaToVector $ computeS
                                  $ Repa.map float2Double
                                  $ slice input (Z :. All :. oivec U.! (n-1)))
                                        | n <- [1 .. nocean]]
        -- total variances
        -- tvar = sum $ A.elems $ fmap variance arr

        -- let's go
        -- (v, c') = pcaN arr nmodes -- error in total variance
        (v', c') = pca arr 0
        s' = pcaTransform arr c'
        c  = matrixToRepa c'
        v  = V.toList v'

    -- output
    buf <- M.replicate (ns * nmodes) nan :: IO (M.IOVector Float)

    -- co <- forM [1 .. nmodes] (\mode -> do
    forM_ [1 .. nmodes] (\mode -> do

        let offset = (mode - 1) * ns
            sp = computeUnboxedS $ Repa.slice c (Z :. All :. (mode-1))

            -- variance
            -- vvar = sumAllS $ sp *^ sp -- vvar == sum v
            -- var = variance $ s' A.! mode  -- index of s' from 1


        -- spatial pattern
        U.mapM_ (\i -> M.write buf (offset + oivec U.! i)
                                   (double2Float $ sp `unsafeIndex` ix1 i))
                                                    $ U.fromList [0 .. (nocean-1)]

        -- return $ trace(printf "mode %d: %12.3f x %12.3f / %12.3f [%12.3f] = %.4f"
        --                 mode var vvar tvar (sum v) (var * vvar / tvar))
        --             (var * vvar / tvar)
        )
    buf' <- U.unsafeFreeze buf

    let buf'' = fromUnboxed (ix2 nmodes ns) buf'
        s     = A.array (1, nmodes) $ take nmodes (A.assocs s')

    -- the following two gives the same result
    -- return (buf'', s, co)
    return (buf'', s, take nmodes $ Prelude.map (/ sum v) v)

doEOFeigOnly :: Array U DIM2 Float -- ^ matrix (space idx, time idx) masked by NaN's
             -> [Double]           -- ^ eigenvalues NOT modal contribution
doEOFeigOnly input
  = let (nocean, oivec) = oceanMask input
        arr  = A.array (1, nocean)
                [(n, repaToVector $ computeS
                                  $ Repa.map float2Double
                                  $ slice input (Z :. All :. oivec U.! (n-1)))
                                        | n <- [1 .. nocean]]
        (v, _) = pca arr 0
     in V.toList v

{-
    let a = A.array (1,3) [(1,a1), (2,a2), (3,a3)]
        a1 = V.fromList [2,3,6,7] :: V.Vector Double
        a2 = V.fromList [1,3,2,5] :: V.Vector Double
        a3 = V.fromList [4,8,9,1] :: V.Vector Double
        m  = pca a (1.0e-9)
        n  = pcaTransform a m
    print m
    print n
-}
-- >> a = [2 3 6 7; 1 3 2 5; 4 8 9 1];
-- >> [c, s] = princomp(a')
-- c = 0.2770   -0.8436    0.4601
--     0.2970   -0.3802   -0.8759
--    -0.9138   -0.3793   -0.1452
-- s = 0.1584    3.3432    0.6006
--    -2.6258    0.2221   -1.2722
--    -3.0056   -2.3077    0.8387
--     5.4729   -1.2575   -0.1671
-- cf.
-- m = c
-- n = [(1, s(:,1)), (2, s(:,2)), (3, s(:,3))]
