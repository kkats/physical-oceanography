{-# LANGUAGE BangPatterns, TypeOperators #-}
--
-- | When filling an array with a shape (`Any` :. k), most of calculations
--   for the last dimension (k) are common such that the matrix is filled in
--   parallel upto the dimension "Any" and serially for "k"
--
--   Monomorphic : `D` -> `U`
--
--   To produce (Any :. k) loadP1 loops over "Any" only
--
--                                                      from Data.Array.Repa.Eval

module Oceanogr.RepaExpand1 (computeP1) where

import Data.Array.Repa
import Data.Array.Repa.Eval (fillChunkedP, Target (..), now)

import qualified Data.Vector.Unboxed as U

import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (traceEventIO)

computeP1 :: (Shape sh, Monad m, U.Unbox e)
            => (sh :. Int)
            -> ((sh :. Int) -> Int -> U.Vector e) -- fn to process 1 chunk
            -> m (Array U (sh :. Int) e)
computeP1 sh getVec
    = now $ unsafePerformIO $ do mvec <- newMVec (size $ sh)
                                 loadP1 sh getVec mvec
                                 unsafeFreezeMVec sh mvec

--                                                      from Data.Array.Repa.Repr.Delay
loadP1 :: (Shape sh, U.Unbox e)
        => (sh :. Int)
        -> ((sh :. Int) -> Int -> U.Vector e)
        -> MVec U e
        -> IO ()
loadP1 sh@(sh' :. _)  getVec mvec
  = mvec `deepSeqMVec` 
    do  traceEventIO "RepaExpand1.loadP1: start"

        -- loops over sh' (not sh)
        fillChunkedP (size sh') (unsafeWriteMVec1 mvec) (getVec sh)

        traceEventIO "RepaExpand1.loadP1: end"

unsafeWriteMVec1 :: (U.Unbox e) => MVec U e -> Int -> U.Vector e -> IO ()
unsafeWriteMVec1 dst n src = goWrite 0
        where
            len = U.length src
            add = n * len
            goWrite :: Int -> IO()
            goWrite c
                | c >= len  = return ()
                | otherwise = do unsafeWriteMVec dst (add + c) (src `U.unsafeIndex` c)
                                 goWrite (c+1)
