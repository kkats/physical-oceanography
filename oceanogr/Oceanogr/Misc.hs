-- Gaussian filter for topography
---- $Id: SmoothTopo.hs,v 1.1 2014/02/24 01:42:14 ka Exp ka $
module Oceanogr.Misc
            (idx2sub,
             interp2,
             closest,
             deg2min,
             inbetween,
             nanmean,
             nanvar,
             nansum,
             nanmean2,
             findRange,
             findRange'
            )
where
import Data.Array.Repa
import Numeric.IEEE (nan, IEEE)

import qualified Data.Vector.Unboxed as U

--
-- | find (pred)
--
findRange, findRange' :: Array U DIM1 Double -> (Double, Double) -> [Int]
-- 1. AND version
findRange ruler (low, high)
        = filter (\i -> let x = ruler `unsafeIndex` ix1 i in low <= x && x <= high)
                                                        [0 .. (size (extent ruler)-1)]
-- 2. OR version
findRange' ruler (low, high)
        = filter (\i -> let x = ruler `unsafeIndex` ix1 i in low <= x || x <= high)
                                                        [0 .. (size (extent ruler)-1)]

--
-- | NaN's
--
nanmean, nansum, nanvar :: (RealFloat a, IEEE a) => [a] -> a
nansum xs = sum $ filter (not . isNaN) xs
{-# INLINE nansum #-}
nanmean xs = let xs' = filter (not . isNaN) xs
              in sum xs' / fromIntegral (length xs')
{-# INLINE nanmean #-}
nanvar xs = let xs' = filter (not . isNaN) xs
                n   = fromIntegral (length xs')
             in if n == 1
                 then nan  -- variance not defined
                 else sum (Prelude.map (^(2::Integer)) xs') / n - (sum xs' / n)^(2::Integer)
{-# INLINE nanvar #-}
nanmean2 :: (RealFloat a, IEEE a) => a -> a -> a
nanmean2 a b
    | not (isNaN a) && isNaN b = a
    | isNaN a && not (isNaN b) = b
    | isNaN a && isNaN b       = nan
    | otherwise                = (a + b) * 0.5
{-# INLINE nanmean2 #-}
--
-- | degree to minutes
--
deg2min :: Double -> (Int, Double)
deg2min x = let deg = truncate x :: Int
                mi' = abs (x - fromIntegral deg) * 60.0
             in (deg, mi')
{-# INLINE deg2min #-}
--
-- | Linear interpolation in 2D
--   cyclic in Lon, fenced in Lat
--
interp2 :: (Source r e, U.Unbox e, Fractional e, Ord e, Show e)
        => Array U DIM1 e  -- ^ src x | Dim 2
        -> Array U DIM1 e  -- ^ src y | Dim 1
        -> Array r DIM2 e  -- ^ src z | (y, x)
        -> (e, e)          -- ^ dst
        -> e
interp2 xs ys zs (xd, yd)
    = let xsU'= toUnboxed xs
          x0  = xsU' U.! 0; x1 = xsU' U.! 1
          xsU = U.cons (2 * x0 - x1) xsU' -- extend to the left to make cyclic
                                          -- i.e. iHere is incremented by 1 (***)
          ysU = toUnboxed ys
          (iHere, xd') = inbetweenCyclic xsU xd
          (jHere, yd') = inbetweenFence  ysU yd      

          (iHere0, iHere1, dx) | iHere == 0 = (U.length xsU' - 1, 0, x1 - x0)
                               | otherwise  = (iHere-1, iHere,              -- (***)
                                                xsU' U.! iHere - xsU' U.! (iHere-1))

          (jHere0, jHere1, dy) | jHere == U.length ysU - 1 = (jHere, jHere, 1.0)
                               | otherwise                 = (jHere, jHere+1,
                                                        ysU U.! (jHere+1) - ysU U.! jHere)

          a = (xd' - xsU U.! iHere) / dx
          b = (yd' - ysU U.! jHere) / dy

          z0 = zs `index` ix2 jHere0 iHere0    -- UNSAFE's
          z1 = zs `index` ix2 jHere0 iHere1
          z2 = zs `index` ix2 jHere1 iHere0
          z3 = zs `index` ix2 jHere1 iHere1

       in   a       * b       * z3
          + (1.0-a) * b       * z2
          + a       * (1.0-b) * z1
          + (1.0-a) * (1.0-b) * z0

{-# INLINE interp2 #-}
----
---- | Locators
----
-- [dummy, minl] = min(abs(ruler - here))
closest :: (U.Unbox a, Num a, Ord a) => U.Vector a -> a -> Int
closest ruler here = U.minIndex $ U.map (abs . subtract here) ruler
{-# INLINE closest #-}

-- | ruler should be sorted (but NOT checked)
inbetweenCyclic, inbetweenFence :: (U.Unbox a, Fractional a, Ord a, Show a)
            => U.Vector a -- ^ ruler
            -> a          -- ^ current position
            -> (Int, a)   -- ^ index and "adjusted" position
inbetween                       :: (U.Unbox a, Fractional a, Ord a)
                                                => U.Vector a -> a -> Maybe Int
inbetween ruler here
    = let ruler' = U.map (subtract here) ruler
       in if U.head ruler' * U.last ruler' > 0.0
            then Nothing
            else U.findIndex (\(p,q) -> p*q <= 0.0) $ U.zip ruler' (U.tail ruler')
{-# INLINE inbetween #-}
--
-- this "Cyclic" does NOT always work. Designated to be used only with interp2 above.
-- Do NOT export
--
inbetweenCyclic ruler here
    | maxr < here = inbetweenCyclic ruler (here - cycle')
    | minr > here = inbetweenCyclic ruler (here + cycle')
    | otherwise   = case inbetween ruler here of
                        Just i  -> (i, here)
                        Nothing -> error "inbetweenCyclic"
  where
    maxr = U.maximum ruler
    minr = U.minimum ruler
    cycle' = maxr - minr
{-# INLINE inbetweenCyclic #-}
inbetweenFence ruler here
    | maxr <= here = (U.maxIndex ruler, maxr)
    | minr >= here = (U.minIndex ruler, minr)
    | otherwise   = case inbetween ruler here of
                        Just i  -> (i, here)
                        Nothing -> error $ "inbetweenFence" Prelude.++ show here
  where
    maxr = U.maximum ruler
    minr = U.minimum ruler
{-# INLINE inbetweenFence #-}

-- | zero-origin (i.e.) idx2sub 3 3 = (1,0)
--   no boundary check
idx2sub :: Int {- idx-} -> Int {- IM -} -> (Int, Int) {- (j,i) -}
idx2sub = quotRem 
