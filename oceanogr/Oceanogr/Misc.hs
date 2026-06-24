-- Gaussian filter for topography
---- $Id: SmoothTopo.hs,v 1.1 2014/02/24 01:42:14 ka Exp ka $
module Oceanogr.Misc
            (idx2sub,
             linearInterp1,
             interp2,
             closest,
             deg2min,
             inbetween,
             nanmean,
             nanvar,
             nansum,
             nanmean2,
             findRange,
             findRange',
             histocount,
             interpBtl,
             interpRRD
            )
where
import Control.Monad (when)
import Data.Array.Repa hiding (map, (++))
import Numeric.IEEE (nan, IEEE)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

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

---
-- |
--  Combination of interpRRD and interp1
--  First, apply interpRRD, second nan's are
--  filled by interp1
--
-- >>> let x = U.fromList [2020,2216,2413,2611,2878,3000] :: U.Vector Double
-- >>> let y = U.fromList [2.211,2.011,1.894,1.788,1.554,1.380] :: U.Vector Double
-- >>> let xi = U.fromList [2100,2200 .. 3000] :: U.Vector Double
-- >>> interpBtl x y xi
-- >>> Right (fromList [2.1293673469387753,2.027326530612245,1.9570207317973867,1.9013255208670579,1.8455641980510156,1.7947405764986781,1.7292355861090267,1.6473991803675845,1.5226229508196722,1.38])
--
--
interpBtl :: U.Vector Double -- ^ bottle pressure
        -> U.Vector Double -- ^ bottle data
        -> U.Vector Double -- ^ pressure to interpolate onto
        -> IO (Either String (U.Vector Double)) -- ^ output

interpBtl x y xi = do

    y0 <- interpRRD x y xi

    case y0 of
      Left e0  -> return $ Left e0
      Right y1 -> do
        let idx = U.fromList [0 .. (U.length xi - 1)] :: U.Vector Int
            gap = U.filter (\(_,_,y10) -> isNaN y10) $ U.zip3 idx xi y1
        if U.null gap
        then return $ Right y1
        else do
            let (ig, xg, _) = U.unzip3 gap
            yg <- linearInterp1 x y xg
            case yg of
              Left e1  -> return $ Left e1
              Right y2 -> let toUpdate = U.zip ig y2
                           in return $ Right (U.update y1 toUpdate)


---
-- | Interpolation of "bottle" data
--   optimized for pelagic ocean data
--   based on the method by Reinger and Ross (1968, DSR)
--   tuned by Jeff Dunn.
--
--   Matlab source is in  ..\/matlabGSW3.04\/gsw_geo_strf_dyn_height.m
--   Example below was taken from example.dat in the gamma_n package.
--   observed temperatures at xi are; 12.21,11.99,10.54,8.36,7.43,6.04
--   observed salinities   at xi are; 35.086,35.078,34.851,34.572,34.509,34.452
--
-- >>> let x = U.fromList [1,97,194,388,581,775,969] :: U.Vector Double
-- >>> let ytemp = U.fromList [12.25,12.09,11.69,9.35,7.86,6.87,5.5] :: U.Vector Double
-- >>> let ysalt = U.fromList [35.066,35.089,35.025,34.696,34.531,34.496,34.458] :: U.Vector Double
-- >>> let xi = U.fromList [48,145,291,485,678,872] :: U.Vector Double
-- >>> interpRRD x ytemp xi
-- Right (fromList [NaN,NaN,10.466597777964449,8.45058039567543,7.375825606234917,NaN])
-- >>> interpRRD x ysalt xi
-- Right (fromList [NaN,NaN,34.85724213411662,34.57730605075967,34.51419549586589,NaN])
--
interpRRD :: U.Vector Double -- ^ bottle pressure
          -> U.Vector Double -- ^ bottle data
          -> U.Vector Double -- ^ pressure to interpolate onto
          -> IO (Either String (U.Vector Double)) -- ^ output
interpRRD x y xi
    | U.length x < 4 = return $ Left "Input too short"
    | U.length x /= U.length y
                    = return $ Left "Inconsistent length"
    | not (U.all (>0) (U.zipWith (-) (U.tail x) x)
        || U.all (<0) (U.zipWith (-) (U.tail x) x)) = return $ Left "Unsorted input"
    | otherwise      = do
      let xfn = U.fromList [0, 300, 1800, 8000] :: U.Vector Double
          yfn = U.fromList [50, 200, 650, 1250] :: U.Vector Double

          interp1' :: Double -> Double
          interp1' x0 = case U.findIndex (x0<) xfn of
                         Nothing -> error $ "too deep at x=" ++ show x0
                         Just i0 -> (x0 - xfn U.! (i0-1)) * (yfn U.! i0 - yfn U.! (i0-1))
                                                         / (xfn U.! i0 - xfn U.! (i0-1))
                                       + yfn U.! (i0-1)
          maxDis = U.map interp1' xi
          ilen   = U.length xi
          ivec   = U.fromList [0 .. (ilen-1)] :: U.Vector Int

      yi <- UM.replicate ilen nan :: IO (UM.IOVector Double)

      let coincide :: (Double, Int) -> IO ()
          coincide (x0, j) =
            let i0 = U.minIndexBy (\a b -> abs (a - x0) `compare` abs (b - x0)) x
             in when (abs (x U.! i0 - x0) < U.minimum maxDis * 0.005) -- "concid_frac"
                   $ UM.write yi j (y U.! i0)

      U.mapM_ coincide $ U.zip xi ivec

    
      let interp :: (Double, Double, Int) -> IO ()
          interp (y0, x0, j)
            | not . isNaN $ y0 = return () -- already assigned
            | otherwise        = case inbetween x x0 of
                        Nothing -> return () -- not within the range
                        Just i0 -> if i0 < 1 || i0 > U.length x - 3
                                   then return () -- not within the range
                                   else do
                                   let sumin = abs $ x U.! i0 - x U.! (i0+1)
                                       outl  = abs $ x U.! (i0-1) - x U.! i0
                                       outr  = abs $ x U.! (i0+2) - x U.! (i0+1)
                                       md    = maxDis U.! j
                                   if sumin > md || outl+sumin+outr > md*3
                                        || min outr outl < sumin / 15.0
                                   -- sumin>maxdis(tdix,1) | outtest>maxdis(tidx,2) |
                                   --      minsep<cfrac
                                   then return ()
                                   else let intp = y U.! i0 + (x0 - x U.! i0)
                                                       * (y U.! (i0+1) - y U.! i0)
                                                       / (x U.! (i0+1) - x U.! i0)
                                            extl = y U.! i0 + (x0 - x U.! i0)
                                                       * (y U.! i0 - y U.! (i0-1))
                                                       / (x U.! i0 - x U.! (i0-1))
                                            extr = y U.! (i0+1) + (x0 - x U.! (i0+1))
                                                       * (y U.! (i0+2) - y U.! (i0+1))
                                                       / (x U.! (i0+2) - x U.! (i0+1))
                                            deno = abs (extl - intp) ** 1.7
                                                 + abs (intp - extr) ** 1.7
                                            nume = abs (extl - intp) ** 1.7 * extr
                                                 + abs (intp - extr) ** 1.7 * extl
                                            y9'' = if abs deno < 1.0e-4
                                                   then intp
                                                   else (intp + nume / deno) * 0.5
                                            -- do not overshoot
                                            y9'  = max y9'' $ min (y U.! i0) (y U.! (i0+1))
                                            y9   = min y9'  $ max (y U.! i0) (y U.! (i0+1))
                                         in UM.write yi j y9
      yi' <- U.freeze yi
      U.mapM_ interp $ U.zip3 yi' xi ivec
      Right `fmap` U.unsafeFreeze yi

--
-- | Simple linear interpolation
--
-- >>> let x = U.fromList [1,97,194,388,581,775,969]               :: U.Vector Double
-- >>> let y = U.fromList [12.25,12.09,11.69,9.35,7.86,6.87,5.5]  :: U.Vector Double
-- >>> let xi = U.fromList [48,145,291,485,678,872]              :: U.Vector Double
-- >>> linearInterp1 x y xi
-- Right (fromList [12.171666666666667,11.892061855670104,10.52,8.601139896373057,7.365,6.1850000000000005])
--
linearInterp1 :: U.Vector Double -- ^ bottle pressure
        -> U.Vector Double -- ^ bottle data
        -> U.Vector Double -- ^ pressure to interpolate onto
        -> IO (Either String (U.Vector Double)) -- ^ output
linearInterp1 x y xi
    | U.length x < 2 = return $ Left "Input too short"
    | U.length x /= U.length y
                    = return $ Left "Inconsistent length"
    | not (U.all (>0) (U.zipWith (-) (U.tail x) x)
        || U.all (<0) (U.zipWith (-) (U.tail x) x)) = return $ Left "Unsorted input"
    | otherwise      = do
        yi <- UM.replicate (U.length xi) nan :: IO (UM.IOVector Double)
        let interp :: (Double, Int) -> IO ()
            interp (x0, j) = case inbetween x x0 of
                        Nothing -> return () -- not within the range
                        Just i0 -> UM.write yi j $ y U.! i0 + (x0 - x U.! i0)
                                                    * (y U.! (i0+1) - y U.! i0)
                                                    / (x U.! (i0+1) - x U.! i0)
        U.mapM_ interp $ U.zip xi (U.fromList [0 .. (U.length xi - 1)])
        Right `fmap` U.unsafeFreeze yi
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

--
-- for histogram
--
histocount, histocount' :: Double -- ^ interval
        -> Int -- ^ start counting (unit = dx = interval)
        -> Int -- ^ finish
        -> [Double] -- ^ input
        -> [Int] -- ^ number of occurence
histocount dx m0 m1 z = let trimMin z' = if z' < dx * fromIntegral m0
                                           then dx * (fromIntegral m0 + 0.5)
                                           else z'
                            trimMax z' = if z' > dx * fromIntegral m1
                                           then dx * (fromIntegral m1 - 0.5)
                                           else z'
                         in histocount' dx m0 m1 (map trimMin . map trimMax $ z)
histocount' dx m0 m1 z = histocount'' m0 []
 where
  histocount'' :: Int -> [Int] -> [Int]
  histocount'' m o
         | m == m1   = reverse o
         | otherwise = let left = fromIntegral m * dx
                           right = fromIntegral (m+1) * dx
                           n'    = length . filter (\w -> left <= w && w < right) $ z
                        in histocount'' (m+1) (n':o)
{-# INLINE histocount #-}
