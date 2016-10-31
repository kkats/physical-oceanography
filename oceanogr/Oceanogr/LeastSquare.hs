--
-- | Least squares fit
--
module Oceanogr.LeastSquare (lsFit2, lsFit2dangerous, lsFit3, lsFit1) where
import Numeric.Statistics                 (ols)
import Numeric.LinearAlgebra              ((<>), diagRect, tr, Matrix, (><))
import Numeric.LinearAlgebra.Data         (fromList, fromLists, toLists, atIndex)
import Numeric.LinearAlgebra              (inv, chol, sym)
import Statistics.Distribution            (complCumulative)
import Statistics.Distribution.ChiSquared (chiSquared)

import Oceanogr.Statistics (olsD)

-- import Debug.Trace (trace)

-- | Least squares fit with one parameter
lsFit1 ::    [Double] -- ^ y
          -> [Double] -- ^ x(:,1)
          -> [Double] -- ^ weights or Rnn (NOT inv(Rnn))
          -> ([[Double]], Double, Double)
                    -- ^ estimated coefs, sigma, goodness of fit i.e. NR(15.2.12)
lsFit1 y'' x1'' w'' =
    if not (length y'' == length x1'' && length y'' == length w'')
    then error "lsFit2(): inconsistent input"
    else let y = tr $ fromLists [y'']
             x = tr $ fromLists [x1'']
             w = sym $ diagRect 0.0 (fromList w'') (length w'') (length w'')
             w2 = inv . chol $ w
             x' = w2 <> x
             y' = w2 <> y
             (b, _, chi2) = olsD x' y'
             dof = length y'' - 1 -- no error check for dof<0
             -- dof = trace (show chi2)((length y'') - 2)
             chi2' = (chi2 `atIndex` (0,0)) * fromIntegral dof -- already divided by dof
             pval = complCumulative (chiSquared dof) chi2'
         in  (toLists b, chi2 `atIndex` (0,0), pval)


-- | Least squares fit with two parameters
lsFit2 ::    [Double] -- ^ y
          -> [Double] -- ^ x(:,1) (x(:,2) will be ones)
          -> [Double] -- ^ weights or Rnn (NOT inv(Rnn))
          -> ([[Double]], Double, Double)
                    -- ^ estimated coefs, sigma, goodness of fit i.e. NR(15.2.12)
lsFit2 y'' x1'' w'' =
    if not (length y'' == length x1'' && length y'' == length w'')
    then error "lsFit2(): inconsistent input"
    else let y = tr $ fromLists [y'']
             x = fromLists $ map (\(p,q) -> [p, q]) $ zip x1'' (repeat 1.0)
             w = sym $ diagRect 0.0 (fromList w'') (length w'') (length w'')
             w2 = inv . chol $ w
             x' = w2 <> x
             y' = w2 <> y
             (b, _, chi2) = olsD x' y'
             dof = length y'' - 2 -- no error check for dof<0
             -- dof = trace (show chi2)((length y'') - 2)
             chi2' = (chi2 `atIndex` (0,0)) * fromIntegral dof -- already divided by dof
             pval = complCumulative (chiSquared dof) chi2'
         in  (toLists b, chi2 `atIndex` (0,0), pval)

-- | "(dangerously)" use NR(15.1.6) instead of NR(15.2.12)
lsFit2dangerous ::    [Double] -- y
          -> [Double] -- x(:,1) 
          -> [Double] -- weights (NOT USED)
          -> ([[Double]], Double, Double) -- estimated coefs and goodness of fit WITH NR(15.1.6)
lsFit2dangerous y'' x1'' _ =
    if length y'' /= length x1''
    then error "lsFit2()dangerous: inconsistent input"                                 
    else let y = tr $ fromLists [y'']                                      
             x = fromLists $ map (\(p,q) -> [p, q]) $ zip x1'' (repeat 1.0)   
             (b, _, chi2) = olsD x y
             dof = length y'' - 2 -- no error check for dof<0               
             chi2' = chi2 `atIndex` (0,0)
             pval = complCumulative (chiSquared dof) chi2'
         in  (toLists b, chi2', pval)                                   

--
-- | Least squares with 2 inputs
--
lsFit3 ::    [Double] -- ^ y
          -> [Double] -- ^ x(:,1)
          -> [Double] -- ^ x(:,2) (x(:,3) will be ones)
          -> [Double] -- ^ weights or Rnn (NOT inv(Rnn))
          -> ([[Double]], Double, Double)
                    -- ^ estimated coefs, sigma, goodness of fit i.e. NR(15.2.12)
lsFit3 y x1 x2 w =
    if not $ length y == length x1 && length y == length x2
    then error "lsFit3: inconsistent input"
    else let y0 = tr $ fromLists [y]
             x0 = fromLists $ map (\(p,q,r) -> [p, q, r]) $ zip3 x1 x2 $ repeat (1.0::Double)
             w0 = sym $ diagRect 0.0 (fromList w) (length w) (length w)
             w2 = inv . chol $ w0
             x' = w2 <> x0
             y' = w2 <> y0
             (b, _, chi2) = olsD x' y'
             dof = length y - 2 -- no error check for dof<0
             chi2' = (chi2 `atIndex` (0,0)) * fromIntegral dof -- already divided by dof
             pval = complCumulative (chiSquared dof) chi2'
         in  (toLists b, chi2 `atIndex` (0,0), pval)


{-
---
--- tests
---
weightTest :: (Matrix Double, Double)
weightTest =
    let y = (3 >< 1) [-2.32, 12.3, -9.9] -- (3 >< 1) [1.17, 2.3, 3.23]
        x = fromLists [[-1.0, 1.0],
                       [0.0, 1.0],
                       [1.0, 1.0]]
        -- weight
        w = sym $ diagRect 0.0 (fromList [0.1, 1.0, 0.4 :: Double]) 3 3
        -- Cholesky
        w2 = inv . chol $ w
        x' = w2 <> x
        y' = w2 <> y
        -- (b <unscaled>, r <scaled>, chi2 <scaled=to be used in the test i.e. NR(15.2.2))
        (b, r, chi2) = olsD x' y'
        dof = 3 - 2
    in  (b, complCumulative (chiSquared dof) $ chi2 `atIndex` (0,0))

simpleTest :: (Matrix Double, Matrix Double, Matrix Double)
simpleTest = 
    let y = (3 >< 1) [1.17, 2.3, 3.23]
        x = fromLists [[-1.0, 1.0],
                       [0.0, 1.0],
                       [1.0, 1.0]]
    in olsD x y
-- "rows are observations, columns are elements"
-}
