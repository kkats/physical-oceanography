--
-- | Least squares fit with Vector
--
module Oceanogr.LeastSquares (lsFit1, lsFit2, lsFit3) where
import Prelude hiding ((<>))
import qualified Data.Vector.Unboxed as V
import Numeric.IEEE (nan)
import Numeric.LinearAlgebra
import Numeric.Statistics                 (ols)
import Statistics.Distribution            (complCumulative)
import Statistics.Distribution.ChiSquared (chiSquared)

-- import Debug.Trace (trace)

lsFit1, lsFit2 :: V.Vector Double -- ^ y
       -> V.Vector Double -- ^ x
       -> Maybe (V.Vector Double) -- weight or Rnn (not inv(Rnn))
       -> ((Double, Double), Double, Double) -- lsFit 1: ((a, "nan"), squared residual / dof, goodness of fit)
                                             -- lsFit 2: ((a, b), squared residual / dof, goodness of fit)
-- | y = ax + e
lsFit1 = lsFit2' False
-- | y = ax + b + e
lsFit2 = lsFit2' True

tov :: V.Vector Double -> Vector Double -- repa 2 hmatrix
tov   = V.convert

lsFit2' ::   Bool -- ^ flag
          -> V.Vector Double -- ^ y
          -> V.Vector Double -- ^ x(:,1) (x(:,2) will be ones)
          -> Maybe (V.Vector Double) -- ^ weights or Rnn (no inv(Rnn))
          -> ((Double, Double), Double, Double) -- ^ estimated coefs, sigma, NR(15.2.12)
lsFit2' flag y x w
  = if not (V.length y == V.length x)
      then error "lsFit2(): inconsistent (x, y)"
      else let ymat  = asColumn $ tov y
               xmat  = case flag of
                         True -> fromColumns [tov x, fromList $ replicate (V.length y) (1::Double)]
                         False -> asColumn $ tov x
               wmat  = case w of
                         Nothing -> sym $ ident (V.length x)
                         Just w' -> if (V.length w' == V.length y)
                                      then sym . diag $ tov w'
                                      else error "lsFit2():inconsistent (y, w)"
               w2    = inv . chol $ wmat
               xmat' = w2 <> xmat
               ymat' = w2 <> ymat

               (b', res', chi2') = ols xmat' ymat' -- chi2 = ((tr' rr) <> rr) / (n - rank)

               res   = (flatten $ tr' res' <> res') `atIndex` 0
               chi2  = (flatten chi2') `atIndex` 0
               dof   = floor $ res / chi2
               pval  = complCumulative (chiSquared dof) res
               b0    = flatten b' `atIndex` 0
               b1    = case flag of
                         True -> flatten b' `atIndex` 1
                         False -> nan
         in ((b0, b1), chi2, pval)

--
-- | y = a x1 + b x2 + c + e
--
lsFit3 ::    V.Vector Double -- ^ y
          -> V.Vector Double -- ^ x(:,1)
          -> V.Vector Double -- ^ x(:,2) (x(:,3) will be ones)
          -> Maybe (V.Vector Double) -- ^ weights or Rnn (NOT inv(Rnn))
          -> ((Double, Double, Double), Double, Double) -- ^ estimated coefs, sigma, NR(15.2.12)
lsFit3 y x1 x2 w
  = if not (V.length y == V.length x1 && V.length y == V.length x2)
      then error "lsFit3(): inconsistent input (x1, x2, y)"
      else let ymat  = asColumn $ tov y
               xmat  = fromColumns [tov x1, tov x2, fromList $ replicate (V.length y) (1::Double)]
               wmat  = case w of
                         Nothing -> sym $ ident (V.length y)
                         Just w' -> if (V.length w' == V.length y)
                                      then sym . diag $ tov w'
                                      else error "lsFit3(): inconsistent (y, w)"
               w2    = inv . chol $ wmat
               xmat' = w2 <> xmat
               ymat' = w2 <> ymat

               (b', res', chi2') =  ols xmat' ymat'

               res   = (flatten $ tr' res' <> res') `atIndex` 0
               chi2  = (flatten chi2') `atIndex` 0
               dof   = floor $ res / chi2
               pval  = complCumulative (chiSquared dof) res
               b0    = flatten b' `atIndex` 0
               b1    = flatten b' `atIndex` 1
               b2    = flatten b' `atIndex` 2
         in ((b0, b1, b2), chi2, pval)
