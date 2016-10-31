module Oceanogr.Statistics (pca, pcaTransform, pcaN, olsD) where

import qualified Data.Array.IArray as I
import Data.List              (sortBy)
import Data.Ord               (comparing)
import Numeric.Statistics     (covarianceMatrix)
import Numeric.GSL.Statistics (mean)
import Numeric.LinearAlgebra

-- | find the principal components of multidimensional data greater than
--    the threshhold
pca :: I.Array Int (Vector Double)    -- the data
    -> Double                         -- eigenvalue threshold
    -> ([Double], Matrix Double)
pca d q = let d' = fmap (\x -> x - (scalar $ mean x)) d -- remove the mean from each dimension
              cv = covarianceMatrix d'
              (val',vec') = eigSH $ trustSym cv -- the covariance matrix is real symmetric
              val = toList val'
              vec = toColumns vec'
              v' = zip val vec
              v = filter (\(x,_) -> x > q) v'  -- keep only eigens > than parameter
              (va, ve) = unzip v
          in (va, fromColumns ve)

pcaTransform :: I.Array Int (Vector Double)    -- ^ the data
             -> Matrix Double                  -- ^ the principal components
             -> I.Array Int (Vector Double)    -- ^ the transformed data
pcaTransform d m = let d' = fmap (\x -> x - (scalar $ mean x)) d -- remove the mean from each dimension
                   in I.listArray (1,cols m) $ toRows $ (tr' m) <> (fromRows $ I.elems d')

-- | find N greatest principal components of multidimensional data
--    according to size of the eigenvalue
pcaN :: I.Array Int (Vector Double)    -- the data
     -> Int                            -- number of components to return
     -> ([Double], Matrix Double)
pcaN d n = let d' = fmap (\x -> x - (scalar $ mean x)) d -- remove the mean from each dimension
               cv = covarianceMatrix d'
               (val',vec') = eigSH $ trustSym cv  -- the covariance matrix is real symmetric
               val = toList val'
               vec = toColumns vec'
               v' = zip val vec
               v = take n $ reverse $ sortBy (comparing fst) v'
               (va, ve) = unzip v
           in (va, fromColumns ve)

-- | ordinary least squares estimation for the multivariate model
--   Y = X B + e        rows are observations, columns are elements
--   mean e = 0, cov e = kronecker s I
olsD :: -- (Num (Vector t), Field t) ==>
       Matrix Double        -- ^ X
    -> Matrix Double        -- ^ Y
    -> (Matrix Double, Matrix Double, Matrix Double) -- ^ (OLS estimator for B, OLS estimator for s, OLS residuals)
olsD x y 
    | rows x /= rows y = error "Numeric.Statistics: ols: incorrect matrix dimensions"
    | otherwise       = let (xr,xc) = (rows x,cols x)
                            -- (yr,yc) = (rows y,cols y)
                            z = (tr' x) <> x
                            r = rank z
                            beta = if r == xc 
                                      then (inv z) <> (tr' x) <> y
                                      else (pinv x) <> y
                            rr = y - x <> beta
                            sigma = ((tr' rr) <> rr) / (fromIntegral $ xr - r)
                        in (beta,rr,sigma)

-----------------------------------------------------------------------------


-- The above programme is a modification from hstatistics-2.5.3/Numeric/Statistics.PCA.hs
--                                        and hstatistics-2.5.3/Numeric/Statistics.hs
--
-- The original LICENSE is shown below;
--
-- Copyright (c) 2010, A.V.H. McPhail
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--    * Neither the name of the author nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

--THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
