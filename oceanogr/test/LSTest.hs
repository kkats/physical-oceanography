import Test.QuickCheck
import qualified Oceanogr.LeastSquare as P
import qualified Oceanogr.LeastSquares as Q
import qualified Data.Vector.Unboxed as V

-- prop_lsFit1 :: [Double] -> [Double] -> Bool
prop_lsFit1 :: [Double] -> [Double] -> Property
prop_lsFit1 x y = length x > 5 && length y > 5 ==>
                  let (x', y') = unzip $ zip x y -- trim the longer one
                      (bp, _, _) = P.lsFit1 y' x' (replicate (length x') (1::Double))
                      ((bq, _), _, _) = Q.lsFit1 (V.fromList y') (V.fromList x') Nothing
                      bp' = head (concat bp)
                   in abs (bp' - bq) < 0.01 * (if abs bp' < abs bq then abs bq else abs bp')

{-
ordered xs = and (zipWith (<=) xs (drop 1 xs))
insert x xs = takeWhile (< x) xs ++ [x] ++ dropWhile (< x) xs

prop_Insert x xs = ordered xs ==> ordered (insert x xs)
  where types = x::Int
main = quickCheck prop_Insert
-}

main :: IO ()
main = quickCheck prop_lsFit1
