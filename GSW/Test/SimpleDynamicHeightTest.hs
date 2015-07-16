module Main where
import Oceanogr.GSW
-- import Oceanogr.GSWtools

import Control.Monad as C
import Data.Ord (comparing)
import Data.List (zip3, unzip3, zip4)
import qualified Data.Vector.Unboxed as U
import Text.Printf

main :: IO ()
main = do
    let matlabOutput = [23.3966,21.8627,20.3569,18.9108,17.5249,16.1720,14.8538,
                    13.5829,12.3850,11.2573,10.1976,9.2010,8.2643,7.3888,6.5711,
                    5.7996,5.0658,4.3608,3.6819,3.0276,2.3936,1.7751,1.1698,0.5783,
                    0,(-0.5666),(-1.1240),(-1.6711)] :: [Double]
        mpres = [100, 200 .. 2800] :: [Double]


    c <- readFile "Test/example.dat"

    let ls = lines c
        ws0 = words . head $ ls
        lon = read (ws0 !! 0) :: Double
        lat = read (ws0 !! 1) :: Double
        n   = read (ws0 !! 2) :: Int
        (ss, ts, ps) = unzip3 $ map (\l -> let ws = words l
                                               s  = read (ws !! 0) :: Double
                                               t  = read (ws !! 1) :: Double
                                               p  = read (ws !! 2) :: Double
                                            in (s,t,p)) $ tail ls -- skip the header line
        salt = U.fromList ss :: U.Vector Double
        temp = U.fromList ts :: U.Vector Double
        pres = U.fromList ps :: U.Vector Double
        pint' = U.fromList [100 .. 2800] :: U.Vector Double

    sintp' <- interp1 pres salt pint'
    printf "Depth     Haskell    Matlab\n"
    case sintp' of
        Left e0 -> error e0
        Right sintp'' -> do
            let (pint, sintp) = U.unzip $
                                U.takeWhile (\(_,s0) -> (not . isNaN) s0) (U.zip pint' sintp'')

            tintp' <- interp1 pres temp pint'
            case tintp' of
                Left e1 -> error e1
                Right tintp'' -> do
                    let tintp = snd $ U.unzip $
                                U.takeWhile (\(_,t0) -> (not . isNaN) t0) (U.zip pint' tintp'')

                    g <- gsw_geo_strf_dyn_height sintp tintp pint 2500.0

                    case g of
                      Left e1  -> error e1
                      Right g1 -> C.forM_ [0 .. (Prelude.length mpres - 1)] (\i ->
                        let j = U.minIndexBy (comparing abs) $ U.map (subtract $ mpres !! i) pint
                         in printf "%10.0f %10.4f %10.4f\n" (mpres !! i) (g1 U.! j) (matlabOutput !! i))
                        
