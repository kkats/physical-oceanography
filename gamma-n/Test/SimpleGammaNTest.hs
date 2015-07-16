module Main where
-- import Oceanogr.GSW
-- import Oceanogr.GSWtools
import Oceanogr.GammaN

import Data.List (zip3, unzip3, zip4, zip5)
import Text.Printf

main :: IO ()
main = do

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
                                            in (s,t,p)) $ tail ls

    printf "Neutral density calculated here\n"
    (g, l, h) <- gamma_n ss ts ps n (lon, lat)
    mapM_ (\(p0, g0, l0, h0) -> printf "%10.5f%10.5f%10.5f%10.5f\n" p0 g0 l0 h0) $ zip4 ps g l h

    let nn = [26.8, 27.9, 28.1]
    (sns, tns, pns, _, _, dpns) <- neutral_surfaces ss ts ps g n nn 3
    mapM_ (\(n0,s0,t0,p0,dp0) -> printf "%10.5f%10.5f%11.5f%11.5f%11.5f\n" n0 s0 t0 p0 dp0) $ zip5 nn sns tns pns dpns

    let exampleOutput = [
                 "    1.00   26.657205    0.001347    0.001347",
                 "   48.00   26.682834    0.001053    0.001053",
                 "   97.00   26.710969    0.000795    0.000795",
                 "  145.00   26.723245    0.000633    0.000633",
                 "  194.00   26.741491    0.000609    0.000609",
                 "  291.00   26.825459    0.000500    0.000500",
                 "  388.00   26.918710    0.000500    0.000500",
                 "  485.00   26.989777    0.000500    0.000500",
                 "  581.00   27.039080    0.000500    0.000500",
                 "  678.00   27.089155    0.000514    0.000514",
                 "  775.00   27.166574    0.000500    0.000500",
                 "  872.00   27.260381    0.000500    0.000500",
                 "  969.00   27.343624    0.000500    0.000500",
                 " 1066.00   27.421587    0.000500    0.000500",
                 " 1260.00   27.557341    0.000500    0.000500",
                 " 1454.00   27.698193    0.000500    0.000500",
                 " 1647.00   27.798448    0.000500    0.000500",
                 " 1841.00   27.866286    0.000500    0.000500",
                 " 2020.00   27.920168    0.000500    0.000500",
                 " 2216.00   27.959251    0.000500    0.000500",
                 " 2413.00   27.997852    0.000500    0.000500",
                 " 2611.00   28.031655    0.000500    0.000500",
                 " 2878.00   28.079949    0.000511    0.000511",
                 " 3000.00   28.117360    0.000504    0.000504",
                 " surfaces",
                 "",
                 "   26.80   34.906435   10.906548    260.096847   0.0",
                 "   27.90   34.630799    2.300252   1953.165140   0.0",
                 "   28.10   34.724831    1.460607   2943.488475   0.0"
                 ]

    printf "\nNeutral density calculated by original Fortran programme\n"
    mapM_ putStrLn exampleOutput
