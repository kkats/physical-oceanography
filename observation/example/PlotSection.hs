{-# LANGUAGE CPP #-}
module Main (main) where

import Oceanogr.CTD
import Oceanogr.CTD.Z

import Control.Monad (forM, forM_, unless, when)
import qualified Data.Vector.Unboxed as V
import System.Directory
import System.Environment (getProgName, getArgs)
import System.FilePath ((</>))
import System.IO
import Text.Printf


#ifdef WOCE
import P17E_1992
ctdReader = knorr
listFile  = "work/1992.list"
title = "P17E-1992"
#endif

#ifdef GOSHIP
import MR1609
ctdReader = mirai
listFile  = "work/2017.list"
title = "P17E-2017"
#endif

title :: String
listFile :: FilePath
ctdReader :: CTDfileRead

#ifdef PRESSURE
-- use the list to find max pressure for plotting
maxP :: Int
maxP = 5800
#endif

#ifdef DENSITY
gamms :: V.Vector Float
gamms = V.fromList $ Prelude.map (/10) [200,201 .. 285]
#endif

main :: IO ()
main = do

    args  <- getArgs
    when (length args /= 1) $ getProgName >>= \pg -> error ("Usage: " ++ pg ++ " [P|CT|SA|DO]")

    let item = case head args of
                    "P" -> P
                    "CT" -> CT
                    "SA" -> SA
                    "DO" -> DO
                    _    -> error $ "unknown item: " ++ (head args)
                    

    files <- filter (\f -> f /= "." && f /= "..") `fmap` getDirectoryContents unzippedDir
    ctds  <- forM files $ \file -> readCTD (unzippedDir </> file) ctdReader >>= addCTSA

    -- station list
    list <- readStnList listFile  -- edited list

    let (lon, lat, dep, dx, ac) = abscissaCTD ctds list
        buf :: [String]
        buf = printf "# %s y = [%8.2f, %8.2f] dif1=%8.2f\n" title
                        (V.head y) (V.last y) (y V.! 1 - V.head y)
            : map (\l -> printf "%3d %10.3f%10.3f%10.1f%10.3f%10.3f\n" l
                        (lon !! l) (lat !! l) (dep !! l) ((dx !! l) / 1000) ((ac !! l) / 1000))
                    [0 .. (length lat - 1)]
#ifdef PRESSURE
        y = V.map (fromIntegral :: Int -> Float) . V.fromList $ [0,2 .. maxP]
        prof item ctd y = return (zOnP item ctd y)
#endif
#ifdef DENSITY
        y = gamms
        prof = zOnG
#endif

    writeFile "work/xy.dat" (concat buf)

    z <- sectionCTD ctds list y (prof item)

    -- binary
    -- writeVecF "work/z.bin" z
    -- ascii
    withFile "work/xyz.dat" WriteMode
        (\h -> let ndep = V.length y
                   nstn = length lat
                in forM_ [0 .. (nstn-1)] $ \i ->
                     forM_ [0 .. (ndep-1)] $ \j ->
                        unless (isNaN $ z V.! (i*ndep+j)) $
                          hPrintf h "%10.3f%10.3f%12.4f\n"
                            ((ac !! i)/1000) (y V.! j) (z V.! (i*ndep+j)))
