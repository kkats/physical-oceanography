{-# LANGUAGE CPP #-}
module Main (main) where

import SBEproc (ctdDir, ctdReader, isCTDdata)

import Oceanogr.CTD
import Oceanogr.CTD.Z

import Oceanogr.GSW (putLoc, gsw_distance, linearInterp1)

import Control.Monad (forM, forM_, unless, when)
import Data.List (minimum, maximum)
import qualified Data.Vector.Unboxed as V
import GHC.Float (float2Double)
import System.Directory
import System.Environment (getProgName, getArgs)
import System.FilePath ((</>))
import System.IO
import Text.Printf

#ifdef PRESSURE
-- use the list to find max pressure for plotting
maxP :: Int
maxP = 5800
#endif

#ifdef DENSITY
gamms :: V.Vector Float
gamms = V.fromList $ Prelude.map (/10) [200,201 .. 285]
#endif

#ifdef SIGMA
sigma :: V.Vector Float
sigma = V.fromList $ Prelude.map (/100) [2000,2001 .. 5000]
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
                    

  files <- filter isCTDdata `fmap` getDirectoryContents ctdDir
  ctds  <- forM files $ \file -> readCTD (ctdDir </> file) ctdReader >>= addCTSA

  -- station list
  list <- readStnList "work/leg1.list"  -- edited list

  let (lon, lat, dep, dx, ac) = abscissaCTD ctds list

#ifdef PRESSURE
      y = V.map (fromIntegral :: Int -> Float) . V.fromList $ [0,2 .. maxP]
      prof item ctd y = return (zOnP item ctd y)
#endif
#ifdef DENSITY
      y = gamms
      prof = zOnG
#endif
#ifdef SIGMA
      y = sigma
      prof = zOnSigma4
#endif

      buf :: [String]
      buf = printf "# %s y = [%8.2f, %8.2f] dif1=%8.2f\n" "KY1804 Line 9"
                        (V.head y) (V.last y) (y V.! 1 - V.head y)
            : map (\l -> printf "%3d %10.3f%10.3f%10.1f%10.3f%10.3f\n" l
                        (lon !! l) (lat !! l) (dep !! l) ((dx !! l) / 1000) ((ac !! l) / 1000))
                    [0 .. (length lat - 1)]

      dlon = gsw_distance (putLoc (head lon) (head lat)) (putLoc (last lon) (head lat))
      dlat = gsw_distance (putLoc (head lon) (head lat)) (putLoc (head lon) (last lat))
      loa  = if dlon > dlat then lon -- zonal
                            else lat -- meridional
      loaI = [ceiling (minimum loa) .. floor (maximum loa)] :: [Int]

  dl' <- linearInterp1 (V.fromList . map float2Double $ loa)
                       (V.fromList . map (/1000) . map float2Double $ ac)
                       (V.fromList . map fromIntegral $ loaI)

  case dl' of
      Left e -> error $ "linearInterp1: " ++ e
      Right dl -> do
        let bufI :: [String]
            bufI = map (\l -> printf "%8d%12.1f\n" (loaI !! l) (dl V.! l)) [0 .. (V.length dl-1)]
        
        writeFile "work/xy.dat" (concat buf)
        writeFile "work/xyI.dat" (concat bufI)

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
