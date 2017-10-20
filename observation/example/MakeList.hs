{-# LANGUAGE CPP #-}
module Main (main) where

import Oceanogr.CTD

import Control.Monad (forM, forM_)
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed   as V
import Data.UnixTime

import System.Directory
import System.FilePath ((</>))
import System.IO
import Text.Printf

#ifdef WOCE
import P17E_1992
ctdReader :: CTDfileRead
ctdReader = knorr
#endif

#ifdef GOSHIP
import MR1609
ctdReader = mirai
#endif

main :: IO ()
main = do
    files <- filter (\f -> f /= "." && f /= "..") `fmap` getDirectoryContents unzippedDir
    ctds  <- forM files $ \file -> readCTD (unzippedDir </> file) ctdReader

    -- station list
    printf "   station  cast    lon       lat       dep       maxp\n"
    forM_ ctds $ \ctd -> let s  = ctdStation ctd
                          in do t <- formatUnixTime webDateFormat (stnTime s)
                                printf "%10s%3d%10.3f%10.3f%10.1f%10.1f  %s\n"
                                        (B.unpack . fst $ stnCast s) (snd . stnCast $ s)
                                        (stnLongitude s) (stnLatitude s) (stnDepth s)
                                        (V.maximum . V.filter (not . isNaN) . ctdP $ ctd)
                                        (B.unpack t)
