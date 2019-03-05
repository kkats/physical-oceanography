{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Oceanogr.CTD

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed   as V

import Control.Monad (forM, mapM_)
import Data.List (sortOn)
import Data.UnixTime
import System.Directory
import System.FilePath ((</>))
import Text.Printf

import SBEproc (ctdDir, ctdReader, isCTDdata)

main :: IO ()
main = do
    files <- filter isCTDdata `fmap` getDirectoryContents ctdDir
    ctds' <- forM files $ \file -> readCTD (ctdDir </> file) ctdReader
    let ctds = sortOn (stnTime . ctdStation) ctds'

    -- station list
    printf "#  station  cast    lon       lat       dep      maxp\n"
    mapM_ printStnList ctds
