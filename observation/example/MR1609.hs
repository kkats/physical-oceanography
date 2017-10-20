{-# LANGUAGE OverloadedStrings #-}
module MR1609 (unzippedDir, mirai) where
--
--
-- Cruise specific CTDfileRead
--
--
import Oceanogr.CTD

import qualified Data.ByteString.Char8 as B

import Data.List (elemIndex)
import Data.Char (isSpace)
import Text.Printf

unzippedDir :: FilePath
unzippedDir = "/data/pub/GO-SHIP/P17E-2017/unzipped"

mirai :: CTDfileRead
mirai = CTDfileRead (== "DBAR,,ITS-90,,PSS-78,,G/KG,,UMOL/KG,,MG/CUM,%TRANS,,/METER,,FTU,,UE/SQM/S,,MG/CUM,,")
                     (itemEq "LONGITUDE" sf)
                     (itemEq "LATITUDE"  sf)
                     (itemEq "STNNBR"    id)
                     (itemEq "CASTNO"    si)
                     (itemEq "DEPTH"     sf)
                     (itemEq "DATE"      id)
                     (itemEq "TIME"      id)
                     (map sf . B.splitWith (== ','))
                     (0,1) -- P
                     (2,3) -- T
                     (4,5) -- S
                     (6,7) -- DO
                     [2]   -- good flags
                    
-- match a pattern "item = value"
itemEq :: B.ByteString -> (B.ByteString -> a) -> B.ByteString -> Maybe a
itemEq item f s = let ss = B.words s
                   in if length ss >= 3 && head ss == item
                        then Just $ f (ss !! 2)
                        else Nothing
