{-# LANGUAGE OverloadedStrings #-}
module SBEproc (ctdDir, ctdReader, isCTDdata) where

import Oceanogr.CTD
import qualified Data.ByteString.Char8 as B
import Data.List (elemIndex)
import Text.Printf (printf)

-- import Debug.Trace

ctdDir :: FilePath
ctdDir = "/data/private/KY1804-Leg1/Oceanography_Phys_Chem/Data_Leg1/CTD/Data/CTD/proc"

-- define relevant CTD files
isCTDdata :: FilePath -> Bool
isCTDdata "."  = False
isCTDdata ".." = False
isCTDdata f    = let len = length f
                     pre = head f
                     sfx = drop (len-3) f
                  in pre == 'd' && sfx == "cnv"

-- last line of the header
isHLL :: B.ByteString -> Bool
isHLL s = if B.length s < 5
            then False
            else let ss = B.take 5 s
                  in ss == "*END*"

ctdReader :: CTDfileRead
ctdReader = CTDfileRead isHLL
                        (degLine  "* NMEA Longitude =")
                        (degLine  "* NMEA Latitude =")
                        (itemEq'  "** Station:"          id)
                        (const $ Just 0)  -- CAST is not defined, returns zero tentatively
                        (itemEq'  "** Bottom Depth [m]:" sf)
                        (findDate "* NMEA UTC (Time) =")
                        (findTime "* NMEA UTC (Time) =")
                        (map sf . B.words)
                        (1,26) -- P
                        (3,26) -- T, ITS-90
                        (20,26) -- S, PSS (-78?)
                        (18,26)
                        [0]

-- match "item = value" where spaces exist in "item"
dropItem :: [B.ByteString] -> [B.ByteString] -> Maybe [B.ByteString]
dropItem (i1:is) (w1:ws) = if i1 == w1 then dropItem is ws else Nothing
dropItem []       ws     = Just ws
dropItem _        []     = Nothing

itemEq' :: B.ByteString -> (B.ByteString -> a) -> B.ByteString -> Maybe a
itemEq' item f s = case dropItem (B.words item) (B.words s) of
                       Just ws -> if null ws then Nothing
                                             else Just $ f (head ws)
                       Nothing -> Nothing

-- .... = deg min (S|N)|(W|E)
degLine :: B.ByteString -> B.ByteString -> Maybe Float
degLine item s = case dropItem (B.words item) (B.words s) of
                     Nothing -> Nothing
                     Just ws -> let deg  = sf $ head ws
                                    min  = sf $ ws !! 1
                                    nswe = ws !! 2
                                 in if nswe == "S" || nswe == "W"
                                      then Just $ negate (deg + min / 60)
                                      else if nswe == "N" || nswe == "E"
                                             then Just $ deg + min / 60
                                             else error $ "degLine :: unrecognized " ++ show nswe
-- date and time
findDate, findTime :: B.ByteString -> B.ByteString -> Maybe B.ByteString
findDate item s = case dropItem (B.words item) (B.words s) of
                      Nothing -> Nothing
                      Just ws -> let month = head ws
                                     mday  = si $ ws !! 1
                                     year  = si $ ws !! 2
                                  in case elemIndex month ["Jan","Feb","Mar","Apr","May","Jun",
                                                    "Jul","Aug","Sep","Oct","Nov","Dec"] of
                                         Just mon -> Just . B.pack $ printf "%4d%02d%02d" year (mon+1) mday
                                         Nothing -> Nothing

findTime item s = case dropItem (B.words item) (B.words s) of
                      Nothing -> Nothing
                      Just ws -> let hh = B.take 2 $ last ws
                                     mm = B.take 2 . B.drop 3 $ last ws
                                  in Just $ hh `B.append` mm
