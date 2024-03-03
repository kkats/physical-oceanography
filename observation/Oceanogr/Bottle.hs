{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Oceanogr.Bottle (readBtl, BTLdata(..)) where
--
-- It is IMPERATIVE that the user edit (comment out, in most cases)
-- (1) OneBtl
-- (2) BTLdata
-- (3) toBTL
-- to match the bottle data.

import Oceanogr.CTD (Station(..), formTime, si)

import qualified Data.ByteString.Lazy  as BL   -- from Word8
import qualified Data.ByteString.Char8 as BC   -- from Char8
import Data.Char (isSpace)
import Data.Csv
import Data.List (sortOn, findIndex)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric.IEEE (nan)
import Text.Printf (printf)

-- import Debug.Trace (trace)

data OneBtl = OneBtl {
        expocode :: !BL.ByteString,
        stnnbr   :: !BL.ByteString,
        castno   :: !Int,
        date     :: !BL.ByteString,
        time     :: !BL.ByteString,
        latitude :: !Float,
        longitude:: !Float,
        depth    :: !Float,
        ctdprs   :: !Float,
        ctdprsF  :: !Int,
        ctdtmp   :: !Float,
        ctdtmpF  :: !Int,
        ctdsal   :: !Float,
        ctdsalF  :: !Int,
        salnty   :: !Float,
        salntyF  :: !Int,
        oxygen   :: !Float,
        oxygenF  :: !Int,
        silcat   :: !Float,
        silcatF  :: !Int,
        nitrat   :: !Float,
        nitratF  :: !Int,
        nitrit   :: !Float,
        nitritF  :: !Int,
        phspht   :: !Float,
        phsphtF  :: !Int,
        cfc11    :: !Float,
        cfc11F   :: !Int,
        cfc12    :: !Float,
        cfc12F   :: !Int,
        cfc113   :: !Float,
        cfc113F  :: !Int,
        sf6      :: !Float,
        sf6F     :: !Int,
        tcarbn   :: !Float,
        tcarbnF  :: !Int,
        alkali   :: !Float,
        alkaliF  :: !Int,
        ph_tot   :: !Float,
        ph_totF  :: !Int,
        delc13   :: !Float,
        delc13F  :: !Int,
        c13err   :: !Float,
        delc14   :: !Float,
        delc14F  :: !Int,
        c14err   :: !Float
} deriving (Eq, Show)


instance FromNamedRecord OneBtl where
    parseNamedRecord r = OneBtl <$> r .: "EXPOCODE"
                                <*> r .: "STNNBR"
                                <*> r .: "CASTNO"
                                <*> r .: "DATE"
                                <*> r .: "TIME"
                                <*> r .: "LATITUDE"
                                <*> r .: "LONGITUDE"
                                <*> r .: "DEPTH"
                                <*> r .: "CTDPRS"
                                <*> r .: "CTDPRS_FLAG_W"
                                <*> r .: "CTDTMP"
                                <*> r .: "CTDTMP_FLAG_W"
                                <*> r .: "CTDSAL"
                                <*> r .: "SALNTY"
                                <*> r .: "SALNTY_FLAG_W"
                                <*> r .: "CTDSAL_FLAG_W" 
                                <*> r .: "OXYGEN"
                                <*> r .: "OXYGEN_FLAG_W"
                                <*> r .: "SILCAT"
                                <*> r .: "SILCAT_FLAG_W"
                                <*> r .: "NITRAT"
                                <*> r .: "NITRAT_FLAG_W"
                                <*> r .: "NITRIT"
                                <*> r .: "NITRIT_FLAG_W"
                                <*> r .: "PHSPHT"
                                <*> r .: "PHSPHT_FLAG_W"
                                <*> r .: "CFC-11"
                                <*> r .: "CFC-11_FLAG_W"
                                <*> r .: "CFC-12"
                                <*> r .: "CFC-12_FLAG_W"
                                <*> r .: "CFC113"
                                <*> r .: "CFC113_FLAG_W"
                                <*> r .: "SF6"
                                <*> r .: "SF6_FLAG_W"
                                <*> r .: "TCARBN"
                                <*> r .: "TCARBN_FLAG_W"
                                <*> r .: "ALKALI"
                                <*> r .: "ALKALI_FLAG_W"
                                <*> r .: "PH_TOT"
                                <*> r .: "PH_TOT_FLAG_W"
                                <*> r .: "DELC13"
                                <*> r .: "DELC13_FLAG_W"
                                <*> r .: "C13ERR"
                                <*> r .: "DELC14"
                                <*> r .: "DELC14_FLAG_W"
                                <*> r .: "C14ERR"

data BTLdata = BTLdata { -- counterpart to CTDdata in Ocenogr.CTD
            btlStation :: Station,
            btlP       :: U.Vector Float,
            btlT       :: U.Vector Float,
            btlS       :: U.Vector Float,
            btlO       :: U.Vector Float,
            btlsilcat  :: U.Vector Float,
            btlnitrat  :: U.Vector Float,
            btlnitrit  :: U.Vector Float,
            btlphspht  :: U.Vector Float,
            btlCFC11   :: U.Vector Float,  -- pmol/kg
            btlCFC12   :: U.Vector Float,
            btlCFC113  :: U.Vector Float,
            btlSF6     :: U.Vector Float,
            btltcarbn  :: U.Vector Float,
            btlalkali  :: U.Vector Float,
            btldelc13  :: U.Vector Float,
            btldelc14  :: U.Vector Float
} deriving (Show)

bottleFile :: FilePath
bottleFile = "../hydro/I07S/bottle/MR190403-20200325-024900-CO2-CDOM-CFC-DOC-FDOM-UREA-EXPO-TCARBON.csv"

delspace :: BL.ByteString -> BC.ByteString
delspace x = let x' = BL.toStrict x :: BC.ByteString
              in BC.filter (not . isSpace) x'

formatD :: BL.ByteString -> BC.ByteString
formatD x = let x' = BL.toStrict x :: BC.ByteString
                x'' = BC.split '/' x'
             in if length x'' /= 3
                  then error $ "formatD: length=" ++ show (length x'')
                  else let mm = si (x'' !! 0)
                           dd = si (x'' !! 1)
                           yyyy = si (x'' !! 2)
                        in BC.pack $ printf "%04d%02d%02d" yyyy mm dd

formatT :: BL.ByteString -> BC.ByteString
formatT x = let x' = BL.toStrict x :: BC.ByteString
                x'' = BC.split ':' x'
             in if length x'' /= 2
                  then error $ "formatT: length=" ++ show (length x'')
                  else let hh = si (x'' !! 0)
                           mm = si (x'' !! 1)
                        in BC.pack $ printf "%02d%02d" hh mm

toStation :: OneBtl -> Station
toStation b
    = Station (delspace (stnnbr b), castno b)
              (longitude b)
              (latitude b)
              (depth b)
              (formTime (formatD (date b)) (formatT (time b)))
              (delspace (expocode b))

cleanse :: Float -> Int -> Float
-- https://exchange-format.readthedocs.io/en/v1.2.0/quality.html
cleanse val flag = if flag == 2
                     then val
                     else nan

splice :: [[Float]] -- [[p0,t0,s0,o0...],[p1,t1,s1,o1...], ...]
       -> [[Float]] -- [[pN,pN-1,... p0],[tN,tN-1,...t0], ...]
splice []  = error "splice: empty input"
splice bd0 = splice' bd0 (replicate (length (head bd0)) [])
splice' :: [[Float]] -> [[Float]] -> [[Float]]
splice' []        out = out
splice' (bd1:bds) out = splice' bds (zipWith (\x1 xs -> x1:xs) bd1 out)

-- splice' bds (_(zipWith (\x1 xs -> x1:xs) bd1 bds):out)

toBtl' :: [(Station, [Float])]     -- [(station, [prs, tmp, sal, oxy, ...])] * 36
       -> [(Station, [[Float]])]   -- (station, [prs], [tmp], [sal], [oxy], ..)
       -> [(Station, [[Float]])]
toBtl' [] out = out
toBtl' pa@(p:_) out
    = let (stn,_) = p
          here = filter (\(s,_) -> stnCast s == stnCast stn) pa
          there = filter (\(s,_) -> stnCast s /= stnCast stn) pa
          bdata = splice (map snd here)
       in toBtl' there ((stn, bdata):out)
                           
toBtl :: V.Vector OneBtl -> [BTLdata]
toBtl vs
    = let ws = V.filter (\(_, f0) -> not (null f0) && (not . isNaN $ head f0)) -- not used if pressure is bad
             $ V.map (\v -> let stn = toStation v
                                prs = cleanse (ctdprs v) (ctdprsF v)  -- 0
                                tmp = cleanse (ctdtmp v) (ctdtmpF v)
                                sal = cleanse (salnty v) (salntyF v)  -- or (ctdsal v) (ctdsalF v)?
                                oxy = cleanse (oxygen v) (oxygenF v)
                                sil = cleanse (silcat v) (silcatF v)
                                nta = cleanse (nitrat v) (nitratF v)  -- 5
                                nti = cleanse (nitrit v) (nitritF v)
                                phs = cleanse (phspht v) (phsphtF v)
                                c11 = cleanse (cfc11 v) (cfc11F v)
                                c12 = cleanse (cfc12 v) (cfc12F v)
                                c13 = cleanse (cfc113 v) (cfc113F v)  -- 10
                                s6f = cleanse (sf6 v) (sf6F v) -- "s6f" is var, "sf6" is func
                                tca = cleanse (tcarbn v) (tcarbnF v)
                                alk = cleanse (alkali v) (alkaliF v)
                                d13 = cleanse (delc13 v) (delc13F v)
                                d14 = cleanse (delc14 v) (delc14F v)  -- 15
                             in (stn, [prs, tmp, sal, oxy, sil, nta, nti, phs,
                                       c11, c12, c13, s6f, tca, alk, d13, d14])) vs
          bs = toBtl' (V.toList ws) []
       in map (\(stn, btldata)
                -> let idx = (fst . unzip
                           $ sortOn snd (zip [0, 1 ..] (head btldata))) -- index sorted by pressure
                    in BTLdata stn (U.fromList $ map ((btldata !! 0) !!) idx) -- 0
                                   (U.fromList $ map ((btldata !! 1) !!) idx)
                                   (U.fromList $ map ((btldata !! 2) !!) idx)
                                   (U.fromList $ map ((btldata !! 3) !!) idx)
                                   (U.fromList $ map ((btldata !! 4) !!) idx)
                                   (U.fromList $ map ((btldata !! 5) !!) idx)
                                   (U.fromList $ map ((btldata !! 6) !!) idx)
                                   (U.fromList $ map ((btldata !! 7) !!) idx)
                                   (U.fromList $ map ((btldata !! 8) !!) idx)
                                   (U.fromList $ map ((btldata !! 9) !!) idx)
                                   (U.fromList $ map ((btldata !! 10) !!) idx)
                                   (U.fromList $ map ((btldata !! 11) !!) idx)
                                   (U.fromList $ map ((btldata !! 12) !!) idx)
                                   (U.fromList $ map ((btldata !! 13) !!) idx)
                                   (U.fromList $ map ((btldata !! 14) !!) idx)
                                   (U.fromList $ map ((btldata !! 15) !!) idx)) bs
                               
readBtl :: IO [BTLdata]
readBtl = do
    buf' <- BC.readFile bottleFile

    let ll = BC.lines buf'
        -- header if "EXPO..."
        hdr' = filter (\l -> BC.take 4 l == "EXPO") ll
        hdr = if length hdr' == 1
                   then head hdr'
                   else error "no header found"
        -- sep if "A14,A6"
        hdrlines = findIndex (\l -> BC.take 4 l == "A14,") ll
        nhdr = maybe (error "no splitter found") id hdrlines
        -- NO end line
        -- endline = findIndex (\l -> BC.take 7 l == "END_DAT") ll
        -- nend = maybe (error "no END_DATA") id endline
        body = drop (nhdr + 1) ll -- (take nend ll)
        -- convert back to lazy
        buf = BL.fromStrict $ BC.unlines (hdr:body)

    case decodeByName buf of
        Left err -> error err
        Right (_, vs :: V.Vector OneBtl) -> return (toBtl vs)
