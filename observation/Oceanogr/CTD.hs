{-# LANGUAGE OverloadedStrings #-}
-- |
-- CTD data handling
--
module Oceanogr.CTD (
Cast, Station(..), CTDdata(..), CTDitem(..), CTDfileRead(..),
readCTD, readStnList, sectionCTD, abscissaCTD, addCTSA,
findCommon, findCTDfromCast, findIdxfromCast, sf, si, formTime
) where

import Oceanogr.GSW (gsw_distance, putLoc)
import Oceanogr.GSWtools (gsw_ct_from_t, gsw_sa_from_sp)

import qualified Data.ByteString.Char8       as B
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad (unless, when)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString.Char8 (double, signed, decimal)
import Data.Char   (isSpace)
import Data.List   (findIndex, minimumBy)
import Data.Maybe  (fromMaybe)
import Data.Monoid (mconcat, Last(..))
import Data.UnixTime
import GHC.Float (double2Float, float2Double)
import Numeric.IEEE (nan)
import Text.Printf

-- import Debug.Trace

--
-- Records
--
type Cast = (B.ByteString {- station name-}, Int {- cast -})

data Station = Station {
             stnCast      :: Cast,
             stnLongitude :: Float,
             stnLatitude  :: Float,
             stnDepth     :: Float,
             stnTime      :: UnixTime
            }
instance Show Station where
    show s = printf "stn=%s, cast=%d, lon=%8.4f, lat=%8.4f, depth=%7.1f (%s)"
                    (B.unpack . fst . stnCast $ s) (snd. stnCast $ s)
                    (stnLongitude s) (stnLatitude s) (stnDepth s)
                    (B.unpack $ formatUnixTimeGMT webDateFormat (stnTime s))

data CTDdata = CTDdata {
             ctdStation :: Station,
             ctdP       :: V.Vector Float,
             ctdT       :: V.Vector Float,
             ctdS       :: V.Vector Float,
             ctdDO      :: V.Vector Float,
             ctdCT      :: V.Vector Float,
             ctdSA      :: V.Vector Float
            } deriving (Show)

data CTDitem = P | T | S | DO | CT | SA

--
-- How to read CTD files
--
data CTDfileRead = CTDfileRead {
            isHeaderLastLine  :: B.ByteString -> Bool,
            getLongitude      :: B.ByteString -> Maybe Float,
            getLatitude       :: B.ByteString -> Maybe Float,
            getStation        :: B.ByteString -> Maybe B.ByteString,
            getCast           :: B.ByteString -> Maybe Int,
            getDepth          :: B.ByteString -> Maybe Float,
            getDate           :: B.ByteString -> Maybe B.ByteString,
            getTime           :: B.ByteString -> Maybe B.ByteString,
            separator         :: B.ByteString -> [Float],
            columnP           :: (Int, Int), -- (value, flag)
            columnT           :: (Int, Int),
            columnS           :: (Int, Int),
            columnDO          :: (Int, Int),
            goodFlags         :: [Int]
        }

--
-- unzipped CTD files
--
readCTD :: FilePath -> CTDfileRead -> IO CTDdata
readCTD fname a = do

    c <- B.readFile fname -- file is so small that we rely on laziness
    let (header, body') = break (isHeaderLastLine a) (B.lines c)
        body            = if null body'
                            then error $ "readCTD(): HeaderLastLine not found in " ++ fname
                            else tail body'

        longitude = fromMaybe (error $ "Longitude not found in " ++ fname)
                        $ (getLast . mconcat) $ map (Last . getLongitude a) header
        latitude  = fromMaybe (error $ "Latitude not found in " ++ fname)
                        $ (getLast . mconcat) $ map (Last . getLatitude a) header
        stnnbr    = fromMaybe (error $ "Stnnbr not found in " ++ fname)
                        $ (getLast . mconcat) $ map (Last . getStation a) header
        cast      = fromMaybe (error $ "Cast not found in " ++ fname)
                        $ (getLast . mconcat) $ map (Last . getCast a) header
        depth     = fromMaybe (error $ "Depth not found in " ++ fname)
                        $ (getLast . mconcat) $ map (Last . getDepth a) header
        date      = fromMaybe (error $ "Date not found in " ++ fname)
                        $ (getLast . mconcat) $ map (Last . getDate a) header
        time      = fromMaybe (error $ "Time not found in " ++ fname)
                        $ (getLast . mconcat) $ map (Last . getTime a) header
        station   = Station (stnnbr,cast) longitude latitude depth (formTime date time)
        maxn      = length body

    -- data
    pres' <- VM.replicate maxn nan :: IO (VM.IOVector Float)
    temp' <- VM.replicate maxn nan :: IO (VM.IOVector Float)
    salt' <- VM.replicate maxn nan :: IO (VM.IOVector Float)
    doxy' <- VM.replicate maxn nan :: IO (VM.IOVector Float)

    let go :: Int -> IO ()
        go n
            | n >= maxn = return ()
            | otherwise = let ws = separator a (body !! n)
                              p  = choose P a ws
                              t  = choose T a ws
                              s  = choose S a ws
                              o  = choose DO a ws
                           in unless (length ws <= 1) -- "END_DATA" or EOF
                                $ do VM.write pres' n p
                                     VM.write temp' n t
                                     VM.write salt' n s
                                     VM.write doxy' n o
                                     go (n+1)
    go 0

    pres <- V.unsafeFreeze pres'
    temp <- V.unsafeFreeze temp'
    salt <- V.unsafeFreeze salt'
    doxy <- V.unsafeFreeze doxy'

    let (pres_, temp_, salt_, doxy_) = V.unzip4
                               $ V.filter (\(p0,t0,s0,d0) -> any (not . isNaN) [p0,t0,s0,d0])
                               $ V.zip4 pres temp salt doxy

    when (V.null pres_ || V.null temp_ || V.null salt_) $ error "Empty P/T/S?"

    return $ CTDdata station pres_ temp_ salt_ doxy_ V.empty V.empty

--
-- Conservative Temperature and Absolute Salinity
--
addCTSA :: CTDdata -> IO CTDdata
addCTSA ctd = do
    let CTDdata stn p t s o _ _ = ctd
        here                    = (float2Double . stnLongitude $ stn,
                                   float2Double . stnLatitude  $ stn)
        conv :: Float -> Float -> Float -> IO (Float, Float)
        conv p3 t3 s3
          | isNaN (p3 + t3 + s3) = return (nan, nan)
          | otherwise            = do
            sa3 <- uncurry (gsw_sa_from_sp (float2Double s3) (float2Double p3)) here
            ct3 <- gsw_ct_from_t sa3 (float2Double t3) (float2Double p3)
            return (double2Float ct3, double2Float sa3)

    (ct, sa) <- V.unzip `fmap` V.mapM (\(p4,t4,s4) -> conv p4 t4 s4) (V.zip3 p t s)

    return $ CTDdata stn p t s o ct sa
            
--
-- From given CTD data and station list, produce a section
--
sectionCTD :: [CTDdata] -> [Cast]
            -> V.Vector Float                                     -- ^ z axis
            -> (CTDdata -> V.Vector Float -> IO (V.Vector Float)) -- ^ profile mapper
                                                            -- (raw data -> z -> profile)
            -> IO (V.Vector Float)                                -- ^ data in 2D
sectionCTD ctds list z prof = do

    let nx   = length list
        stns = map (\m -> findIdxfromCast ctds (list !! m)) [0 .. (nx-1)]
        nz   = V.length z

    dat <- VM.replicate (nx * nz) nan :: IO (VM.IOVector Float)

    -- fill in n-th column of dat by applying `prof` on (stns !! n)'s ctds
    let castWrite :: Int -> IO ()
        castWrite n = let ctd    = ctds !! (stns !! n)
                          offset = n * nz
                          write1 :: Int -> V.Vector Float -> IO ()
                          write1 o z1
                               | o >= V.length z1 - 1 = return ()
                               | otherwise            = do VM.write dat (offset + o) (z1 V.! o)
                                                           write1 (o+1) z1
                       in prof ctd z >>= write1 0

    mapM_ castWrite [0 .. (nx-1)]
    V.unsafeFreeze dat

--
-- From given CTD data and station list, produce a section abscissa
--
abscissaCTD :: [CTDdata] -> [Cast]
            -> ([Float], [Float], [Float], [Float], [Float])
                -- ^ (longitude, latitude, depths, distance, accumulated distance)
abscissaCTD ctds' list = 
    let nxs  = [0 .. (length list-1)]
        ctds = map (\m -> findCTDfromCast ctds' (list !! m)) nxs

        (lons, lats, deps) = unzip3
                            $ map (\l -> let s = ctdStation $ ctds !! l
                                          in (stnLongitude s, stnLatitude s, stnDepth s)) nxs
        dx = map (\l -> let s0 = ctdStation $ ctds !! l
                            s1 = ctdStation $ ctds !! (l-1)
                         in gsw_distance (putLoc (stnLongitude s0) (stnLatitude s0))
                                        (putLoc (stnLongitude s1) (stnLatitude s1))) (tail nxs)
        ac = scanl (+) 0 dx
     in (lons, lats, deps, 0:dx, ac)

--
-- use the first two columns as stnnbr and cast for plotting
--
readStnList :: FilePath -> IO [Cast]
readStnList f = (map extractor . filter (not . isComment) . B.lines) `fmap` B.readFile f
  where
    extractor :: B.ByteString -> Cast
    extractor ll = let ws = B.words ll
                    in if length ws > 1 then (head ws, si $ ws !! 1)
                                        else ("", 0)
    isComment :: B.ByteString -> Bool
    isComment ll = B.length ll > 1 && B.head ll == '#'
                            
--
-- misc
--

-- returns index
findIdxfromCast :: [CTDdata] -> Cast -> Int
findIdxfromCast ctds cast = fromMaybe (error "findCTDfromCast: no such cast")
                                      (findIndex (\ctd -> (stnCast . ctdStation) ctd == cast) ctds)
findCTDfromCast :: [CTDdata] -> Cast -> CTDdata
findCTDfromCast ctds cast = ctds !! (findIdxfromCast ctds cast)

formTime :: B.ByteString -> B.ByteString -> UnixTime
formTime date time = parseUnixTime "%Y%m%d%H%M" (date `B.append` time)

choose :: CTDitem -> CTDfileRead -> [Float] -> Float
choose item a ws = let (vc, fc) = case item of
                                    P -> columnP a
                                    T -> columnT a
                                    S -> columnS a
                                    DO -> columnDO a
                                    _  -> error "choose: not implemented"
                       value    = ws !! vc
                       flag     = round $ ws !! fc
                    in if flag `elem` goodFlags a
                         then value
                         else nan

-- two common Casts within a distance less than dx (in meter)
findCommon :: Float -- maximum distance in [m]
           -> ([Cast], [CTDdata]) -> ([Cast], [CTDdata]) -> [(Cast, Cast)]
findCommon dx (ws, ctdws) (gs, ctdgs)
  = let cast2lonlat ctds cast = let ctd = findCTDfromCast ctds cast
                                 in (stnLongitude . ctdStation $ ctd,
                                     stnLatitude  . ctdStation $ ctd)
        -- fix W and search G
        (long,latg)           = unzip $ map (cast2lonlat ctdgs) gs
        (cast0,_,_)           = unzip3
                              $ map (\cast -> let (lon,lat) = cast2lonlat ctdws cast
                                               in minimumBy (\(_,lonA,latA) (_,lonB,latB)
                                                   -> gsw_distance
                                                        (putLoc lonA latA) (putLoc lon lat)
                                                   `compare`
                                                      gsw_distance
                                                        (putLoc lonB latB) (putLoc lon lat))
                                (zip3 gs long latg)) ws
     in filter (\(w,g) -> let (ow,aw) = cast2lonlat ctdws w
                              (og,ag) = cast2lonlat ctdgs g
                           in gsw_distance (putLoc ow aw) (putLoc og ag) < dx) $ zip ws cast0
--
-- parsers
--
prepstring :: B.ByteString -> B.ByteString
prepstring s0 = let s1 = B.dropWhile isSpace s0
                    s2 = B.takeWhile (not . isSpace) s1
                 in if B.length s2 > 1 && B.last s2 == '.' -- cannot cope with "123."
                      then B.init s2
                      else s2

sf :: B.ByteString -> Float
sf = either (const nan) double2Float . parseOnly double . prepstring

si :: B.ByteString -> Int
si = either (const 0) id . parseOnly (signed decimal) . prepstring

