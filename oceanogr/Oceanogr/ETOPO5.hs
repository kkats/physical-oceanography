-- |
--
-- "/The file may be thought of as having a logical record size of 
--  8640 bytes.  The data start at the North Pole (90 deg N, 0 deg 0'E)
--  and are arranged in bands of 360 degrees x 12 points\/degree
--  = 4320 values (8640 bytes) ranging eastward from 0 deg 0' East 
--  longitude to 359 deg 55' East longitude (since it represents the 
--  North Pole, all possible longitudes still refer to a single 
--  point, thus the first band has 4320 identical values of -4290 m). 
--  The 8641st starts the latitude band for 89 deg 55' N, and so on. 
--  There is NO record for the South Pole (elevation 2810 m.)/"
--
--        <http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5/ETOPO5.txt>
--
module Oceanogr.ETOPO5 (readEtopo5, dEtopo5) where
import Data.Array.Repa hiding (map)
import Data.Binary.Get
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<$>), (<*>))
import Data.Int (Int16)

-- import Text.Printf (printf)

dataFile :: FilePath
dataFile = "/data/pub/ETOPO/ETOPO5.DOS" -- Little Endian, 16 byte Int

dEtopo5 :: Double -- grid interval
dEtopo5 = 1.0 / 12.0

readEtopo5 :: IO (Array U DIM2 Double, U.Vector Double, U.Vector Double)
readEtopo5 = do
    topo' <- readVecI16le dataFile
    let topo = fromUnboxed (ix2 2160 4320) topo'
        lat = map ((* dEtopo5) . i2d) $ reverse $ tail ([(-90*12) .. (90*12)]::[Integer]) -- first is redundant
        lon = map ((* dEtopo5) . i2d) $ init ([(0*12) .. (360*12)]::[Integer])  -- last is redundant

    return (topo, U.fromList lon, U.fromList lat)

-- IO
parser :: Get a -> Get [a]
parser getf = do
    ie <- isEmpty
    if ie then return []
          else (:) <$> getf <*> parser getf


readVecI16le :: FilePath -> IO (U.Vector Double)
readVecI16le fname = do
    buf <- B.readFile fname
    let fvec = map i2d $ runGet (parser getInt16le) buf
    return $ U.fromList fvec

i2d :: (Integral a) => a -> Double
i2d i = fromIntegral i :: Double


{-
--
--
-- testing purposes only
tester :: IO()
tester = do
    (topo, lon, lat) <- readEtopo5
    loop topo lon lat
    return ()
  where
    loop topo lon lat = do
        putStrLn "i?"
        i' <- getLine
        let i = read i' :: Int
        if i < 0 || i >= U.length lon then return ()
                                      else do
        putStrLn "j?"
        j' <- getLine
        let j = read j' :: Int
        if j < 0 || j >= U.length lat then return ()
                                      else do
        printf "%.3f at Lat=%.2f, Lon=%.2f\n" (topo ! (ix2 i j)) (lat U.! i) (lon U.! j)
        loop topo lon lat
-}
