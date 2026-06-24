{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts,
             OverloadedStrings, CPP, LambdaCase #-}
--
-- | handle Argo profile NetCDF outputs
--
--   argoDir :: FilePath -- to be defined appropriately before use
--
---  For DELAYED mode data only
#define D_ONLY
--
--   station = PLATFORM_NUMBER + ("a", "b", "c", ...) for N_PROF=1, 2, 3, ...
--   cast = CYCLE_NUMBER
--
--
module Oceanogr.Argo (argoDir, dacDir,
argoCTD,
argoSTN,
wmoFname,
findProf
)
where

import qualified Oceanogr.NCRepa as N
import Oceanogr.CTD (CTDdata(..), Station(..), si)

import Control.Exception (bracket)
import Control.Monad (when, forM)
import Data.Array.Repa as Repa hiding ((++))
import Data.Array.Repa.Repr.ForeignPtr
import qualified Data.ByteString.Char8 as B
import Data.Convertible (convert)
import Data.Char (isSpace)
import Data.Map (Map, fromList)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.NetCDF
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.UnixTime
import Foreign.C
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable, peekElemOff)
import GHC.Float (double2Float)
import Math.Extras.Double (isFinite)
import Numeric.IEEE
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)

argoDir :: FilePath
argoDir = "/data/Argo/dac/"

dacDir :: [FilePath]
-- dacDir = ["aoml", "bodc", "coriolis", "csio", "csiro",
--           "incois", "jma", "kma", "kordi", "meds", "nmdis"]
dacDir = ["aoml", "bodc", "coriolis", "csio", "csiro",
          "incois", "jma", "kiost", "kma", "meds", "nmdis"]

-- time origin
argoOrigin :: String
argoOrigin = "1950-01-01 00:00:00 UTC"
tOrigin :: UnixTime
tOrigin = parseUnixTimeGMT "%Y-%m-%d %H:%M:%S UTC" (B.pack argoOrigin)

argoCTD :: FilePath -> IO [CTDdata]
argoCTD = argoCTD' True

argoSTN :: FilePath -> IO [Station]
argoSTN fname = Prelude.map ctdStation `fmap` argoCTD' False fname

argoCTD' :: Bool     -- True to output prpfiles, False to output header only (no profile data)
         -> FilePath -- Argo profile NetCDF file
         -> IO [CTDdata]
argoCTD' needData fname = bracket (openFile fname >>= either (error . show) return) closeFile $ \info -> do
  let nprof = ncDimLength $ fromMaybe (error $ "N_PROF not found:" ++ fname) (ncDim info "N_PROF")
        --nlev = ncDimLength $ fromMaybe (error $ "N_LEVELS not found:" ++ fname) (ncDim info "N_LEVELS")

  when (nprof > 52) $ error ("argoCTD(): nprof > 52 in " ++ show fname) -- bodc/3901987(154) has 36 profiles!

#ifdef D_ONLY
  -- printf fname
  dmode' <- N.getChar info "DATA_MODE" :: IO (Array F DIM1 CChar)
  let dmode = castCCharToChar $ dmode' `index` ix1 0
  case dmode of
   'R' -> return []
   'A' -> return []
   'D' -> do
#else
  do
#endif
    j' <- N.getReal info "JULD" :: IO (Array F DIM1 CDouble)
    jq' <- N.getChar info "JULD_QC" :: IO (Array F DIM1 CChar)
    j <- foreignToU j' (Just jq') :: IO (Array U DIM1 Double)

    lon' <- N.getReal info "LONGITUDE" :: IO (Array F DIM1 CDouble)
    lat' <- N.getReal info "LATITUDE"  :: IO (Array F DIM1 CDouble)
    lon <- foreignToU lon' Nothing :: IO (Array U DIM1 Double)
    lat <- foreignToU lat' Nothing :: IO (Array U DIM1 Double)
    
    cnum <- N.getInt info "CYCLE_NUMBER" :: IO (Array F DIM1 CInt)
    pnum <- N.getChar info "PLATFORM_NUMBER" :: IO (Array F DIM2 CChar)
    let stnnum :: Int -> B.ByteString
        stnnum n = let pnum1 = computeS $ slice pnum (Z:. n :. All) :: Array F DIM1 CChar
                       m = Repa.size (Repa.extent pnum1)
                       pn = filter (not . isSpace) $ Prelude.map (castCCharToChar . index pnum1 . ix1) [0 .. (m-1)]
                    in B.pack pn `B.snoc` "abcdefghijklmnopqurstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" !! n

    case needData of
        False -> let nthData' :: Int -> Maybe CTDdata
                     nthData' np = let cast = (fromIntegral :: CInt -> Int)
                                              (cnum `index` ix1 (np-1))
                                       dayn = j `index` ix1 (np-1)
                                       diff = UnixDiffTime ((convert :: Integer -> CTime) . floor
                                              $ dayn * 24 * 3600) 0
                                       time = addUnixDiffTime tOrigin diff
                                       latn = lat `index` ix1 (np-1)
                                       lonn = lon `index` ix1 (np-1)
                                    in if isFinite latn && isFinite lonn
                                         then Just $ CTDdata (Station (stnnum (np-1), cast) (double2Float lonn) (double2Float latn) 
                                                       (-999) time "noexpo") V.empty V.empty V.empty V.empty V.empty V.empty V.empty
                                         else Nothing
                  in return $ mapMaybe nthData' [1 .. nprof]
        True -> do
            ps' <- readProfVar info "PRES" -- reads "PRES_ADJUSTED"
            ss' <- readProfVar info "PSAL" -- "PSAL_ADJUSTED"
            ts' <- readProfVar info "TEMP" -- "TEMP_ADJUSTED"

            case (ps', ss', ts') of
                (Just ps, Just ss, Just ts) ->
                    let nthData :: Int -> Maybe CTDdata
                        nthData np = let cast = (fromIntegral :: CInt -> Int)
                                                (cnum `index` ix1 (np-1))
                                         dayn = j `index` ix1 (np-1)
                                         diff = UnixDiffTime ((convert :: Integer -> CTime) . floor
                                              $ dayn * 24 * 3600) 0
                                         time = addUnixDiffTime tOrigin diff
                                         latn = lat `index` ix1 (np-1)
                                         lonn = lon `index` ix1 (np-1)
                                         pn = toUnboxed . computeUnboxedS . Repa.map realToFrac
                                              $ slice ps (Z:. np-1 :. All)
                                         sn = toUnboxed . computeUnboxedS . Repa.map realToFrac
                                              $ slice ss (Z:. np-1 :. All)
                                         tn = toUnboxed . computeUnboxedS . Repa.map realToFrac
                                              $ slice ts (Z:. np-1 :. All)
                                      in if isFinite latn && isFinite lonn
                                           then Just $ CTDdata (Station (stnnum (np-1), cast) (double2Float lonn) (double2Float latn) 
                                                       (-999) time "noexpo") pn tn sn V.empty V.empty V.empty V.empty
                                           else Nothing
                     in return $ mapMaybe nthData [1 .. nprof]
                _ -> return []
#ifdef D_ONLY
   _ -> error $ "unknown DATA_MODE (" ++ show fname ++ ")"
#endif
--
--
-- | read pressure or temperature or salinity
--   use _ADJUSTED *ONLY*
--
readProfVar :: forall sh. (Shape sh) =>
                NcInfo NcRead -> String -> IO (Maybe (Array U sh Float))
readProfVar info varname = do

    va <- N.getRealAvail info $ varname ++ "_ADJUSTED"    :: IO (Maybe (Array F sh CFloat))
    qa' <- N.getCharAvail info $ varname ++ "_ADJUSTED_QC" :: IO (Maybe (Array F sh CChar))

    let qa = fromMaybe (error "readProfVar") qa'
    case va of
        -- ADJUSTED available
        Just va' -> foreignToU va' (Just qa) >>= \vau
                                -> if V.all isNaN (toUnboxed vau)
                                     then return Nothing
                                     else return $ Just vau
        -- not available
        Nothing -> return Nothing

--
-- ForeignPtr to Unboxed
--
type family FD i where
    FD CFloat = Float
    FD CDouble = Double

foreignToU :: forall sh a. (Shape sh, Storable a, IEEE a,
                            VM.Unbox (FD a), Fractional (FD a), IEEE (FD a))
                => Array F sh a -- source
                -> Maybe (Array F sh CChar) -- flag, if exists
                -> IO (Array U sh (FD a))
foreignToU x xq =
    let sh0 = Repa.extent x
        m = Repa.size sh0
     in case xq of
    --
    -- Nothing -> (copyS :: Array F sh a -> Array U sh a) x
    -- was impossible because CDoube & CFloat are not V.Unbox
    -- cf. https://qiita.com/mod_poppo/items/cf6b66ff16464c170ac2
    --
    Nothing -> withForeignPtr (toForeignPtr x) $ \xp -> VM.generateM m (\n -> realToFrac `fmap` peekElemOff xp n)
                                                        >>= V.unsafeFreeze >>= return . fromUnboxed sh0
    Just xq' -> withForeignPtr (toForeignPtr x) $ \xp -> VM.generateM m (\n -> let idx = fromIndex sh0 n
                                                                                   yc = castCCharToChar (xq' `index` idx)
                                                                                in if yc == '1' || yc == '2' || yc == '5' || yc == '8'
                                                                                     then realToFrac `fmap` peekElemOff xp n
                                                                                     else return nan)
                                                        >>= V.unsafeFreeze >>= return . fromUnboxed sh0

---
--- WMO and cast to file
---
wmoFname :: IO (Map Int FilePath) -- WMO -> FilePath
wmoFname = do
    list <- forM dacDir $ \dac -> do
        wmo <- listDirectory (argoDir </> dac)
        return $ Prelude.map (\w' -> (si (B.pack w'), argoDir </> dac)) wmo
    return $ fromList (concat list)

---
--- WMO, cast to filepath
--- returns D and R files ONLY
---
findProf :: FilePath -- upto WMO
            -> Int   -- WMO
            -> Int   -- cast
            -> IO FilePath
findProf path wmo cast = do
    let fname = printf "%d_%03d.nc" wmo cast
        fnameD = printf "%d_%03dD.nc" wmo cast -- for descending profile
        test :: FilePath -> IO (Maybe FilePath)
        test t = let thePath = path </> show wmo </> "profiles" </> (t ++ fname)
                     thePathD = path </> show wmo </> "profiles" </> (t ++ fnameD)
                  in doesFileExist thePath >>= \b
                     -> if b then return $ Just thePath
                             else doesFileExist thePathD >>= \bD
                                -> if bD then return $ Just thePathD
                                         else return Nothing

    test "D" >>= \case {
        Just fp -> return fp;
        Nothing -> test "R" >>= \case {
                        Just fp' -> return fp';
                        Nothing -> error $ "not found:" ++ show ((path </> show wmo </> "profiles" </> "X") ++ fname);
                                      }
                        }

