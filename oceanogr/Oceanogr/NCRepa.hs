{-# LANGUAGE ScopedTypeVariables #-}
--
-- | NetCDF -> Repa
--
--  `getReal` is polymorphic. To use, the correct knowledge of the type of
--  the variable to read (i.e. Float? Double?) is indispensable, because
--  explicit typing such as
--  @
--  var <- getReal info "var" :: IO (Array F DIM3 CFloat)
--  @
--  is necessary. If NetCDF's automatic conversion by the "scale_factor" attribute
--  and\/or the "add_offset" attribute fails, do check if Float\/Double
--  choice is correct.
--
module Oceanogr.NCRepa (
    get1Double, getReal, getInt, getChar, getRealAvail, getCharAvail)
where
import Data.NetCDF
import Data.NetCDF.Repa ()
import Data.NetCDF.Store

import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Maybe (fromMaybe)

import Foreign.C
import Numeric.IEEE (IEEE(..), nan)

import Prelude hiding (getChar)

get1Double :: NcInfo NcRead -> String -> IO CDouble
get1Double info varname
  = case ncVar info varname of
      Nothing  -> error "getOne: no such variable"
      Just var -> do
        case ncVarType var of
            NcInt -> do
                eval <- get1 info var [0] :: IO (Either NcError CInt)
                case eval of
                    Left err -> error $ show err
                    Right a  -> return $ mycoardsScale1 var a
            _     -> error "get1Double: not implemented"

getReal :: forall sh a. (Shape sh, Real a, FromNcAttr a, NcStorable a, IEEE a)
                        => NcInfo NcRead -> String -> IO (Array F sh a)
getReal info varname
  = case ncVar info varname of
      Nothing  -> error "getReal: no such variable"
      Just var -> do
        case ncVarType var of
            NcDouble -> do
                eval <- get info var :: IO (Either NcError (Array F sh CDouble))
                case eval of
                    Left err -> error $ show err
                    Right a  -> return $ mycoardsScale var a
            NcFloat -> do
                eval <- get info var :: IO (Either NcError (Array F sh CFloat))
                case eval of
                    Left err -> error $ show err
                    Right a  -> return $ mycoardsScale var a
            -- Int (< 2147483647) -> Float (<3.e38)
            NcInt   -> do
                eval <- get info var :: IO (Either NcError (Array F sh CInt))
                case eval of
                    Left err -> error $ show err
                    Right a  -> return $ mycoardsScale var a
            -- Short (< 32767) -> Float (3.e38)
            NcShort   -> do
                eval <- get info var :: IO (Either NcError (Array F sh CShort))
                case eval of
                    Left err -> error $ show err
                    Right a  -> return $ mycoardsScale var a
                                   

            -- otherwise
            _       -> error "getReal: not implemented"

getInt :: forall sh. (Shape sh) => NcInfo NcRead -> String -> IO (Array F sh CInt)
getInt info varname
  = case ncVar info varname of
      Nothing  -> error "getInt: no such variable"
      Just var -> do
        eval <- get info var :: IO (Either NcError (Array F sh CInt))
        case eval of
            Left err -> error $ show err
            Right a  -> return a

getChar :: forall sh. (Shape sh) => NcInfo NcRead -> String -> IO (Array F sh CChar)
getChar info varname
  = case ncVar info varname of
      Nothing  -> error "getChar: no such variable"
      Just var -> do
        eval <- get info var :: IO (Either NcError (Array F sh CChar))
        case eval of
            Left err -> error $ show err
            Right a  -> return a

--
-- Variable may not exist
--
getRealAvail :: forall sh a. (Shape sh, Real a, FromNcAttr a, NcStorable a, IEEE a)
                        => NcInfo NcRead -> String -> IO (Maybe (Array F sh a))
getRealAvail info varname
    = case ncVar info varname of
        Nothing -> return Nothing
        Just _  -> Just `fmap` getReal info varname

getCharAvail :: forall sh. (Shape sh) => NcInfo NcRead -> String -> IO (Maybe (Array F sh CChar))
getCharAvail info varname
    = case ncVar info varname of
        Nothing -> return Nothing
        Just _  -> Just `fmap` getChar info varname

-- | originally Data.NetCDF.coardsScale
--   modified to return NaN when _FillValue
mycoardsScale :: forall a b s. (NcStorable a, NcStorable b, FromNcAttr a, FromNcAttr b,
                                NcStore s, Real a, Fractional b, IEEE b)
             => NcVar -> s a -> s b
mycoardsScale v din = Data.NetCDF.Store.smap xform din
  where (offset, scale, fill) = triplet v :: (b, b, Maybe a) -- ScopedTypeVariables
        xform x = case fill of
                    Nothing -> realToFrac $ realToFrac x * scale + offset
                    Just f -> if x == f
                                then nan
                                else realToFrac $ realToFrac x * scale + offset

mycoardsScale1 :: forall a b. (NcStorable a, NcStorable b, FromNcAttr a, FromNcAttr b,
                                Real a, Fractional b, IEEE b)
                => NcVar -> a -> b
mycoardsScale1 v x = case fill of
                       Nothing -> realToFrac $ realToFrac x * scale + offset
                       Just f -> if x == f
                                   then nan
                                   else realToFrac $ realToFrac x * scale + offset
  where (offset, scale, fill) = triplet v :: (b, b, Maybe a) -- ScopedTypeVariables



triplet :: forall a b. (NcStorable a, NcStorable b, FromNcAttr a, FromNcAttr b,
                        Fractional b) => NcVar -> (b, b, Maybe a)
triplet v = (offset, scale, fill)
  where offset = fromMaybe 0.0 $ ncVarAttr v "add_offset"   >>= fromAttr
        scale  = fromMaybe 1.0 $ ncVarAttr v "scale_factor" >>= fromAttr
        fill   = ncVarAttr v "_FillValue"   >>= fromAttr

