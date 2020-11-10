{-# LANGUAGE ScopedTypeVariables, ConstraintKinds #-}
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
import Data.Maybe (fromMaybe, fromJust)

import Foreign.C
import Numeric.IEEE (IEEE(..), nan)

import Prelude hiding (getChar)

-- import Debug.Trace

get1Double :: NcInfo NcRead -> String -> IO CDouble
get1Double info varname
  = case ncVar info varname of
      Nothing  -> error "getOne: no such variable"
      Just var ->
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
      Just var ->
        case ncVarType var of
            NcDouble -> do
                eval <- get info var :: IO (Either NcError (Array F sh CDouble))
                case eval of
                    Left err -> error $ show err
                    Right arr  -> return $ mycoardsScale var arr
            NcFloat -> do
                eval <- get info var :: IO (Either NcError (Array F sh CFloat))
                case eval of
                    Left err -> error $ show err
                    Right arr  -> return $ mycoardsScale var arr
            -- Int (< 2147483647) -> Float (<3.e38)
            NcInt   -> do
                eval <- get info var :: IO (Either NcError (Array F sh CInt))
                case eval of
                    Left err -> error $ show err
                    Right arr  -> return $ mycoardsScale var arr
            -- Short (< 32767) -> Float (3.e38)
            NcShort   -> do
                eval <- get info var :: IO (Either NcError (Array F sh CShort))
                case eval of
                    Left err -> error $ show err
                    Right arr  -> return $ mycoardsScale var arr
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
getRealAvail :: forall sh a. (Shape sh, FromNcAttr a, NcStorable a, IEEE a)
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

--
-- | Patched on Data.NetCDF.coardsScale in hnetcdf <https://github.com/ian-ross/hnetcdf>
--   modified to return NaN when _FillValue. Origial LICENSE shown at the bottom.
--
mycoardsScale :: forall a b s. (NcStorable a, NcStorable b, FromNcAttr a, FromNcAttr b,
                                NcStore s, Real a, Fractional b, IEEE b,
                                NcStoreExtraCon s a, NcStoreExtraCon s b)
             => NcVar -> s a -> s b
mycoardsScale v = Data.NetCDF.Store.smap xform
  where (offset, scale, fill) = triplet v
        xform x = case fill of
                    Nothing -> realToFrac $ realToFrac x * scale + offset
                    Just f -> if x == f
                                then nan
                                else realToFrac $ realToFrac x * scale + offset

mycoardsScale1 :: forall a b. (NcStorable a, NcStorable b, FromNcAttr a, FromNcAttr b,
                                Real a, Fractional b, IEEE b)
                => NcVar -> a -> b
mycoardsScale1 v x = let (offset, scale, fill) = triplet v
                      in case fill of
                           Nothing -> realToFrac $ realToFrac x * scale + offset
                           Just f -> if x == f
                                       then nan
                                       else realToFrac $ realToFrac x * scale + offset

--- "add_offset" and "scale_factor" can be CFloat or CDouble
triplet :: forall a. (NcStorable a, FromNcAttr a)
                => NcVar -> (CDouble, CDouble, Maybe a)
triplet v = (offset, scale, fill)
  where offset' = ncVarAttr v "add_offset"
        offset = case offset' of
                   Just (NcAttrDouble _) -> fromMaybe 0.0 (fromAttr . fromJust $ offset')
                   Just (NcAttrFloat _) -> (realToFrac :: CFloat -> CDouble)
                                           (fromMaybe 0.0 (fromAttr . fromJust $ offset'))
                   Just _ -> 0.0
                   Nothing -> 0.0
        scale' = ncVarAttr v "scale_factor"
        scale  = case scale' of
                   Just (NcAttrDouble _) -> fromMaybe 1.0 (fromAttr . fromJust $ scale')
                   Just (NcAttrFloat _) -> (realToFrac :: CFloat -> CDouble)
                                           (fromMaybe 1.0 (fromAttr . fromJust $ scale'))
                   Just _ -> 1.0
                   Nothing -> 1.0
        fill   = ncVarAttr v "_FillValue" >>= fromAttr :: Maybe a

--
-- Copyright (c) 2013, Ian Ross
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
-- 
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
-- 
--     * Neither the name of Ian Ross nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
