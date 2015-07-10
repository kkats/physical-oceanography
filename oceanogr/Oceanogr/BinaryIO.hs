{-# LANGUAGE ScopedTypeVariables #-}
---- $Id: BinaryIO.hs,v 1.1 2015/01/05 04:48:43 ka Exp ka $ --
-- 
-- | With no reasons, I prefer Big Endian to Little Endian.
--   For interfacing with GrADS, float (4 byte) for floating point numbers.
--   Int32 (4 byte) is used for integral numbers.
--
module Oceanogr.BinaryIO (
    -- * Vectors
    readVecF, readVecI, writeVecF, appendVecF,
    -- * Repa matrices
    readMatF, writeMatF)
where
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector.Unboxed as U

import Data.Array.Repa
import Data.Array.Repa.IO.Binary
import Data.Array.Repa.Repr.ForeignPtr

import Foreign.Storable (sizeOf, peek, poke)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree, mallocBytes)
import Foreign.Ptr (plusPtr, Ptr)
import Data.Endian (toBigEndian)
-- import System.Endian (toBE32)

import Control.Applicative ((<$>), (<*>))
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.Int
import Data.Word (Word32)
-- import Foreign.C (CFloat)
import System.IO

-- | read until EOF
readVecF :: FilePath -> IO (U.Vector Float)
readVecF fname = do
    buf <- B.readFile fname
    return $ U.fromList $ runGet (parser getFloat32be) buf

readVecI :: FilePath -> IO (U.Vector Int32)
readVecI fname = do
    buf <- B.readFile fname
    return $ U.fromList $ runGet (parser getInt32be) buf

getInt32be :: Get Int32
getInt32be = getWord32be >>= \x -> return (fromIntegral x::Int32)

parser :: Get a -> Get [a] -- read until EOF
parser getf = do
    ie <- isEmpty
    if ie then return []
          else (:) <$> getf <*> (parser getf)

-- | write a vector
writeVecF :: FilePath -> U.Vector Float -> IO ()
writeVecF fname v = do
    B.writeFile fname $ runPut (mapM_ putFloat32be (U.toList v)) -- float
    -- B.writeFile fname $ runPut (mapM_ putFloat64be (U.toList v)) -- double
    return ()

appendVecF :: FilePath -> U.Vector Float -> IO ()
appendVecF fname v = do
    B.appendFile fname $ runPut (mapM_ putFloat32be (U.toList v))
    return ()

-- | read a Repa matrix in BigEndian
readMatF :: forall sh. (Shape sh) => FilePath -> sh -> IO (Array F sh Float)
readMatF fname sh =
    if sizeOf (undefined :: Word32) /= sizeOf (undefined :: Float)
      then do
        hPutStrLn stderr "readMatF :: cannot convert Word32 <-> Float -- using native endian" 
        readArrayFromStorableFile fname sh
      else do
        src <- readArrayFromStorableFile fname sh :: IO (Array F sh Word32)

        let ptr    = toForeignPtr src

        withForeignPtr ptr (\p -> inPlaceEndianSwap p (size sh))
        return $ fromForeignPtr sh (castForeignPtr ptr :: ForeignPtr Float)

--- | write a Repa matrix in BigEndian
writeMatF :: forall sh. (Shape sh) => FilePath -> Array U sh Float -> IO ()
writeMatF fname arr =
    if sizeOf (undefined :: Word32) /= sizeOf (undefined :: Float)
      then do
        hPutStrLn stderr "write :: cannot convert Word32 <-> Float -- using native endian" 
        writeArrayToStorableFile fname arr
      else do

        -- after Data.Array.Repa.IO.Binary.writeArrayToStorableFile
        let bytes1      = sizeOf (arr ! zeroDim)
        let bytesTotal  = bytes1 * (size $ extent arr)
        
        buf  <- mallocBytes bytesTotal :: IO (Ptr Float)
        fptr <- newForeignPtr finalizerFree buf        
        computeIntoP fptr (delay arr)

        withForeignPtr (castForeignPtr fptr :: ForeignPtr Word32)
                         (\p -> inPlaceEndianSwap p (size $ extent arr))
        
        h <- openBinaryFile fname WriteMode
        hPutBuf h buf bytesTotal 
        hClose h


-- after http://d.hatena.ne.jp/kazu-yamamoto/20131225/1387938629
inPlaceEndianSwap :: Ptr Word32
                   -> Int  -- ^ #  of Floats, NOT # of bytes
                   -> IO ()
inPlaceEndianSwap _   0 = return ()
inPlaceEndianSwap ptr n = do
    f <- peek ptr
    poke ptr (toBigEndian f)

    inPlaceEndianSwap (ptr `plusPtr` sizeOf (undefined :: Word32)) (n - 1)
                -- function binding is stronger than backticked infix function
