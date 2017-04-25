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
    -- * Repa matrices -- no longer supported
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

parser :: Get a -> Get [a] -- read until EOF
parser getf = do
    ie <- isEmpty
    if ie then return []
          else (:) <$> getf <*> parser getf

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
readMatF fname sh = hPutStrLn stderr "readMatF is no longer supported. Rewrite to use readVecF instead\n"
                 >> readArrayFromStorableFile fname sh

--- | write a Repa matrix in BigEndian
writeMatF :: forall sh. (Shape sh) => FilePath -> Array U sh Float -> IO ()
writeMatF fname arr = hPutStrLn stderr "writeMatF is no longer supported. Rewrite to use writeVecF instead\n"
