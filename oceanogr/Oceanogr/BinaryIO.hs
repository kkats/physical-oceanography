{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
---- $Id: BinaryIO.hs,v 1.1 2015/01/05 04:48:43 ka Exp ka $ --
-- 
-- | With no reasons, I prefer Big Endian to Little Endian.
--   For interfacing with GrADS, float (4 byte) for floating point numbers.
--   Int32 (4 byte) is used for integral numbers.
--
module Oceanogr.BinaryIO (readVecF, readVecI16le, writeVecF, appendVecF)
where

import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.Conduit                 (runConduit, (.|), ConduitT)
import Data.Conduit.Binary          (sourceFile, sinkIOHandle)
import Data.Conduit.Combinators     (sinkVector, yieldMany, sinkFile)
import Data.Conduit.Cereal          (conduitGet2, conduitPut)

import Data.Serialize.Get (getInt16le, getInt32be)
import Data.Serialize.IEEE754   (getFloat32be, putFloat32be)
import qualified Data.Vector.Unboxed as V

import qualified Data.ByteString as B
import qualified System.IO as IO

-- sinkVector :: (MonadBase base m, Vector v a, PrimMonad base)  => forall o. ConduitM a o m (v a)
--              instance PrimMonad IO
--              instance MonadBase IO IO, instance MonadBase IO (ResourceT IO)
--              instance Vector Vector Float
-- conduitGet2 :: MonadThrow m => Get o -> ConduitM ByteString o m ()
--              instance MonadThrow IO
-- sourceFile :: MonadResource m => FilePath -> forall i. ConduitM i ByteString m ()
--              instance (MonadThrow m, MonadBase IO m, MonadIO m, Applicative m)
--                                                              => MonadResource (ResourceT m)
-- runConduit :: Monad m => ConduitM () Void m r -> m r
-- runResourceT :: MonadBaseControl IO m => ResourceT m a -> m a
--
readVecF :: FilePath -> IO (V.Vector Float)
readVecF fname = let go = sourceFile fname .| conduitGet2 getFloat32be .| sinkVector
                    -- go :: ConduitM () Void (ResourceT IO) (Vector Float)
                  in runResourceT $ runConduit go

readVecI :: FilePath -> IO (V.Vector Int)
readVecI fname = do
        ivec <- runResourceT $ runConduit $ sourceFile fname
                                                .| conduitGet2 getInt32be
                                                .| sinkVector
        return (V.map fromIntegral ivec)


readVecI16le :: FilePath -> IO (V.Vector Double)
readVecI16le fname = do
        ivec <- runResourceT $ runConduit $ sourceFile fname
                                                .| conduitGet2 getInt16le
                                                .| sinkVector
        return (V.map fromIntegral ivec)

-- yieldMany :: (Monad m, MonoFoldable mono) => mono -> forall i. ConduitM i (Element mono) m
--              instance MonoFoldable (Vector a)
--              type family Element mono; type instance Element (Vector a) = a
-- conduitPut :: Monad m => Putter a -> ConduitM a ByteString m
-- type Putter a = a -> Put
-- putFloat32be :: Float -> Put
-- sinkFile :: MonadResource m => FilePath -> forall o. ComduitM ByteString o m ()
--
writeVecF :: FilePath -> V.Vector Float -> IO ()
writeVecF fname v = let go = yieldMany v .| conduitPut putFloat32be .| sinkFile fname
                     in runResourceT $ runConduit go

-- slight modification of Data.Conduit.Binary.sinkFile
sinkFileByAppend :: MonadResource m => FilePath -> forall o. ConduitT B.ByteString o m ()
sinkFileByAppend fname = sinkIOHandle (IO.openBinaryFile fname IO.AppendMode)

appendVecF :: FilePath -> V.Vector Float -> IO ()
appendVecF fname  v = let go = yieldMany v .| conduitPut putFloat32be .| sinkFileByAppend fname
                       in runResourceT $ runConduit go

-- obsolete
-- readMatF :: forall sh. (Shape sh) => FilePath -> sh -> IO (Array F sh Float)
-- writeMatF :: forall sh. (Shape sh) => FilePath -> Array U sh Float -> IO ()
