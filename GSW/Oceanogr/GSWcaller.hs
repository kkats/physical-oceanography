module Oceanogr.GSWcaller (with4Args, with3Args, with2Args, with1Args) where

import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)

--
with4Args :: [CDouble] -> [Ptr CDouble]
          -> (Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble)
          -> IO CDouble
with4Args []     bs f = f (bs!!3) (bs!!2) (bs!!1) (bs!!0)
with4Args (a:as) bs f = alloca $ \xp -> poke xp a >> with4Args as (xp:bs) f

--
with3Args :: [CDouble] -> [Ptr CDouble]
          -> (Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble)
          -> IO CDouble
with3Args []     bs f = f (bs!!2) (bs!!1) (bs!!0)
with3Args (a:as) bs f = alloca $ \xp -> poke xp a >> with3Args as (xp:bs) f

--                                   
with2Args :: [CDouble] -> [Ptr CDouble]
          -> (Ptr CDouble -> Ptr CDouble -> IO CDouble)
          -> IO CDouble
with2Args []     bs f = f (bs!!1) (bs!!0)
with2Args (a:as) bs f = alloca $ \xp -> poke xp a >> with2Args as (xp:bs) f

-- must be plural (with1Arg's')
with1Args :: [CDouble] -> [Ptr CDouble]
          -> (Ptr CDouble -> IO CDouble)
          -> IO CDouble
with1Args []     bs f = f (bs!!0)
with1Args (a:as) bs f = alloca $ \xp -> poke xp a >> with1Args as (xp:bs) f
