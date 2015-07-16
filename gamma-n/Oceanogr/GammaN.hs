{-# LANGUAGE ForeignFunctionInterface #-}
--
module Oceanogr.GammaN (
            gamma_n,
            neutral_surfaces,
           )
where

import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (mallocArray, withArray, peekArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
---
--- neutral surface
---
gamma_n :: [Double] -- salinity
        -> [Double] -- temperature
        -> [Double] -- pressure
        -> Int      -- n
        -> (Double, Double) -- (lon, lat)
        -> IO ([Double], [Double], [Double]) -- (gamma, dg_lo, dg_hi)
gamma_n s t p n (lon, lat) = do

    gamm <- mallocArray n :: IO (Ptr CDouble)
    dglo <- mallocArray n :: IO (Ptr CDouble)
    dghi <- mallocArray n :: IO (Ptr CDouble)

    withArray (cdbl s) (\sp ->
        withArray (cdbl t) (\tp ->
            withArray (cdbl p) (\pp ->
                alloca (\np -> poke np (fromIntegral n) >>
                    alloca (\lonp -> poke lonp (cdbl' lon) >>
                        alloca (\latp -> poke latp (cdbl' lat) >>
                             gamma_n_ sp tp pp np lonp latp gamm dglo dghi
                                ))))))

    gamm' <- dbl `fmap` peekArray n gamm
    dglo' <- dbl `fmap` peekArray n dglo
    dghi' <- dbl `fmap` peekArray n dghi
    free gamm >> free dglo >> free dghi
    return (gamm', dglo', dghi')

cdbl' :: Double -> CDouble
cdbl' = realToFrac
cdbl :: [Double] -> [CDouble]
cdbl = map cdbl'
dbl :: [CDouble] -> [Double]
dbl = map realToFrac

neutral_surfaces :: [Double] -- salinity
                 -> [Double] -- temperature
                 -> [Double] -- pressure
                 -> [Double] -- gamma
                 -> Int      -- n
                 -> [Double] -- gleves
                 -> Int      -- ng
                 -> IO ([Double], [Double], [Double], [Double], [Double], [Double]) -- sns,tns,pns,dsns,dtns,dpns
neutral_surfaces s t p g n glev ng = do

    sns <- mallocArray ng :: IO (Ptr CDouble)
    tns <- mallocArray ng :: IO (Ptr CDouble)
    pns <- mallocArray ng :: IO (Ptr CDouble)
    dsns <- mallocArray ng :: IO (Ptr CDouble)
    dtns <- mallocArray ng :: IO (Ptr CDouble)
    dpns <- mallocArray ng :: IO (Ptr CDouble)

    withArray (cdbl s) (\sp ->
        withArray (cdbl t) (\tp ->
            withArray (cdbl p) (\pp ->
                withArray (cdbl g) (\gp ->
                    alloca (\np -> poke np (fromIntegral n) >>
                        withArray (cdbl glev) (\glevp ->
                            alloca (\ngp -> poke ngp (fromIntegral ng) >>
                                neutral_surfaces_ sp tp pp gp np glevp ngp sns tns pns dsns dtns dpns
                                    )))))))
    sns' <- dbl `fmap` peekArray ng sns
    tns' <- dbl `fmap` peekArray ng tns
    pns' <- dbl `fmap` peekArray ng pns
    dsns' <- dbl `fmap` peekArray ng dsns
    dtns' <- dbl `fmap` peekArray ng dtns
    dpns' <- dbl `fmap` peekArray ng dpns
    free sns >> free tns >> free pns >> free dsns >> free dtns >> free dpns
    return (sns', tns', pns', dsns', dtns', dpns')


foreign import ccall "gamma_n_" gamma_n_ :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr Int -> Ptr CDouble -> Ptr CDouble
                                         -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign import ccall "neutral_surfaces_" neutral_surfaces_ :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
                                                           -> Ptr CInt -> Ptr CDouble -> Ptr CInt
                                                           -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble
                                                           -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
