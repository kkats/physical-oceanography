--
-- | CARS 2009 data
--
module Oceanogr.CARS (accessCARS, readCARSmean, readCARS)
where

import Oceanogr.GSWtools (gsw_sa_from_sp, gsw_ct_from_t)
import Oceanogr.GSW (Loc, getLon, getLat)
import Oceanogr.NCRepa
import Oceanogr.GammaN (gamma_n, neutral_surfaces)
import Oceanogr.Misc (inbetween)

import Data.Array.Repa as Repa hiding ((++))
import Data.Array.Repa.Repr.ForeignPtr
import Data.List (zip4, unzip4)
import Data.NetCDF
import qualified Data.Vector.Unboxed as V
import Foreign.C
import Numeric.IEEE (nan)

--
-- modified from Oceanogr.Misc.interp2 to retain deep data
--
interp2 :: Array U DIM1 Double
        -> Array U DIM1 Double
        -> Array U DIM2 Double
        -> (Double, Double)
        -> Double
interp2 xs' ys' zs' (xd', yd')
        -- and do not worry about Greenwich transit (i.e. fix when used in Atlantic)
    = let xs = toUnboxed xs'
          ys = toUnboxed ys'
          iHere = maybe (error "interp2") id (inbetween xs xd')
          jHere = maybe (error "interp2'") id (inbetween ys yd')

          a = (xd' - xs V.! iHere) / (xs V.! (iHere+1) - xs V.! iHere)
          b = (yd' - ys V.! jHere) / (ys V.! (jHere+1) - ys V.! jHere)

          z0 = zs' `index` ix2 jHere iHere
          z1 = zs' `index` ix2 jHere (iHere+1)
          z2 = zs' `index` ix2 (jHere+1) iHere
          z3 = zs' `index` ix2 (jHere+1) (iHere+1)

          z01 = if isNaN z0 && isNaN z1 then nan
                  else if isNaN z0 then z1
                      else if isNaN z1 then z0
                          else (1.0 - a) * z0 + a * z1
          z23 = if isNaN z2 && isNaN z3 then nan
                  else if isNaN z2 then z3
                      else if isNaN z3 then z2
                          else (1.0 - a) * z2 + a * z3
       in if isNaN z01 && isNaN z23 then nan
           else if isNaN z01 then z23
               else if isNaN z23 then z01
                   else (1.0 - b) * z01 + b * z23
       
-- | read CARS only the mean (... at the moment)
--
readCARSmean :: FilePath -> IO (Array U DIM1 Double, Array U DIM1 Double,
                                Array U DIM1 Double, Array U DIM3 Double)
readCARSmean carsfile = readCARS carsfile "mean"

readCARS :: FilePath -- ^ Either temperature_cars2009a.nc or salinity_cars2009a.nc
    -> String        -- ^ what to read
    -> IO (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double,
           Array U DIM3 Double) -- ^(lon, lat, depth, T/S)
readCARS carsfile item = do
    info' <- openFile carsfile
    case info' of
        Left err   -> error $ show err
        Right info -> do
            dep' <- getReal info "depth" :: IO (Array F DIM1 CFloat)
            lat' <- getReal info "lat" :: IO (Array F DIM1 CFloat)
            lon' <- getReal info "lon" :: IO (Array F DIM1 CFloat)
            var' <- getReal info item :: IO (Array F DIM3 CDouble)
            closeFile info

            let dep = computeUnboxedS $ Repa.map realToFrac dep'
                lat = computeUnboxedS $ Repa.map realToFrac lat'
                lon = computeUnboxedS $ Repa.map realToFrac lon'
                var = computeUnboxedS $ Repa.map realToFrac var'

            return (lon, lat, dep, var)

--
-- un-"is good"
--
-- ig = map (\x'-> if good then True else False) x
-- xf = f(ig)
-- y = unig ig xf []
--
unig :: [Bool] -- ^ flag
     -> [Double] -- ^ variable
     -> [Double] -- ^ accumulator
     -> [Double]
unig _ [] out = reverse out
unig [] (_:_) _ = error "unig"
unig (o:os) (x:xs) out = if o then unig os xs (x:out)
                              else unig os (x:xs) (nan:out)

--
-- | Conservative temperature and salinity
--   returns 2 functions to access CARS data
--
accessCARS :: (Double, Double, Double, Double) -- ^ N, E, S, W lat/lon's
        -> IO (Array U DIM1 Double, -- ^ lon
               Array U DIM1 Double, -- ^ lat
               Array U DIM1 Double, -- ^ depth
               -- gridded on isopycnals
               [Double]      -- ^ gamma levels
               -> IO (Array U DIM3 Double, -- ^ CT
                      Array U DIM3 Double, -- ^ SA
                      Array U DIM3 Double), -- ^ isopycnal depth
               -- profile at one station
               Loc Double    -- ^ location, OUT: CT profile and SA profile, gamma
               -> IO (V.Vector Double, -- ^ CT profile
                      V.Vector Double, -- ^ SA profile
                      V.Vector Double), -- ^ gamma
               -- profile exactly on grid point w/o interp2
               Loc Double    -- ^ location, OUT: CT profile and SA profile, gamma
               -> IO (V.Vector Double, -- ^ CT profile
                      V.Vector Double, -- ^ SA profile
                      V.Vector Double) -- ^ gamma
              )
accessCARS (boundN, boundE, boundS, boundW) = do

    (long', lati', dept, salt') <- readCARSmean "/local/data/CARS/salinity_cars2009a.nc"
    (_,    _,     _,     temp') <- readCARSmean "/local/data/CARS/temperature_cars2009a.nc"

    let (_:. k1 :. _ :. _) = extent salt'

        (is, lon9) = V.unzip $ V.takeWhile (\(_, o9) -> o9 <= boundE)
                   . V.dropWhile (\(_, o9) -> o9 < boundW) $ (V.indexed . toUnboxed $ long')
        long       = fromUnboxed (ix1 (V.length is)) lon9
        (js, lat9) = V.unzip $ V.takeWhile (\(_, o9) -> o9 <= boundN)
                   . V.dropWhile (\(_, o9) -> o9 < boundS) $ (V.indexed . toUnboxed $ lati')
        lati       = fromUnboxed (ix1 (V.length js)) lat9
        salt = computeUnboxedS $ extract (ix3 0 (V.head js) (V.head is))
                                         (ix3 k1 (V.length js) (V.length is)) salt'
        temp = computeUnboxedS $ extract (ix3 0 (V.head js) (V.head is))
                                         (ix3 k1 (V.length js) (V.length is)) temp'

    -- GSW_ function calls are in IO. I cannot find mapM in Repa.
    -- Let's go with V.Vector's.
        stGamma :: [Double] -- glev
                   -> Int -> IO (V.Vector Double, -- ^ gamma
                                 V.Vector Double, -- ^ depth of gamma
                                 V.Vector Double, -- ^ ct on gamma
                                 V.Vector Double) -- ^ sa on gamma
        stGamma glev i1
            = let (_:. j2 :. i2) = fromIndex (ix2 (V.length js) (V.length is)) i1
                  lonHere        = long `index` ix1 i2
                  latHere        = lati `index` ix1 j2
                  s2             = toUnboxed . computeUnboxedS
                                     $ extract (ix3 0 j2 i2) (ix3 k1 1 1) salt
                  t2             = toUnboxed . computeUnboxedS
                                     $ extract (ix3 0 j2 i2) (ix3 k1 1 1) temp
                  ig             = V.zipWith (\s' t' -> if isNaN (t' + s')
                                                          then False
                                                          else True) s2 t2
                  s3             = snd . V.unzip . V.filter fst $ V.zip ig s2
                  t3             = snd . V.unzip . V.filter fst $ V.zip ig t2
                  p3             = snd . V.unzip . V.filter fst $ V.zip ig (toUnboxed dept)
               in if V.all (not . id) ig
                    then let nans = V.replicate k1 nan
                          in return (nans, nans, nans, nans)
                    else do
                      (g3, _dglo, _dghi) <- gamma_n (V.toList s3) (V.toList t3) (V.toList p3) (V.length p3) (lonHere, latHere)


                      let (s4, t4, p4, g4) = unzip4 . filter (\(_,_,_,g') -> g' > 0) $ zip4 (V.toList s3) (V.toList t3) (V.toList p3) g3
                          g4'              = unig (V.toList ig) g3 []
                          ih               = Prelude.zipWith (\i' g' -> if i' && g' > 0 then True else False) (V.toList ig) g4' -- new flag to go with s4, t4, p4, and g4
                      (sns, tns, pns, _dsns, _dtns, _dpns) <- neutral_surfaces s4 t4 p4 g4 (length g4) glev (length glev)
                      let cleanse p' = if p' < -80 then nan else p'
                          sn4 = Prelude.map cleanse sns
                          tn4 = Prelude.map cleanse tns
                          pn4 = Prelude.map cleanse pns

                      sa4 <- Prelude.mapM (\(s',p') -> if isNaN (s'+ p')
                                                         then return nan
                                                         else gsw_sa_from_sp s' p' lonHere latHere)
                            $ zip sn4 pn4
                      ct4 <- Prelude.mapM (\(s',t',p') -> if isNaN (s'+t'+p')
                                                            then return nan
                                                            else gsw_ct_from_t s' t' p')
                            $ zip3 sa4 tn4 pn4
                      let g5  = V.fromList $ unig ih g4 []
                          g6  = g5 V.++ V.replicate (k1 - V.length g5) nan

                      return (g6, V.fromList pn4, V.fromList ct4, V.fromList sa4)

    let onGamma :: [Double]
                    -> IO (Array U DIM3 Double, Array U DIM3 Double, Array U DIM3 Double)
        onGamma gamlevs = do
            (_g7, pn7, ct7, sa7) <- unzip4 `fmap` mapM (stGamma gamlevs)
                                                [0 .. (V.length is * V.length js - 1)]

            let reform :: Int -> [V.Vector Double] -> Array U DIM3 Double
                reform kn x = computeUnboxedS
                                $ fromFunction (ix3 kn (V.length js) (V.length is))
                                $ \(Z:. k :. j :. i)
                                     -> let y = x !!  (toIndex (ix2 (V.length js) (V.length is)) (ix2 j i))
                                         in y V.! k
                pn8 = reform (length gamlevs) pn7
                ct8 = reform (length gamlevs) ct7
                sa8 = reform (length gamlevs) sa7
            -- writeVecF "work/gamma.bin" (V.map double2Float $ toUnboxed pn8)
            -- appendVecF "work/gamma.bin" (V.map double2Float $ toUnboxed ct8)
            -- appendVecF "work/gamma.bin" (V.map double2Float $ toUnboxed sa8)
            return (ct8, sa8, pn8)

        --
        prof, prof' :: Loc Double -> IO (V.Vector Double, V.Vector Double, V.Vector Double)
                            -- ^ CT, SA, gamma-n
        -- anywhere with interp2
        prof location = do
            let lo = getLon location
                la = getLat location

                (t3, s3) =  V.unzip
                         $  V.map (\k -> let temp2 = computeUnboxedS
                                                    $ slice temp (Z :. k :. All :. All)
                                             salt2 = computeUnboxedS
                                                    $ slice salt (Z :. k :. All :. All)
                                             t2 = interp2 long lati temp2 (lo, la)
                                             s2 = interp2 long lati salt2 (lo, la)
                                         in (t2, s2)) (V.fromList [0 .. (k1-1)])
            let ig = V.zipWith (\t' s' -> if isNaN (t' + s') then False else True) t3 s3
                t4 = snd . V.unzip . V.filter fst $ V.zip ig t3
                s4 = snd . V.unzip . V.filter fst $ V.zip ig s3
                p4 = snd . V.unzip . V.filter fst $ V.zip ig (toUnboxed dept)
            if (V.all not ig)
              then let nans = V.replicate k1 nan
                    in return (nans, nans, nans)
              else do
                (g4, _dl4, _dh4) <- gamma_n (V.toList s4) (V.toList t4) (V.toList p4) (V.length p4) (lo, la)
                sa4 <- V.mapM (\(s',p') -> gsw_sa_from_sp s' p' lo la) $ V.zip s4 p4
                ct4 <- V.mapM (\(s',t',p') -> gsw_ct_from_t s' t' p') $ V.zip3 sa4 t4 p4
                let ct5 = V.fromList $ unig (V.toList ig) (V.toList ct4) []
                    sa5 = V.fromList $ unig (V.toList ig) (V.toList sa4) []
                    gn5 = V.fromList $ unig (V.toList ig) g4 []
                    gn6 = V.map (\g' -> if g' < 0 then nan else g') gn5
                return (ct5, sa5, gn6)
        -- exactly on grid w/o interp2
        prof' location = do
            let lo = getLon location
                la = getLat location

                (i1, _) = V.minimumBy (\(_,a) (_,b) -> abs (a - lo) `compare` abs (b - lo))
                         (V.indexed . toUnboxed $ long)
                (j1, _) = V.minimumBy (\(_,a) (_,b) -> abs (a - la) `compare` abs (b - la))
                         (V.indexed . toUnboxed $ lati)
                t3 = toUnboxed $ computeUnboxedS $ slice temp (Z:. All :. j1 :. i1)
                s3 = toUnboxed $ computeUnboxedS $ slice salt (Z:. All :. j1 :. i1)
                ig = V.zipWith (\t' s' -> if isNaN (t' + s') then False else True) t3 s3
                t4 = snd . V.unzip . V.filter fst $ V.zip ig t3
                s4 = snd . V.unzip . V.filter fst $ V.zip ig s3
                p4 = snd . V.unzip . V.filter fst $ V.zip ig (toUnboxed dept)
            if (V.all not ig)
              then let nans = V.replicate k1 nan
                    in return (nans, nans, nans)
              else do
                (g4, _dl4, _dh4) <- gamma_n (V.toList s4) (V.toList t4) (V.toList p4) (V.length p4) (lo, la)
                sa4 <- V.mapM (\(s',p') -> gsw_sa_from_sp s' p' lo la) $ V.zip s4 p4
                ct4 <- V.mapM (\(s',t',p') -> gsw_ct_from_t s' t' p') $ V.zip3 sa4 t4 p4
                let ct5 = V.fromList $ unig (V.toList ig) (V.toList ct4) []
                    sa5 = V.fromList $ unig (V.toList ig) (V.toList sa4) []
                    gn5 = V.fromList $ unig (V.toList ig) g4 []
                    gn6 = V.map (\g' -> if g' < 0 then nan else g') gn5
                return (ct5, sa5, gn6)

    return (long, lati, dept, onGamma, prof, prof')
