{-# LANGUAGE BangPatterns, TypeFamilies, ScopedTypeVariables #-}

--
-- | Not all functions of GSW in matlab are not exported
--   to fortran, i.e., FFI is not a full solution.
--
module Oceanogr.GSW (
-- * distance calculated in both Double and Float
    LatLon, Loc(..), getLon, getLat, putLoc,
--
    gsw_distance, gsw_f,
--            module GSWtools
  
    linearInterp1, interpRRD, interp1,
    gsw_geo_strf_dyn_height
           )
where

import Oceanogr.Misc (inbetween)
import Oceanogr.GSWtools (gsw_specvol)

import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Numeric.IEEE (nan)

class Floating a => LatLon a where
    data Loc a :: *
    getLon, getLat :: Loc a -> a
    putLoc  :: a -> a -> Loc a

instance LatLon Double where
    data Loc Double = LocD {getLonD  :: !Double,  getLatD :: !Double} deriving (Eq, Show)
    getLon = getLonD
    getLat = getLatD
    putLoc = \lon lat -> LocD lon lat

instance LatLon Float where
    data Loc Float  = LocF {getLonF :: !Float,   getLatF :: !Float} deriving (Eq, Show)
    getLon = getLonF
    getLat = getLatF
    putLoc = \lon lat -> LocF lon lat

-- no depth correction
-- based on Matlab v3.4
gsw_distance :: forall a. (RealFloat a, LatLon a) => Loc a -> Loc a -> a
gsw_distance p0 p1 = earth_radius * angles
  where
    dlon = pi180 * (getLon p0 - getLon p1)
    dlat = pi180 * (getLat p0 - getLat p1)
    a = (sin $ 0.5 * dlat)^(2::Integer)
      + (cos $ getLat p0 * pi180) * (cos $ getLat p1 * pi180) * (sin $ 0.5 * dlon)^(2::Integer)
    angles = 2 * atan2 (sqrt a) (sqrt $ 1.0-a)

    pi180 = 3.14159265358979 / 180.0
    earth_radius = 6371000 -- [m]

---
gsw_f :: Floating a => a -> a
gsw_f lat = let deg2rad = pi / 180
                omega   = 7.292115e-5
             in 2 * omega * sin (lat * deg2rad)

---
-- |
--  Combination of interpRRD and interp1
--  First, apply interpRRD, second nan's are
--  filled by interp1
--
-- >>> let x = U.fromList [2020,2216,2413,2611,2878,3000] :: U.Vector Double
-- >>> let y = U.fromList [2.211,2.011,1.894,1.788,1.554,1.380] :: U.Vector Double
-- >>> let xi = U.fromList [2100,2200 .. 3000] :: U.Vector Double
-- >>> Right (fromList [2.1293673469387753,2.027326530612245,1.9570207317973867,1.9013255208670579,1.8455641980510156,1.79474057649 86781,1.7292355861090267,1.6473991803675845,1.5226229508196722,1.38])
--
--
interp1 :: U.Vector Double -- ^ bottle pressure
        -> U.Vector Double -- ^ bottle data
        -> U.Vector Double -- ^ pressure to interpolate onto
        -> IO (Either String (U.Vector Double)) -- ^ output

interp1 x y xi = do

    y0 <- interpRRD x y xi

    case y0 of
      Left e0  -> return $ Left e0
      Right y1 -> do
        let idx = U.fromList [0 .. (U.length xi - 1)] :: U.Vector Int
            gap = U.filter (\(_,_,y10) -> isNaN y10) $ U.zip3 idx xi y1
        if U.null gap
        then return $ Right y1
        else do
            let (ig, xg, _) = U.unzip3 gap
            yg <- linearInterp1 x y xg
            case yg of
              Left e1  -> return $ Left e1
              Right y2 -> let toUpdate = U.zip ig y2
                           in return $ Right (U.update y1 toUpdate)


---
-- | Interpolation of "bottle" data
--   optimized for pelagic ocean data
--   based on the method by Reinger and Ross (1968, DSR)
--   tuned by Jeff Dunn.
--
--   Matlab source is in  ..\/matlabGSW3.04\/gsw_geo_strf_dyn_height.m
--   Example below was taken from example.dat in the gamma_n package.
--   observed temperatures at xi are; 12.21,11.99,10.54,8.36,7.43,6.04
--   observed salinities   at xi are; 35.086,35.078,34.851,34.572,34.509,34.452
--
-- >>> let x = U.fromList [1,97,194,388,581,775,969] :: U.Vector Double
-- >>> let ytemp = U.fromList [12.25,12.09,11.69,9.35,7.86,6.87,5.5] :: U.Vector Double
-- >>> let ysalt = U.fromList [35.066,35.089,35.025,34.696,34.531,34.496,34.458] :: U.Vector Double
-- >>> let xi = U.fromList [48,145,291,485,678,872] :: U.Vector Double
-- >>> interpRRD x ytemp xi
-- Right (fromList [NaN,NaN,10.466597777964449,8.45058039567543,7.375825606234917,NaN])
-- >>> interpRRD x ysalt xi
-- Right (fromList [NaN,NaN,34.85724213411662,34.57730605075967,34.51419549586589,NaN])
--
interpRRD :: U.Vector Double -- ^ bottle pressure
          -> U.Vector Double -- ^ bottle data
          -> U.Vector Double -- ^ pressure to interpolate onto
          -> IO (Either String (U.Vector Double)) -- ^ output
interpRRD x y xi
    | U.length x < 4 = return $ Left "Input too short"
    | U.length x /= U.length y
                    = return $ Left "Inconsistent length"
    | not (U.all (>0) (U.zipWith (-) (U.tail x) x)
        || U.all (<0) (U.zipWith (-) (U.tail x) x)) = return $ Left "Unsorted input"
    | otherwise      = do
      let xfn = U.fromList [0, 300, 1800, 8000] :: U.Vector Double
          yfn = U.fromList [50, 200, 650, 1250] :: U.Vector Double

          interp1 :: Double -> Double
          interp1 x0 = case U.findIndex (x0<) xfn of
                        Nothing -> error $ "too deep at x=" ++ (show x0)
                        Just i0 -> (x0 - xfn U.! (i0-1)) * (yfn U.! i0 - yfn U.! (i0-1))
                                                         / (xfn U.! i0 - xfn U.! (i0-1))
                                       + yfn U.! (i0-1)
          maxDis = U.map interp1 xi
          ilen   = U.length xi
          ivec   = U.fromList [0 .. (ilen-1)] :: U.Vector Int

      yi <- UM.replicate ilen nan :: IO (UM.IOVector Double)

      let coincide :: (Double, Int) -> IO ()
          coincide (x0, j) =
            let i0 = U.minIndexBy (\a b -> (abs $ a - x0) `compare` (abs $ b - x0)) x
             in if abs (x U.! i0 - x0) < U.minimum maxDis * 0.005 -- "concid_frac"
                then UM.write yi j (y U.! i0)
                else return ()

      U.mapM_ coincide $ U.zip xi ivec

      let interp :: (Double, Double, Int) -> IO ()
          interp (y0, x0, j)
            | not . isNaN $ y0 = return () -- already assigned
            | otherwise        = case inbetween x x0 of
                        Nothing -> return () -- not within the range
                        Just i0 -> if i0 < 1 || i0 > U.length x - 3
                                   then return () -- not within the range
                                   else do
                                   let sumin = abs $ x U.! i0 - x U.! (i0+1)
                                       outl  = abs $ x U.! (i0-1) - x U.! i0
                                       outr  = abs $ x U.! (i0+2) - x U.! (i0+1)
                                       md    = maxDis U.! j
                                   if sumin > md || outl+sumin+outr > md*3
                                        || min outr outl < sumin / 15.0
                                   -- sumin>maxdis(tdix,1) | outtest>maxdis(tidx,2) |
                                   --      minsep<cfrac
                                   then return ()
                                   else let intp = y U.! i0 + (x0 - x U.! i0)
                                                       * (y U.! (i0+1) - y U.! i0)
                                                       / (x U.! (i0+1) - x U.! i0)
                                            extl = y U.! i0 + (x0 - x U.! i0)
                                                       * (y U.! i0 - y U.! (i0-1))
                                                       / (x U.! i0 - x U.! (i0-1))
                                            extr = y U.! (i0+1) + (x0 - x U.! (i0+1))
                                                       * (y U.! (i0+2) - y U.! (i0+1))
                                                       / (x U.! (i0+2) - x U.! (i0+1))
                                            deno = (abs $ extl - intp) ** 1.7
                                                 + (abs $ intp - extr) ** 1.7
                                            nume = (abs $ extl - intp) ** 1.7 * extr
                                                 + (abs $ intp - extr) ** 1.7 * extl
                                            y0'' = if abs deno < 1.0e-4
                                                   then intp
                                                   else (intp + nume / deno) * 0.5
                                            -- do not overshoot
                                            y0'  = max y0'' $ min (y U.! i0) (y U.! (i0+1))
                                            y0   = min y0'  $ max (y U.! i0) (y U.! (i0+1))
                                         in UM.write yi j y0
      yi' <- U.freeze yi
      U.mapM_ interp $ U.zip3 yi' xi ivec
      Right `fmap` (U.unsafeFreeze yi)

--
-- | Simple linear interpolation
--
-- >>> let x = U.fromList [1,97,194,388,581,775,969]               :: U.Vector Double
-- >>> let y = U.fromList [12.25,12.09,11.69,9.35,7.86,6.87,5.5]  :: U.Vector Double
-- >>> let xi = U.fromList [48,145,291,485,678,872]              :: U.Vector Double
-- >>> interp1 x y xi
-- Right (fromList [12.171666666666667,11.892061855670104,10.52,8.601139896373057,7.365,6.1850000000000005])
--
linearInterp1 :: U.Vector Double -- ^ bottle pressure
        -> U.Vector Double -- ^ bottle data
        -> U.Vector Double -- ^ pressure to interpolate onto
        -> IO (Either String (U.Vector Double)) -- ^ output
linearInterp1 x y xi
    | U.length x < 2 = return $ Left "Input too short"
    | U.length x /= U.length y
                    = return $ Left "Inconsistent length"
    | not (U.all (>0) (U.zipWith (-) (U.tail x) x)
        || U.all (<0) (U.zipWith (-) (U.tail x) x)) = return $ Left "Unsorted input"
    | otherwise      = do
        yi <- UM.replicate (U.length xi) nan :: IO (UM.IOVector Double)
        let interp :: (Double, Int) -> IO ()
            interp (x0, j) = case inbetween x x0 of
                        Nothing -> return () -- not within the range
                        Just i0 -> UM.write yi j $ y U.! i0 + (x0 - x U.! i0)
                                                    * (y U.! (i0+1) - y U.! i0)
                                                    / (x U.! (i0+1) - x U.! i0)
        U.mapM_ interp $ U.zip xi (U.fromList [0 .. (U.length xi - 1)])
        Right `fmap` (U.unsafeFreeze yi)



--
-- %==========================================================================
-- %  This function calculates enthalpy at the Standard Ocean Salinity, SSO, 
-- %  and at a Conservative Temperature of zero degrees C, as a function of
-- %  pressure, p, in dbar, using a streamlined version of the 48-term CT 
-- %  version of the Gibbs function, that is, a streamlined version of the 
-- %  code "gsw_enthalpy(SA,CT,p)".
-- %
-- % VERSION NUMBER: 3.04 (10th December, 2013)
--
-- |
-- gsw_enthalpy_SSO_0_p
-- ported from gsw_enthalpy_SSO_0_p.m in GSW3.04
--
--
-- >>> map gsw_enthalpy_SSO_0_p [100,1000,1500,2500]
-- [972.4363273245723,9704.322969032506,14540.028654210344,24179.386014150285]
--
gsw_enthalpy_SSO_0_p :: Double -> Double
gsw_enthalpy_SSO_0_p p =
    let db2Pa = 1.0e4
        sso   = 35.16504
        v01 =  9.998420897506056e+2
        v05 =(-6.698001071123802)
        v08 =(-3.988822378968490e-2)
        v12 =(-2.233269627352527e-2)
        v15 =(-1.806789763745328e-4)
        v17 =(-3.087032500374211e-7)
        v20 =  1.550932729220080e-10
        v21 =  1.0
        v26 =(-7.521448093615448e-3)
        v31 =(-3.303308871386421e-5)
        v36 =  5.419326551148740e-6
        v37 =(-2.742185394906099e-5)
        v41 =(-1.105097577149576e-7)
        v43 =(-1.119011592875110e-10)
        v47 =(-1.200507748551599e-15)
        a0 = v21 + sso*(v26 + v36*sso + v31*sqrt sso)
        a1 = v37 + v41*sso
        a2 = v43
        a3 = v47
        b0 = v01 + sso*(v05 + v08*sqrt sso)
        b1 = 0.5*(v12 + v15*sso)
        b2 = v17 + v20*sso
        b1sq = b1*b1
        sqrt_disc = sqrt (b1sq - b0*b2)
        nn = a0 + (2*a3*b0*b1/b2 - a2*b0)/b2
        mm = a1 + (4*a3*b1sq/b2 - a3*b0 - 2*a2*b1)/b2
        aa = b1 - sqrt_disc;
        bb = b1 + sqrt_disc;
        part = (nn*b2 - mm*b1)/(b2*(bb - aa))
     in db2Pa*(p*(a2 - 2*a3*b1/b2 + 0.5*a3*p)/b2 + 
          (mm/(2*b2))*log (1 + p*(2*b1 + b2*p)/b0) +
           part*log (1 + (b2*p*(bb - aa))/(aa*(bb + b2*p))))

--
-- % gsw_geo_strf_dyn_height                            dynamic height anomaly
-- %                                                        (48-term equation)
-- %==========================================================================
-- %
-- % DESCRIPTION:
-- %  Calculates dynamic height anomaly as the integral of specific volume
-- %  anomaly from the pressure p of the bottle to the reference pressure
-- %  p_ref.
-- %
-- %  Hence, geo_strf_dyn_height is the dynamic height anomaly with respect
-- %  to a given reference pressure.  This is the geostrophic streamfunction 
-- %  for the difference between the horizontal velocity at the pressure 
-- %  concerned, p, and the horizontal velocity at p_ref.  Dynamic height 
-- %  anomaly is the geostrophic streamfunction in an isobaric surface.  The 
-- %  reference values used for the specific volume anomaly are 
-- %  SSO = 35.16504 g/kg and CT = 0 deg C.  This function calculates 
-- %  specific volume anomaly using the computationally efficient 48-term 
-- %  expression for specific volume of IOC et al. (2010). 
--                    :
-- % INPUT:
-- %  SA    =  Absolute Salinity                                      [ g/kg ]
-- %  CT    =  Conservative Temperature (ITS-90)                     [ deg C ]
-- %  p     =  sea pressure                                           [ dbar ]
-- %           ( i.e. absolute pressure - 10.1325 dbar )
-- %  p_ref =  reference pressure                                     [ dbar ]
-- %           ( i.e. reference absolute pressure - 10.1325 dbar )
--                    :
-- % OUTPUT:
-- %  geo_strf_dyn_height  =  dynamic height anomaly               [ m^2/s^2 ]
-- %   Note. If p_ref exceeds the pressure of the deepest bottle on a 
-- %     vertical profile, the dynamic height anomaly for each bottle 
-- %     on the whole vertical profile is returned as NaN.
--                    :
-- % AUTHOR:  
-- %  Paul Barker, Jeff Dunn and Trevor McDougall         [ help@teos-10.org ]
-- %
-- % VERSION NUMBER: 3.04 (10th December, 2013)
-- %
-- % REFERENCES:
-- %  IOC, SCOR and IAPSO, 2010: The international thermodynamic equation of 
-- %   seawater - 2010: Calculation and use of thermodynamic properties.  
-- %   Intergovernmental Oceanographic Commission, Manuals and Guides No. 56,
-- %   UNESCO (English), 196 pp.  Available from http://www.TEOS-10.org
-- %    See Eqn. (3.7.3) and section 3.27 of this TEOS-10 Manual. 
--
-- |
--  Dynamic height anomaly.
--  Interpolation is not implemented. This function therefore
--  expects input in Vector interpolated between p & p_ref at
--  an equal interval.
--
gsw_geo_strf_dyn_height :: U.Vector Double -- ^ Absolute Salinity in [g/kg]
                        -> U.Vector Double -- ^ Conservative Temperature (ITS-90) in [deg C]
                        -> U.Vector Double -- ^ sea pressure in [dbar]
                        -> Double   -- ^ reference pressure in [dbar]
                        -> IO (Either String (U.Vector Double)) -- ^ dynamic height anomaly in [m2/s2]
gsw_geo_strf_dyn_height sa ct p pref
    | not (U.length sa == U.length ct && U.length ct == U.length p)
        = return $ Left "Input size inconsistent"
    | U.any isNaN sa || U.any isNaN ct || U.any isNaN p || isNaN pref
        = return $ Left "NaN found"
    | U.minimumBy (comparing abs) (U.map (subtract pref) p) > 1.0e-3
        = return $ Left "No data at pref"
    | U.any (\p0 -> (abs $ p0 - (p U.! 1 - p U.! 0)) > 1.0e-6) $ U.zipWith (-) (U.tail p) p
         = return $ Left "Uneven pressure grid"
    | otherwise
        = do
        bb <- U.mapM (\(sa0, ct0, p0) -> gsw_specvol sa0 ct0 p0) $ U.zip3 sa ct p

        let cc  = U.map gsw_enthalpy_SSO_0_p p
            bav = U.zipWith (\a b -> 0.5 * (a + b)) bb (U.tail bb)
            dp  = p U.! 1 - p U.! 0
            dd  = U.map (* (dp * 1.0e4)) bav
            gsdh0 = U.zipWith (-) cc (U.scanl' (+) 0 dd)
            gsdhpref = case U.findIndex (\p0 -> (abs $ p0 - pref) <= 1.0e-3) p of
                        Nothing -> error "impossible"
                        Just i0 -> gsdh0 U.! i0
        return $ Right (U.map (subtract gsdhpref) gsdh0)
