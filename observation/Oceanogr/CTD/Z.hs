-- |
-- Vertical interpolations for CTD data
--
module Oceanogr.CTD.Z (
zOnP, zOnG, zOnSigma1, zOnSigma2, zOnSigma4,    -- ^ interpolated
zOnP', gOnP', sigma1OnP', sigma2OnP', sigma4OnP' -- ^ uninterpolated (i.e. raw output)
) where

import Oceanogr.GammaN (gamma_n, neutral_surfaces)
import Oceanogr.GSW (linearInterp1)
import Oceanogr.GSWtools (gsw_ct_from_t, gsw_sigma1, gsw_sa_from_sp, gsw_sigma2, gsw_sigma4)

import Oceanogr.CTD

import Control.Monad (when)
import qualified Data.Vector.Unboxed as V
import GHC.Float (double2Float, float2Double)
import Numeric.IEEE (nan)

zOnP' :: CTDitem -- ^ y
      -> CTDdata -- ^ source
      -> (V.Vector Float, V.Vector Float) -- ^ (pressure and y)
zOnP' item ctd
    = let p0 = ctdP ctd
          y0 = case item of
                P  -> let y = ctdP ctd
                       in if V.null y then error "P empty" else y
                CT -> let y = ctdCT ctd
                       in if V.null y then error "CT empty" else y
                SA -> let y = ctdSA ctd
                       in if V.null y then error "SA empty" else y
                DO -> let y = ctdDO ctd
                       in if V.null y then error "DO empty" else y
                --
                T  -> let y = ctdT ctd
                       in if V.null y then error "T empty" else y
                S  -> let y = ctdS ctd
                       in if V.null y then error "S empty" else y
                --
                -- _  -> error "zOnP': not implemented"
        in (p0, y0)
---
--- interpolated onto pressure grid
---
zOnP :: CTDitem -- ^ y
     -> CTDdata -- ^ source
     -> V.Vector Float -- ^ pressure
     -> V.Vector Float -- ^ y on pressure
zOnP item ctd p
   | V.length p < 2 = error "zOnP: p too short"
   | V.any (/= (p V.! 1) - V.head p) (V.zipWith (-) (V.tail p) p) = error "zOnP: non-regular p"
   | otherwise = let dp2 = (p V.! 1 - V.head p) * 0.5
                     (p0, y0)  = zOnP' item ctd
                     -- if NaN included -> output is NaN
                     av :: V.Vector Int -> Float
                     av ig = if V.null ig then nan
                                          else V.sum (V.map (y0 V.!) ig)
                                                        / fromIntegral (V.length ig)
                  in V.map (\q -> av $ V.findIndices (\r -> q - dp2 < r && r <= q + dp2) p0) p
---
--- density grid
---

-- uninterpolated
gOnP', sigma1OnP', sigma2OnP', sigma4OnP' ::
    CTDdata -- ^ source
 -> IO (V.Vector Float, V.Vector Float) -- ^ pressure and dens

-- interpolated
zOnG, zOnSigma1, zOnSigma2, zOnSigma4 ::
    CTDitem -- ^ y
 -> CTDdata -- ^ source
 -> V.Vector Float -- ^ vertical grid
 -> IO (V.Vector Float) -- ^ y on grid

-- On neutral surface gamma-n
isbadgamma :: Double -> Bool
isbadgamma x = abs (x + 99) < 0.001

cleanse :: [Double] -> V.Vector Float
cleanse = V.map double2Float . V.map (\x -> if isbadgamma x then nan else x)
                             . V.fromList

cleansen :: [Double] -> [Double] -> V.Vector Float
cleansen x dx
    = V.map double2Float
    . V.map (\(x',dx') -> if isbadgamma x' || abs dx' > 0.001 then nan else x')
    $ V.zip (V.fromList x) (V.fromList dx)

-- internal worker
gOnP_ :: CTDdata -> IO ([Double], [Double], [Double], [Double], [Double]) -- (p,g,t,s,o)
gOnP_ ctd = do
    let CTDdata stn p' t' s' o' _ _ = ctd
        here         = (float2Double $ stnLongitude stn,
                        float2Double $ stnLatitude stn)
        (p, t, s, o) = V.unzip4
                     $ V.filter (\(p9,t9,s9,_) -> (not. isNaN $ p9 + t9 + s9))--no DO
                     $ V.zip4 p' t' s' o'
        phere        = V.toList . V.map float2Double $ p
        there        = V.toList . V.map float2Double $ t
        shere        = V.toList . V.map float2Double $ s
        ohere        = V.toList . V.map float2Double $ o
    (ghere, _dg_lo, _dg_hi) <- gamma_n shere there phere (V.length s) here
    return (phere, ghere, there, shere, ohere)

-- uninterpolated
gOnP' ctd = gOnP_ ctd >>= \(phere, ghere, _, _, _)
                         -> return (V.map double2Float . V.fromList $ phere, cleanse ghere)

-- interpolated
zOnG item ctd g = do
    (phere, ghere, there, shere, ohere) <- gOnP_ ctd
    let g' = V.toList . V.map float2Double $ g
        n  = Prelude.length shere
    (sns', tns', pns', dsns', dtns', dpns')
                    <- neutral_surfaces shere there phere ghere n g' (V.length g)
    let sns = cleansen sns' dsns'
        tns = cleansen tns' dtns'
        pns = cleansen pns' dpns'

    case item of
        DO -> do
            ons' <- linearInterp1 (V.fromList phere) (V.fromList ohere) (V.fromList pns')
            case ons' of
                Left err -> error $ "zOnG: "++err
                Right ons -> return . cleanse . V.toList $ ons

        P  -> return pns
        --- calculate CT & SA from in situ (S,T,P)
        _  -> let here = (float2Double . stnLongitude . ctdStation $ ctd,
                          float2Double . stnLatitude  . ctdStation $ ctd)
                  conv p3 t3 s3
                      | isNaN (p3 + t3 + s3) = return (nan, nan)
                      | otherwise            = do
                            sa3 <- uncurry (gsw_sa_from_sp (float2Double s3) (float2Double p3))
                                           here
                            ct3 <- gsw_ct_from_t sa3 (float2Double t3) (float2Double p3)
                            return (double2Float ct3, double2Float sa3)
               in V.unzip `fmap` V.mapM (\(p4,t4,s4) -> conv p4 t4 s4) (V.zip3 pns tns sns)
                    >>= \(ct, sa) -> case item of
                                         CT -> return ct
                                         SA -> return sa
                                         _  -> error "zOnG: not implemented"

-- sigmaN using TEOS-10
sigma1OnP' = sigmaNOnP' gsw_sigma1
sigma2OnP' = sigmaNOnP' gsw_sigma2
sigma4OnP' = sigmaNOnP' gsw_sigma4

sigmaNOnP' :: (Double -> Double -> IO Double)
            -> CTDdata
            -> IO (V.Vector Float, V.Vector Float)
sigmaNOnP' fdens ctd =
        let ct = ctdCT ctd
            sa = ctdSA ctd
            p  = ctdP  ctd
         in V.mapM (uncurry fdens) (V.zip (V.map float2Double sa) (V.map float2Double ct))
                        >>= \sigma -> return (p, V.map double2Float sigma)

zOnSigma1 = zOnSigmaN gsw_sigma1
zOnSigma2 = zOnSigmaN gsw_sigma2
zOnSigma4 = zOnSigmaN gsw_sigma4

zOnSigmaN :: (Double -> Double -> IO Double)
        -> CTDitem
        -> CTDdata
        -> V.Vector Float
        -> IO (V.Vector Float)
zOnSigmaN fdens item ctd g = do
    (_p, sigma) <- sigmaNOnP' fdens ctd

    let z = case item of
            P  -> ctdP ctd
            CT -> ctdCT ctd
            SA -> ctdSA ctd
            DO -> ctdDO ctd
            _  -> error "zOnSigmaN: not implemented"
    -- count inversions
        dsigma = V.zipWith (-) (V.tail sigma) sigma

    when (V.any (<= 0) dsigma) $ error ("sigmaNOnP: "
                                         ++ show (V.length $ V.filter (<= 0) dsigma)
                                         ++ " inversions out of "
                                         ++ show (V.length dsigma)
                                         ++ " were found")

    si1 <- linearInterp1 (V.map float2Double sigma)
                         (V.map float2Double z)
                         (V.map float2Double g)
    case si1 of
        Left err -> error $ "sigmaNOnP: " ++ err
        Right si1' -> return (V.map double2Float si1')
