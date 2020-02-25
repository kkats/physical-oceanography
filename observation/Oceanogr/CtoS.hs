{-# OPTIONS_GHC -Wno-type-defaults #-}
module Oceanogr.CtoS (ctos)
--
-- Conductivity to practical salinity according to 海洋観測指針
--
-- temperature is in IPTS-68
--
-- R = C(S,t,p)/C(35,15,0) = Rp * rt * Rt                                                        (5.4)
-- which gives Rt = C(S,t,0) /  C(35,15,0)
-- once Rt is known, use
--
where
--    Reference 35 psu = C=42.896 mS/cm at 15 degC and 0 dbar for XCTD
cXCTD :: Double
cXCTD = 42.896

ctos :: Double -- ^ pressure in 10^5 Pa
     -> Double -- ^ temperature in degC
     -> Double -- ^ conductivity in mS/cm
     -> Double -- ^ salinity in PSU
ctos p t c = let r'   = c / cXCTD
                 crp' = crp p t r'
                 rt'  = rt t
                 crt  = r' / (crp' * rt')
                 deltaS' = deltaS t crt
              in a0 + a1 * crt**0.5 + a2 * crt + a3 * crt**1.5 + a4 * crt^2 + a5 * crt**2.5 + deltaS'      -- (5.2)
  where
    a0 = 0.0080
    a1 = (-0.1692)
    a2 = 25.3851
    a3 = 14.0941
    a4 = (-7.0261)
    a5 = 2.7081

crp :: Double -- ^ pressure in 10^5 Pa
    -> Double -- ^ temperature in degC
    -> Double -- ^ C(S,t,p) / C(35,15,0)
    -> Double -- ^ Rp (capital rp)
crp p t r = 1 + p * (e1 + e2 * p + e3 * p^2) / (1 + d1 * t + d2 * t^2 + (d3 + d4 * t) * r)     -- (5.5)
  where
    e1 = 2.070e-4
    e2 = (-6.370e-8)
    e3 = 3.989e-12
    d1 = 3.426e-2
    d2 = 4.464e-4
    d3 = 4.215e-1
    d4 = (-3.107e-3)
rt :: Double -- ^ temperature in degC
   -> Double -- ^ rt
rt t = c0 + c1 * t + c2 * t^2 + c3 * t^3 + c4 * t^4                                           -- (5.6)
  where
    c0 = 0.6766097
    c1 = 2.00564e-2
    c2 = 1.104259e-4
    c3 = (-6.9698e-7)
    c4 = 1.0031e-9
deltaS :: Double -- ^ temperature in degC
       -> Double -- ^ Rt
       -> Double -- ^ deltaS
deltaS t rt' = (t - 15) / (1 + k * (t - 15))
         * (b0 + b1 * rt'**0.5 + b2 * rt' + b3 * rt'**1.5 + b4 * rt'^2 + b5 * rt'**2.5)       -- (5.3)
  where
    b0 = 0.0005
    b1 = (-0.0056)
    b2 = (-0.0066)
    b3 = (-0.0375)
    b4 = 0.0636
    b5 = (-0.0144)
    k = 0.0162
