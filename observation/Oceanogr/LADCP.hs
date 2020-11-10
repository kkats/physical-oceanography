module Oceanogr.LADCP (LADCPdata(..)) where

import Oceanogr.CTD (Cast)
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Char8 as B

--
-- dzt is "length of transmitted sound pulse" (Polzin et al. 2002)
--     output as LADCP_dn_conf_pulse_len_m
--     see `WT' command of LADCP configuration. If not set, equal to WT
-- dzr is "receive bin, depth bin range, finite-differencing bin" (Polzin et al., 2002)
--     output as LADCP_dn_conf_bin_len_m
--     see `WS' command of LADCP configuration
--
-- isPre2002 = True  -- if processed with UH shear method.
--           = False -- if processed with LDEO inversion method.
--
data LADCPdata = LADCPdata {
            ladcpIsPre2002 :: Bool,
            ladcpCast :: Cast,
            ladcpEXPO :: B.ByteString,
            ladcpDzt  :: Float,
            ladcpDzr  :: Float,
            ladcpZ    :: V.Vector Float,
            ladcpU    :: V.Vector Float,
            ladcpV    :: V.Vector Float,
            ladcpEV   :: V.Vector Float,
            ladcpN    :: V.Vector Int
            } deriving (Show)
