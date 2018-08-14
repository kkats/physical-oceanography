module Oceanogr.LADCP (LADCPdata(..)) where

import Oceanogr.CTD (Cast)
import qualified Data.Vector.Unboxed as V

--
-- dzt is "length of transmitted sound pulse" (Polzin et al. 2002)
--     output as LADCP_dn_conf_pulse_len_m
-- dzr is "receive bin, depth bin range, finite-differencing bin" (Polzin et al., 2002)
--     output as LADCP_dn_conf_bin_len_m
--
data LADCPdata = LADCPdata {
            ladcpCast :: Cast,
            ladcpDzt  :: Float,
            ladcpDzr  :: Float,
            ladcpZ    :: V.Vector Float,
            ladcpU    :: V.Vector Float,
            ladcpV    :: V.Vector Float,
            ladcpEV   :: V.Vector Float,
            ladcpN    :: V.Vector Int
            }
