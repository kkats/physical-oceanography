DSET ^/data/WGHC.bin
*
OPTIONS big_endian
UNDEf -9.99999999e33
*
XDEF 720 linear    0 0.5
YDEF 341 linear  -80 0.5
ZDEF  45 levels   0   10   20   30   40   50   75  100  125  150
                175  200  250  300  350  400  500  600  700  800
                900 1000 1100 1200 1300 1400 1500 1750 2000 2250
               2500 2750 3000 3250 3500 3750 4000 4250 4500 4750
               5000 5250 5500 5750 6000
TDEF   1 linear 1Jan9999 1yr
*
VARS 32
nbrlev  1 0 Number of gridded levels
radbub  1 0 Radius of the influence bubble (km) (*)
radcor  1 0 Decorrelation length scale (km)
depthml 1 0 Mixed layer depth (m) defined as depth where vertical density gradient >= 0.005 kg/m4
pres  45 0 pressure (dbar)
temp  45 0 temperature in situ (deg C)
ptem  45 0 potential temperature (deg C)
salt  45 0 salinity (PSU?)
doxy  45 0 oxygen (ml/L)
sili  45 0 silicate (umol/kg) -- var 10
ntra  45 0 nitrate (umol/kg)
psha  45 0 phosphate (umol/kg)
gamn  45 0 gamma-n (kg/m3)
sig0  45 0 sigma 0 (kg/m3)
sig2  45 0 sigma 2 (kg/m3)
sig4  45 0 sigma 4 (kg/m3)
err1  45 0 relative optimum interpolation error for T, Theta & S
err2  45 0 relative optimum interpolation error for Oxygen
err3  45 0 relative optimum interpolation error for Silicate
err4  45 0 relative optimum interpolation error for Nitrate -- var 20
err5  45 0 relative optimum interpolation error for Phosphate
lev1  45 0 actual num.of obs.used for the optimal interp.of T, Theta & S
lev2  45 0 actual num.of obs.used for the optimal interp.of Oxygen
lev3  45 0 actual num.of obs.used for the optimal interp.of Silicate
lev4  45 0 actual num.of obs.used for the optimal interp.of Nitrate
lev5  45 0 actual num.of obs.used for the optimal interp.of Phosphate
var1  45 0 temperature std. dev. from the mean within radcor (*)
var2  45 0 salinity    std. dev. from the mean within radcor (*)
var3  45 0 oxygen      std. dev. from the mean within radcor (*)
var4  45 0 silicate    std. dev. from the mean within radcor (*) --30
var5  45 0 nitrate     std. dev. from the mean within radcor (*)
var6  45 0 phosphate   std. dev. from the mean within radcor (*)
ENDVARS
