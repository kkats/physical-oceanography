# physical-oceanography
Tools for physical oceanographic data analysis.

## oceanogr

Modules for IO (binary IO, netcdf), special data handling (SAM index, ETOPO5 topography),
linear data analysis (EOF, low-pass filters, leastsquare fit), power spectra (PSD),
parallel matrix computation (RepaExpand), and others (Misc).

## GSW

Haskell interface for *Thermodynamic Equation Of Seawater - 2010* or [TEOS-10](http://www.teos-10.org). Although apparently much newer versions are available on [GitHub](https://github.com/TEOS-10/GSW-Fortran), this version is based on the latest version available on [the original wabsite](http://www.teos-10.org/software.html) as suggested [elsewhere](https://github.com/TEOS-10/GSW-Matlab/blob/master/README.md).
