# physical-oceanography
Tools for physical oceanographic data analysis.

## oceanogr

Modules for IO (binary IO, netcdf), special data handling (SAM index, ETOPO5 topography, WGHC climatology),
linear data analysis (EOF, low-pass filters, leastsquare fit), power spectra (PSD),
parallel matrix computation (RepaExpand), and others (Misc).

## observation

For visualising CTD data from [GO-SHIP](http://www.go-ship.org)-style, full-depth observation.

1. It is necessary to provide `CTDfileRead`, a set of header information for the CTD output (e.g. `example/MR1609.hs`).
2. Build a list of CTD station to plot using `readCTD` (e.g. `example/MakeList.hs`).
3. Section data along the list can be output by `sectionCTD` (e.g. `example/PlotSection.hs`).

## gamma-n

Calculate an approximation of the [neutral density](http://www.teos-10.org/preteos10_software/neutral_density.html) surfaces, *gamma-n* from measured temperature, salinity, and pressure.

## GSW

Is now in a separate [repo](https://github.com/TEOS-10/GSW-Haskell.git) under [TEOS-10](https://github.com/TEOS-10).

