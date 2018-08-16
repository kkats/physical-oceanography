# Installation

## oceanogr

Following libraries are necessary before installing the Haskell library.

- [FFTW](http://www.fftw.org) Both double and float precisions for package `fft`.
- [NetCDF](https://www.unidata.ucar.edu/software/netcdf/) for package `hnetcdf`.
- [LAPACK](http://www.netlib.org/lapack/) for package `hmatrix`.
- [GSL](https://www.gnu.org/software/gsl/) for package `hmatrix-gsl-stats`


    % cd oceanogr
    % stack install

## gamma-n

Because of the use of *double precision* rather than *single*, we use [Fortran/Matlab](http://www.teos-10.org/preteos10_software/neutral_density.html), not the Fortran only version

    % cd gamma-n/fortran
    % gzip -dc $(DOWNLOAD)/gamma.tar.Z | tar xvf -

Apply the patch which does two jobs.
The codes cannot be compiled with `gfortran` unless long lines are folded. Also the Fortran function *indx* could clash with the function with the same name in [GSW Toolbox](http://www.teos-10.org/software.htm). Change the function name here to *indxGamma*.

    % cp -r gamma patched
    % cd patched
    % patch -p1 < ../patch.gamma

Change the location of the data file `gamma.nc` at lines 81 to 82 in `read-nc.F`. Compile and test.

    % make
    % make example
    % ./example

You need [NetCDF](https://www.unidata.ucar.edu/software/netcdf/) library in Fortran compiled with V2 interface. This usually means C version of `libnetcdf` be configure and compiled with `--enable-v2` option.

A table of neutral density from 1 to 3000 dbar followed by interpolated salinity, temperature, pressure should appear on screen.

    % cd ../..
    % stack build
    % stack test
    % stack install

Two tables will be output. The first table is the output from this Haskell interface. The second is a copy of Fortran output. Sould be the same except format differences.

## observation

    % cd observation
    % stack install
