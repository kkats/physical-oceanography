# Preparations

## oceanogr

The [GNU Scientific Library](https://www.gnu.org/software/gsl/gsl.html) must be installed in your system.

Download source package of [hstatistics](https://hackage.haskell.org/package/hstatistics).  Tested with version 0.2.5.3 but should work as far as Numeric.Statistics.PCA is not drastically modified. Expand it at the same directory as oceanogr.cabal.

    % cd oceanogr
    % tar xvfz $(DOWNLOAD)/hstatistics-0.2.5.3.tar.gz

Apply patch.hstatistics and install right away

    % mv hstatistics-0.2.5.3 hstatistics-patched
    % cd hstatistics-patched
    % patch -p1 < ../patch.hstatistics
    % mv hstatistics.cabal hstatistics-patched.cabal
    % stack install hstatistics-patched

As of 25 Aug 2016, patches are necessary for [hnetcdf](https://github.com/ian-ross/hnetcdf) and [hmatrix-repa](https://github.com/amcphail/hmatrix-repa). See stack.yaml for detail.

## gamma-n

Because of the use of *double precision* rather than *single*, we use [Fortran/Matlab](http://www.teos-10.org/preteos10_software/gamma.tar.Z), not the Fortran only version.

    % cd gamma-n
    % cd fortran
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

A table of neutral density from 1 to 3000 dbar followed by interpolated salinity, temperature, pressure should appear on screen.

    % cd ../..
    % stack build
    % stack test

Two tables will be output. The first table is the output from this Haskell interface. The second is a copy of Fortran output. Sould be the same except format differences.

# Installation
