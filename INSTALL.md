# Preparations

## oceanogr

Download source package of [hstatistics](https://hackage.haskell.org/package/hstatistics).  Tested with version 0.2.5.2 but should work as far as Numeric.Statistics.PCA is not drastically modified. Expand it at the same directory as oceanogr.cabal.

    % cd oceanogr
    % tar xvfz $(DOWNLOAD)/hstatistics-0.2.5.2.tar.gz


Apply hstatistics.patch and install first

    % mv hstatistics-0.2.5.2 hstatistics-patched
    % cd hstatistics-patched
    % patch -p1 < ../patch.hstatistics
    % mv hstatistics.cabal hstatistics-patched.cabal
    % stack install hstatistics-patched

## GSW

Prepare Fortran object files

    % make obj

The attached Oceanogr/GSWtools.hs is built for TEOS-10 GSW Fortran version 3.03.
If this version is to be used, nothing needs to be done. Otherwise;

- Download Fortran version from the [TEOS-10 website](http://www.teos-10.org/software/gsw_fortran_v3_03.zip), as of writing version 3.03 is available.

    ```
    % cd gsw_fortran
    % unzip $(DOWNLOAD)/gsw_fortran_v3_03.zip
    ```
- Path to gsw_data_v3_0.dat is written in gsw_oceanographic_toolbox.f90. Change if necessary.

	```
	--- gsw_oceanographic_toolbox.f90.dist  2015-07-14 10:38:42.000000000 +0900
	+++ gsw_oceanographic_toolbox.f90       2015-07-14 10:39:33.000000000 +0900
	@@ -3835,7 +3835,7 @@
	if(icalled.eq.0d0) then
	icalled = 1
	-   open(10,file='gsw_data_v3_0.dat',status='old',err=1)
	+   open(10,file='/opt/lib/GSW/gsw_fortran/gsw_data_v3_0.dat',status='old',err=1)
	flag_saar = 1
	read(10,*) (longs_ref(i), i=1,nx)
	read(10,*) (lats_ref(i), i=1,ny)
	```

- `% make gentool` will yield generate GSWtools.hs. Edit this.

- When ready, `% mv GSWtools.hs Oceangr/`.

# Build
Totally relying on [stack](https://github.com/commercialhaskell/stack).

    % stack build
    % cd GSW; stack test
