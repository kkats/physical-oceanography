# oceanogr

Download source package of [hstatistics](https://hackage.haskell.org/package/hstatistics).  Tested with version 0.2.5.2 but should work as far as Numeric.Statistics.PCA is not drastically modified. Expand it at the same directory as oceanogr.cabal.

    % cd oceanogr
    % tar xvfz $(DOWNLOAD)/hstatistics-0.2.5.2.tar.gz


Apply hstatistics.patch and install first

    % mv hstatistics-0.2.5.2 hstatistics-patched
    % cd hstatistics-patched
    % patch -p1 < ../patch.hstatistics
    % mv hstatistics.cabal hstatistics-patched.cabal
    % stack install hstatistics-patched

Build the whole library

    % stack build

## Bugs

`stack ghci` does not work. Why?
