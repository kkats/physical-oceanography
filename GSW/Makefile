# change
GSWSRC=gsw_fortran/gsw_oceanographic_toolbox.f90

# no need to change
SIZESRC=size_r14.f90
SIZEOBJ=$(SIZESRC:.f90=.o)
GSWOBJ=$(shell basename $(GSWSRC:.f90=.o))

FC=gfortran

gentool : $(SIZEOBJ) Gentoolbox.hs
	stack build
	stack exec gentoolbox $(GSW_FORTRAN_SRC)

obj :: $(SIZEOBJ) $(GSWOBJ)

$(SIZEOBJ) : $(SIZESRC)
	$(FC) -c -fPIC -o $@ $<
$(GSWOBJ) : $(GSWSRC)
	$(FC) -c -fPIC -o $@ $<


clean:
	rm -fr *.o *.hi a.out
