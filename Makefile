.SUFFIXES:

vpath %.f90 src
vpath %.f90 test

FC = gfortran
FFLAGS = -Og -std=f2018 -Wall -Wextra -pedantic -fimplicit-none -fcheck=all -fbacktrace
#FFLAGS = -O3 -std=f2018 -Wall -Wextra -pedantic -fimplicit-none -fbacktrace
COMPILE = $(FC) $(FFLAGS) -c
MAKEMOD = $(FC) $(FFLAGS) -fsyntax-only -c

SOURCES = dllnode_mod.f90 \
    dll_mod.f90 \
	  check.f90

test.exe : $(subst .f90,.o,$(SOURCES))
	$(FC) -o $@ $+

.PHONY: clean

clean:
	-rm -f *.o *.mod *.smod

%_m.mod %.o : %.f90
	$(COMPILE) -o $*.o $<
	@touch $@

check.o : dllnode_mod.o dll_mod.o

dll_mod.o : dllnode_mod.o




# build rules

#$(build)/%.o : $(src)/%.f90
#	$(cc) -c $< -o $@
#$(build)/%.o : $(test)/%.f90
#	$(cc) -c $< -o $@
