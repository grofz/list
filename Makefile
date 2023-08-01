.SUFFIXES:

vpath %.f90 src
vpath %.f90 test
vpath %.f90 example

FC = gfortran
FFLAGS = -Og -std=f2018 -Wall -Wextra -pedantic -fimplicit-none -fcheck=all -fbacktrace
#FFLAGS = -O3 -std=f2018 -Wall -Wextra -pedantic -fimplicit-none -fbacktrace
COMPILE = $(FC) $(FFLAGS) -c
MAKEMOD = $(FC) $(FFLAGS) -fsyntax-only -c

SOURCES = dllnode_mod.f90 \
    dll_mod.f90 \
    user_mod.f90


MAIN1 = check.f90
MAIN2 = ex1.f90

all :	test.exe ex1.exe

test.exe : $(subst .f90,.o,$(SOURCES)) $(subst .f90,.o,$(MAIN1))
	$(FC) -o $@ $+

ex1.exe : $(subst .f90,.o,$(SOURCES)) $(subst .f90,.o,$(MAIN2))
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
