FC = mpifort

LDFLAGS = -lnetcdff
f2pyflags = --f90exec=${HOME}/opt/bin/mpifort

sources = $(wildcard child_*.f90)
execs = $(basename $(sources))
all: $(execs) python

child_% : child_%.f90 fortaleza_common.o
	$(FC) -o $@ $^ $(LDFLAGS)

%.o : %.f90
	$(FC) -c $< -o $@ $(LDFLAGS)

python : fortaleza.f90
	f2py -c -m fortaleza_mod $^ $(f2pyflags)

clean :
	rm *.so $(filter-out netcdf.mod, $(wildcard *.mod)) $(execs)
