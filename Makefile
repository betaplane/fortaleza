FC = mpifort

LDFLAGS = -lnetcdff
f2pyflags = --f90exec=${HOME}/opt/bin/mpifort

sources = $(wildcard child_*.f90)
execs = $(basename $(sources))
all: $(execs) python

child_%: child_%.f90 common.o
	$(FC) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) -c $< -o $@

python: fortaleza.f90
	f2py -c -m fortaleza_mod $^ $(f2pyflags)

clean:
	rm *.o *.so $(filter-out netcdf.mod, $(wildcard *.mod)) $(execs)
