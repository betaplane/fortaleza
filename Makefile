FC = mpifort

LDFLAGS = -lnetcdff
f2pyflags = --f90exec=${HOME}/opt/bin/mpifort

test: test.f90
	$(FC) -o $@ $^ $(LDFLAGS)

python: ptest.f90
	f2py -c -m ptest $^ $(f2pyflags)
