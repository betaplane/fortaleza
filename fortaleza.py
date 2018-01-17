import sys
sys.path.append('/sata1_ceazalabs/arno/HPC/uvHome/code/fortran')
import fortaleza_mod
import numpy as np
import netCDF4
from timeit import default_timer as timer

class fort(object):
    def __init__(self, file_name, var_name=None, nproc=4):
        start = timer()
        self.mod = fortaleza_mod.mod
        self.mod.spawn(nproc, file_name, var_name)
        self.duration = timer() - start

        s = self.mod.var_shape
        cs = np.r_[0, np.cumsum(s)]
        self.dims = [self.mod.dims[cs[i]: cs[i+1]] for i in range(4)]
        self.x = self.mod.x.reshape((nproc, s[1], -1)).transpose((0, 2, 1)).reshape((s[0], -1))
