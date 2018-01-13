import fortaleza_mod
from timeit import default_timer as timer

class fort(object):
    def __init__(self, file_name, var_name=None, np=4):
        start = timer()
        self.mod = fortaleza_mod.mod
        self.mod.spawn(np, file_name, var_name)
        s = self.mod.shape
        c = int(s[0] / np)
        x = self.mod.x.reshape((np, c, -1)).transpose((1, 0, 2)).reshape((s[1], -1))
        self.x = x
        self.duration = timer() - start
