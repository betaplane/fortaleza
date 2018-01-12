import fortaleza_mod

class fort(object):
    def __init__(self, file_name, var_name=None, np=4):
        self.mod = fortaleza_mod.mod
        self.mod.spawn(np, file_name, var_name)
        s = self.mod.shape
        x = self.mod.reshape((np, s[0], s[1])).transpose((1, 0, 2)).reshape((s[1], -1))
        self.x = x
