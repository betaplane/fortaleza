import fortaleza_mod

class fort(object):
    def __init__(self, file_name):
        self.mod = fortaleza_mod.mod
        self.mod.spawn(1, file_name)

    
