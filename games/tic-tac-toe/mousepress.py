from input import Input

class MousePress(Input):
    def __init__(self, id, function):
        Input.__init__(self, id, function)

    def update(self,state):
        if state == "Down":
            self.fun()
