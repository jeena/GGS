from rectangle import Rectangle

class GameRectangle(Rectangle):
    def __init__(self, index, x, y, width, height):
        self.index = index
        self.state = ' '
        Rectangle.__init__(self, x, y, width, height)
