from rectangle import Rectangle

class Point(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def inside(self, rectangle):
        return ((self.x >= rectangle.x) 
        and (self.x <= (rectangle.x + rectangle.width))
        and (self.y >= rectangle.y) 
        and (self.y <= rectangle.y + rectangle.height))
