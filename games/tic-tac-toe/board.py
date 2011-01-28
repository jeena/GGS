from rectangle import Rectangle
from gamerectangle import GameRectangle
from math import sqrt
#param: nr_of_squares, dimensions(could be a Rectangle)
#creates an array of gamerectangles within itself with correct 
#positions from each other

class Board(object):
    def __init__(self, nr_of_rectangles, dimensions):
        self.game_rectangles = []
        axis = sqrt(nr_of_rectangles)
        width = dimensions.width
        height = dimensions.height
        for index in range(nr_of_rectangles):
            x = width * (index % axis)
            y = height * int(index / axis) 
            gr = GameRectangle(index, x, y, width, height)
            self.game_rectangles.append(gr)
