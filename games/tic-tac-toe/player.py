from pygame.mouse import get_pos
from point import Point

class Player(object):
    def __init__(self, id, shape, board):
        self.shape = shape
        self.board = board
        self.id = id


    def turn(self):
        #Ask mouse for position
        board.make_turn(Point(get_pos()[0],get_pos()[1]))
            
        
