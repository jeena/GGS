from board import Board
from rectangle import Rectangle
from point import Point
from pygame.image import load
from pygame.rect import Rect
from pygame import Surface

#inherits Board.
#Used for displaying the board on the screen and interact with it
#thus images are needed for the various rectangles
#as well as a paint function
#Should be updated after every valid player move

class TicTacToeBoard(Board):
    def __init__(self, nr_of_rectangles):
        self.image_e = load("e.png")
        self.image_x = load("x.png")
        self.image_o = load("o.png") #TODO add the o image
        dimensions = Rectangle(0, 0, self.image_x.get_width(),
                                     self.image_x.get_height())
        self.players_turn = 0
        Board.__init__(self, nr_of_rectangles, dimensions)
        
    def paint(self, table_image):
        for game_rectangle in self.game_rectangles:
            x = game_rectangle.x
            y = game_rectangle.y
            w = game_rectangle.width
            h = game_rectangle.height

            image = None
            if game_rectangle.state == ' ':
                image = self.image_e
            elif game_rectangle.state == 'x':
                image = self.image_x
            elif game_rectangle.state == 'o':
                image = self.image_o
            print(game_rectangle.state)
            table_image.blit(image, Rect(x, y, w, h))

    def make_turn(self, mouse_point):
        for game_rectangle in self.game_rectangles:
            if (mouse_point.inside(game_rectangle) and
            game_rectangle.state == ' '):
                if self.players_turn == 0:
                    game_rectangle.state = 'x'
                elif self.players_turn == 1:
                    game_rectangle.state = 'o'
                self.players_turn = (self.players_turn + 1) % 2

            
