from sys import exit
#from pygame.image import load
from pygame.display import set_mode, flip
from pygame import init
import pygame.event as event
from pygame.mouse import get_pos
from pygame import QUIT,K_UP
from inputmanager import InputManager
from tictactoeboard import TicTacToeBoard
from point import Point

#inputmanager = None
#screen = None
#board = None

def main():
    global inputmanager,screen,board
    #init graphics
    init()
    size = (150,150)
    screen = set_mode(size)
    
    #init game data
    nr_of_rectangles = 9
    board = TicTacToeBoard(nr_of_rectangles)
    #board.paint(screen)

    #init input
    inputmanager = InputManager([
    ("Mouse", 1, "Press", (lambda: board.make_turn(Point(get_pos()[0],get_pos()[1])))),
    ("Key", K_UP, "Press", (lambda: print("Hello Keyboard!"))),
    ])


    loop()

def loop():
    global inputmanager,screen,board
    while True:
        inputmanager.update()
        board.paint(screen)
        flip()
        


if __name__ == '__main__':
    main()
