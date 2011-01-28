#Input_Manager.py
#

from sys import exit
import pygame.event as event
import pygame.key as key
from pygame import QUIT,KEYDOWN,KEYUP,MOUSEBUTTONDOWN,MOUSEBUTTONUP
from keypress import KeyPress
from mousepress import MousePress


#inputlist: [Input]
#Input = {Key_Id, Key_Behaviour, Action}
#Key_Id: Id of a pygame key or a String matching the pygame key
#Key_Behaviour: OnPress OnRelease etc
#Action: A function to perform when the given input occurs.

#Key_Dictionary: A dictionary of Strings for the corresponding pygame key id:s
#To speed up input lookups a store a dictionary where each key corresponds to an input or a set of inputs
#for each update, lookup each key input in dict to get the affected inputs only

#Each Input subclass name must begin with Key


#Functions
#add:     Add an input to the list of inputs
#remove:  Remove an input from the list of inputs
#Process: For all inputs, check if any of them is occuring and trigger the corresponding action


#Defect: Press two keys at the same time and release them. Should some time give 2 KeyUp events but sometimes only gets one.

class InputManager(object):
#param: [Input]
#requires:Pygame is initialized
    inputs = None
    def __init__(self, inputs):
        self.inputs = []
        for i in inputs: #LAST UPDATED HERE!!!
            if i[0] is "Key":
                key_behaviour = "Key" + i[2]
                #key_behaviour = key_behaviour + "." + key_behaviour
                self.inputs.append(eval(key_behaviour)(i[1],i[3]))
            elif i[0] is "Mouse":
                mouse_behaviour = "Mouse" + i[2]
                #mouse_behaviour = mouse_behaviour + "." + mouse_behaviour
                self.inputs.append(eval(mouse_behaviour)(i[1],i[3]))
  #desc: Look up the refreshed current input state and call the affected methods
    def update(self):
        for e in event.get():
            if e.type == QUIT: exit()
            if e.type is KEYDOWN:
                for i in self.inputs:
                    if i.id == e.key:
                        i.update("Down")
            if e.type is KEYUP:
                for i in self.inputs:
                    if i.id == e.key:
                        i.update("Up")
            if e.type is MOUSEBUTTONDOWN:
                for i in self.inputs:
                    if i.id == e.button:
                        i.update("Down")
            if e.type is MOUSEBUTTONUP:
                for i in self.inputs:
                    if i.id == e.button:
                        i.update("Up") 
			
        for i in self.inputs:
            i.notify()
            #chec for keyUpdates
