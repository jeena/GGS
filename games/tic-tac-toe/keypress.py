#KeyPress.py

#Should call method if a key was pressed
#Variales: isPressed, actionPerformed
#To make sure that the key only get's pressed once when holding a button down, Two variables are needed.
#isPressed: If not pressed, call method on next keypress
#actionPerformed: If method was called and key is still down, do not call method no more
#Input instances will be allocated before gameplay, therefore: isPressed and actionPerformed will be initialized false.

#methods: init, update
#update: Will be called when there is a change in this actual keys state. With respect to the input class behaviour, 
#update will evaluate if it's corresponding method should be called or not
#param: state tells if the key is down or up.


#Four possible preconditions: is not pressed and key is up.   ->  Set isPressed to false.
#                             is not pressed and key is down. ->  
#			      is pressed and key is up.
#			      is pressed and key is down.

from input import Input

class KeyPress(Input):
  def __init__(self, id, function):
      Input.__init__(self, id, function)

  def update(self,state):
      if state == "Down":
	      self.fun()
