#Input.py

#Variables: Key_Id[Int,String], KeyBehaviour[enum], Action[method]
#Methods: update() Param nothing,  
#Behaviour


#in update. when called. takes the state of the key. compares with the behaviour. 
#if fullfilled perform action.


#All Input instances have an id.
class Input(object):
  id = -1
  fun = None
  def __init__(self, id, fun):
    self.id  = id
    self.fun = fun

  def notify(self):
      pass
  def update(self,state):
      pass
