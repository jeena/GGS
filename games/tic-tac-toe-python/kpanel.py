#!/usr/bin/env python

import sys, socket, thread, gobject, getpass, time, os
try:
 	import pygtk
  	pygtk.require("2.16")
except:
  	pass
try:
	import gtk
  	import gtk.glade
except:
	sys.exit(1)

class GGSKPanel:

    def __init__(self):	
        #Set the Glade file
        self.gladefile = "ttt.glade"  
        self.wTree = gtk.glade.XML(self.gladefile, "window1") 

        #Create our dictionay and connect it
        dic = {  "on_window1_destroy_event"         : gtk.main_quit
                ,"on_coordinatorButton_clicked"     : lambda x: self.terminateProcess("ggs_coordinator")
                ,"on_coordinatorBackupButton_clicked" :\
                    lambda x: self.terminateProcess("ggs_coordinator_backup")
                ,"on_dispatcherButton_clicked"     : lambda x: self.terminateProcess("ggs_dispatcher")
            }

        self.wTree.signal_autoconnect(dic)

        self.wTree.get_widget("window1").show()
       
    def terminateProcess(self, process):
        os.system("echo \"exit(whereis(%s), 'Bye bye').\" | erl_call -sname ggs -e" % process)

    def setStatus(self, msg):
        self.wTree.get_widget("statusbar").push(0, msg)


if __name__ == "__main__":
    chat = GGSKPanel()
    gobject.threads_init()
    gtk.main()
