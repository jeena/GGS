#!/usr/bin/env python

import sys, socket, thread, gobject, getpass
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

class GGSChat:

    def __init__(self,host, port):	
        #Set the Glade file
        self.gladefile = "ggschat.glade"  
        self.wTree = gtk.glade.XML(self.gladefile, "window1") 

        self.setStatus("Not connected")
        self.connect(host, port)
        thread.start_new_thread(self.listenChat, ())
        #Create our dictionay and connect it
        dic = {"on_window1_destroy_event" : gtk.main_quit
            , "on_sendButton_clicked" : lambda x: self.chat()
            , "on_entry_activate"   : lambda x : self.chat()
            , "on_chatBox_focus" : lambda x, y: self.wTree.get_widget("entry").grab_focus()
            }

        for i in range(0,9):
            dic
        self.wTree.signal_autoconnect(dic)

        self.wTree.get_widget("nickBox").set_text(getpass.getuser())
        self.wTree.get_widget("window1").show()
        self.wTree.get_widget("entry").grab_focus()

    def setStatus(self, msg):
        self.wTree.get_widget("statusbar").push(0, msg)

    def chat(self):
        exp = self.wTree.get_widget("entry").get_text()
        nick = self.wTree.get_widget("nickBox").get_text()
        exp = "<%s> %s" % (nick, exp)
        self.s.send("Game-Command: chat\n"+
                "Token: %s\n" % self.token +
                "Content-Type: text\n"+
                "Content-Length: %s\n" % (len(exp))+
                "\n"+
                exp+"\n")
        self.wTree.get_widget("entry").set_text("")
        #self.listenChat()


    def connect(self, host,port):
        print "Connecting"
        self.setStatus("Connecting")
        HOST = host    # The remote host
        PORT = port # The same port as used by the server
        self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.s.connect((HOST, PORT))
        self.token = self.s.recv(1024)
        self.setStatus("Connected!")

    def listenChat(self):
        print "listening"
        fs      = self.s.makefile()
        while True:
            line    = fs.readline()
            print "Received: ", line
            gobject.idle_add(self.updateChatText, line)

    def updateChatText(self, text):
        self.wTree.get_widget("chatBox").get_buffer().insert_at_cursor(text)
if __name__ == "__main__":
    host = "localhost"
    port = 9000
    if len(sys.argv) >= 2:
        host = sys.argv[1]
        port = int(sys.argv[2])
    chat = GGSChat(host, port)
    gobject.threads_init()
    gtk.main()
