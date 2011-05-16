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

class GGSTTT:

    def __init__(self):	
        #Set the Glade file
        self.gladefile = "ttt.glade"  
        self.wTree = gtk.glade.XML(self.gladefile, "window1") 
        host = "localhost"
        port = 9000

        #Create our dictionay and connect it
        dic = {  "on_window1_destroy_event"         : gtk.main_quit
                ,"on_x0y0_clicked"     : lambda x: self.sendMove("{'x':0,'y':0}")
                ,"on_x0y1_clicked"     : lambda x: self.sendMove("{'x':0,'y':1}")
                ,"on_x0y2_clicked"     : lambda x: self.sendMove("{'x':0,'y':2}")
                ,"on_x1y0_clicked"     : lambda x: self.sendMove("{'x':1,'y':0}")
                ,"on_x1y1_clicked"     : lambda x: self.sendMove("{'x':1,'y':1}")
                ,"on_x1y2_clicked"     : lambda x: self.sendMove("{'x':1,'y':2}")
                ,"on_x2y0_clicked"     : lambda x: self.sendMove("{'x':2,'y':0}")
                ,"on_x2y1_clicked"     : lambda x: self.sendMove("{'x':2,'y':1}")
                ,"on_x2y2_clicked"     : lambda x: self.sendMove("{'x':2,'y':2}")
                ,"on_connectBtn_clicked" : lambda x: self.doConnect()
            }

        self.wTree.signal_autoconnect(dic)

        self.wTree.get_widget("window1").show()

    def doConnect(self):
        self.setStatus("Not connected")
        hostport = self.wTree.get_widget("adress").get_text()
        host, port = hostport.split(":")
        
        self.connect(host, int(port))
        thread.start_new_thread(self.listen, ())

        token = self.wTree.get_widget("token").get_text()
        self.s.send("Server-Command: hello\n" +
            "Content-Type: text\n" +
            "Content-Length: %s\n" % len(token)+
            "\n"+
            token)
        self.s.send("Game-Command: hi\n" +
            "Content-Type: text\n" +
            "Content-Length: 0\n"+
            "\n")

    def sendMove(self, move):
        print "Sending move", move
        cmd = "set"
        self.s.send("Game-Command: %s\n" % cmd +
            "Content-Type: text\n" +
            "Content-Length: %s\n" % len(move)+
            "\n"+
            move)

    def setStatus(self, msg):
        self.wTree.get_widget("statusbar").push(0, msg)

    def listen(self):
        msg = {}
        print "listening"
        fs      = self.s.makefile()
        while True:
            line    = fs.readline()
            print "Received: '%s" % line.strip()
            if line != "\n":
                key = line.split(":")[0]
                value = line.split(":")[1]
                msg[key] = value.strip()
            else:
                msg["DATA"] = fs.read(int("%s" % msg["Content-Size"]))
                print "Got data:", msg
                self.protocolHandler(msg)

    def protocolHandler(self, msg):
        if msg["Client-Command"] == "hello":
            data = msg["DATA"]
            self.token, defined, table_token = data.split(",")
            if defined == "false":
                print "Defining game"
                js = open("server.js").read()
                self.s.send("Server-Command: define\n"+
                            "Content-Type: text\n" +
                            ("Content-Length: %s\n" % len(js))+
                            "\n%s" % js)

        elif msg["Client-Command"] == "welcome":
            self.setStatus("You are player %s" % msg["data"])
        elif msg["Client-Command"] == "chat":
            gobject.idle_add(self.updateChatText, msg["DATA"])
        elif msg["Client-Command"] == "lusers":
            print msg
            gobject.idle_add(self.updateUsers, msg["DATA"])

    def connect(self, host,port):
        print "Connecting"
        self.setStatus("Connecting")
        self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.s.connect((host, port))
        self.setStatus("Connected!")




if __name__ == "__main__":
    ttt = GGSTTT()
    gobject.threads_init()
    gtk.main()
