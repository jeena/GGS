#!/usr/bin/env python

import sys, socket, thread, gobject, getpass, time
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
        self.nicksListStore = gtk.ListStore(str)
        self.gladefile = "ggschat.glade"  
        self.wTree = gtk.glade.XML(self.gladefile, "window1") 

        self.setStatus("Not connected")
        self.connect(host, port)
        thread.start_new_thread(self.listenChat, ())
        thread.start_new_thread(self.luserCheck, ())
        #Create our dictionay and connect it
        dic = {"on_window1_destroy_event"   : gtk.main_quit
            , "on_sendButton_clicked"       : lambda x: self.chat()
            , "on_entry_activate"           : lambda x : self.chat()
            , "on_nickBox_activate"         : lambda x : self.changeNick()
            , "on_chatBox_focus"            : lambda x, y: self.wTree.get_widget("entry").grab_focus()
            }

        self.wTree.signal_autoconnect(dic)

        self.wTree.get_widget("nickBox").set_text(getpass.getuser())
        self.wTree.get_widget("window1").show()
        self.wTree.get_widget("entry").grab_focus()
        nicksList = self.wTree.get_widget("nicksList")
        self.changeNick()
        nicksList.set_model(self.nicksListStore)
#        self.nicksListStore.append(["Test!"])
       
        rendererText = gtk.CellRendererText()
        column = gtk.TreeViewColumn("Participants", rendererText, text=0)
        column.set_sort_column_id(0)    
        nicksList.append_column(column)

    def setStatus(self, msg):
        self.wTree.get_widget("statusbar").push(0, msg)

    def changeNick(self):
        params = self.wTree.get_widget("nickBox").get_text()
        self.s.send("Game-Command: nick\n" +
            "Token: %s\n" % self.token +
            "Content-Type: text\n" +
            "Content-Length: %s\n" % len(params)+
            "\n"+
            params)


    def chat(self):
        exp = self.wTree.get_widget("entry").get_text()
        nick = self.wTree.get_widget("nickBox").get_text()
        if exp[0] == "/":
            cmdStr = exp[1:].split(" ")
            cmd = cmdStr[0]
            params = ' '.join(cmdStr[1:])
            self.s.send("Game-Command: %s\n" % cmd +
                "Token: %s\n" % self.token +
                "Content-Type: text\n" +
                "Content-Length: %s\n" % len(params)+
                "\n"+
                params)
        else:
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
            if line.split(" ")[0] == "LUSERS":
                gobject.idle_add(self.updateUsers, line)
            else:
                gobject.idle_add(self.updateChatText, line)

    def updateChatText(self, text):
        self.wTree.get_widget("chatBox").get_buffer().insert_at_cursor(text)

    def luserCheck(self):
        while True:
            self.s.send("Game-Command: lusers\n" +
                "Token: %s\n" % self.token +
                "Content-Type: text\n" +
                "Content-Length: 0\n"+
                "\n")
            time.sleep(2)

    def updateUsers(self, text):
        nicks = ' '.join(text.split(" ")[1:])
        evalNicks = eval(nicks)
        self.nicksListStore.clear()
        for nick in evalNicks:
            self.nicksListStore.append([nick])

if __name__ == "__main__":
    host = "localhost"
    port = 9000
    if len(sys.argv) >= 2:
        host = sys.argv[1]
        port = int(sys.argv[2])
    chat = GGSChat(host, port)
    gobject.threads_init()
    gtk.main()
