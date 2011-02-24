#!/usr/bin/env python

import sys, socket
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

class GGSCalc:

    def __init__(self):	
        #Set the Glade file
        self.gladefile = "calc.glade"  
        self.wTree = gtk.glade.XML(self.gladefile, "window1") 

        #Create our dictionay and connect it
        dic = {"on_mainWindow_destroy" : gtk.main_quit
            , "on_btn0_clicked" : lambda x: self.OnBtnClick(0)
            , "on_btn1_clicked" : lambda x: self.OnBtnClick(1)
            , "on_btn2_clicked" : lambda x: self.OnBtnClick(2)
            , "on_btn3_clicked" : lambda x: self.OnBtnClick(3)
            , "on_btn4_clicked" : lambda x: self.OnBtnClick(4)
            , "on_btn5_clicked" : lambda x: self.OnBtnClick(5)
            , "on_btn6_clicked" : lambda x: self.OnBtnClick(6)
            , "on_btn7_clicked" : lambda x: self.OnBtnClick(7)
            , "on_btn8_clicked" : lambda x: self.OnBtnClick(8)
            , "on_btn9_clicked" : lambda x: self.OnBtnClick(9)
            , "on_btnDiv_clicked" : lambda x: self.OnBtnClick("/")
            , "on_btnMul_clicked" : lambda x: self.OnBtnClick("*")
            , "on_btnMin_clicked" : lambda x: self.OnBtnClick("-")
            , "on_btnPlus_clicked" : lambda x: self.OnBtnClick("+")
            , "on_btnEq_clicked" : lambda x: self.calc() 
            , "on_btnDel_clicked" : lambda x: self.OnBtnClick("Del")
            , "on_btnConnect_clicked" : lambda x: self.connect()
            }

        for i in range(0,9):
            dic
        self.wTree.signal_autoconnect(dic)

        self.wTree.get_widget("window1").show()
        self.setStatus("Not connected")

    def setStatus(self, msg):
        self.wTree.get_widget("statusbar").push(0, msg)

    def calc(self):
        exp = self.wTree.get_widget("txtCalc").get_text()
        self.s.send("Server-Command: call\n"+
                "Token: %s\n" % self.token +
                "Content-Type: text\n"+
                "Content-Length: %s\n" % len(exp)+
                "\n"+
                exp)
        fs = self.s.makefile()
        self.wTree.get_widget("txtCalc").set_text(fs.readline().split(" ")[1])


    def connect(self):
        print "Connecting"
        self.setStatus("Connecting")
        HOST = 'localhost'    # The remote host
        PORT = 9000 # The same port as used by the server
        self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.s.connect((HOST, PORT))
        self.s.send("Server-Command: hello\n"+
                    "Content-Type: text\n"+
                    "Content-Length: 0\n"+
                    "\n")
        fs = self.s.makefile()
        self.token = fs.readline().split(" ")[0]
        self.setStatus("Connected!")

    def OnBtnClick(self, btn): 
        calcTxt = self.wTree.get_widget("txtCalc")
        t = calcTxt.get_text()
        if btn == "+":
            t += "+"
        elif btn == "-":
            t += "-"
        elif btn == "/":
            t += "/"
        elif btn == "=":
            t += "="
        elif btn == "*":
            t += "*"
        elif btn == "Del":
            t = t[:-1]
        else:
            t += str("\""+str(btn)+"\"")
        calcTxt.set_text(t)

if __name__ == "__main__":
	calc = GGSCalc()
	gtk.main()
