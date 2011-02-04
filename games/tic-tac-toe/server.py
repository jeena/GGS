#server.py
import json
from socket import socket, AF_INET, SOCK_STREAM


class server(object):
    def __init__(self, port=None):
        self.port = port
        self.world = GGS.init()
        self.socket = socket(AF_INET, SOCK_STREAM)
        self.socket.connect(("www.???.com", 80))

    def turn(self, id, index):
        rows = sqrt(board.nr_of_rectangles)
        x = int(index / rows)
        y = int(index % rows)

        json.dumps({"x": x, "y": y}
        world.callCommand("tictactoe", "set", json.dumps({"x": x, "y": y}))

        sent = 0
        length = len(
        while sent
