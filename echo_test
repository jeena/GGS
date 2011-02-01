#!/usr/bin/env ruby -wKU

require 'socket'      # Sockets are in standard library

hostname = 'localhost'
port = 7000

print "Which port @ loclhost?"
port = gets

s = TCPSocket.open(hostname, port.chop)

s.print("__hello")

while true
  line = s.gets   # Read lines from the socket
  puts ">> " + line.chop      # And print with platform line terminator
  s.print(gets.chop)
end
s.close               # Close the socket when done
