#!/usr/bin/env ruby -wKU

require 'socket'      # Sockets are in standard library

hostname = 'localhost'
port = 7000

print "Which port @ loclhost?"
port = gets

s = TCPSocket.open(hostname, port.chop)

s.print(" __boot")

while true
  line = s.gets   # Read lines from the socket
  puts "Got Echo: " + line.chop      # And print with platform line terminator
end
s.close               # Close the socket when done
