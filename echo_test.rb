#!/usr/bin/env ruby -wKU

require 'socket'      # Sockets are in standard library

hostname = 'localhost'
port = 7000

s = TCPSocket.open(hostname, port)

print "What to echo? "
q = gets
s.print(q)

while true
  line = s.gets   # Read lines from the socket
  puts "Got Echo: " + line.chop      # And print with platform line terminator
end
s.close               # Close the socket when done
