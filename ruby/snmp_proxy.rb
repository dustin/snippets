#!/usr/bin/env ruby -w

require 'socket'

$SAFE=2

l=UDPSocket.new
l.bind('', 161)

while true:
  d, stuff=l.recvfrom(1024)
  puts "proxying #{d.length} bytes of trapness"
  l.send(d, 0, '127.0.0.1', 1162)
end
