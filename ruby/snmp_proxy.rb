#!/usr/bin/env ruby -w

require 'socket'

$SAFE=2

# Requested ports
ports=[161, 162]
# Create the bound sockets.
sockets = ports.map{|p| l = UDPSocket.new; l.bind('', p); l }
puts "Bound sockets:  #{sockets.join(', ')}"

while true:
  ls=IO.select(sockets)
  ls[0].each do |l|
    d, stuff=l.recvfrom(1024)
    puts "proxying #{d.length} bytes of trapness (from #{l})"
    l.send(d, 0, '127.0.0.1', 1162)
  end
end
