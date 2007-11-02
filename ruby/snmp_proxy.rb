#!/usr/bin/env ruby -w

require 'socket'

$SAFE=2

# Requested ports
ports=[161, 162]
# Create the bound sockets.
sockets = ports.map{|p| l = UDPSocket.new; l.bind('', p); l }
pmap=Hash[*sockets.zip(ports).flatten]
puts "Bound to #{ports.join(', ')}"

while true:
  ls=IO.select(sockets)
  ls[0].each do |l|
    d, stuff=l.recvfrom(1024)
    puts "proxying #{d.length} bytes of trapness (from #{pmap[l]})"
    l.send(d, 0, '127.0.0.1', 1162)
  end
end
