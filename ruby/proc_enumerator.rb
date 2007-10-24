#!/usr/bin/env ruby -w

class Array
  def Array.enumerate_proc(ob, method, args=[])
    a=[]
    ob.send(method, *args){|x| a << x }
    a
  end
end
