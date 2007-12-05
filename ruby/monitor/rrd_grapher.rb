#!/usr/bin/env ruby

SERVERS=%w(mem01.stag.caring.com mem02.stag.caring.com)

class RrdGrapher
  def initialize(files, width=640, height=400)
    @files=files
    @width=width
    @height=height
  end

  # Make a variable by adding up all of values for the given rname accross the
  # given files.
  def mk_var(name, rname, type, transform="", files=@files)
    f_hash=Hash[*files.zip((1..files.length).to_a).flatten]
    defs=files.map do |file|
      "DEF:#{name}#{f_hash[file]}=#{file}:#{rname}:#{type.to_s.upcase}"
    end
    cdef="CDEF:#{name}=0"
    f_hash.values.each {|x| cdef += ",#{name}#{x},+"}
    defs + [cdef + transform]
  end

  def common_args(fn, title, range)
    args=['rrdtool', 'graph', fn, "-v", title, '-s', range]
    args += %W(-w #{@width} -h #{@height} -a PNG -t) + [title]
  end

end

class MemcacheGrapher < RrdGrapher
  def do_hit_misses(fn, range)
    args = common_args fn, 'Cache Requests/m', range
    args += mk_var 'hit', 'get_hits', :max, ',60,*'
    args += mk_var 'miss', 'get_misses', :max, ',60,*'
    args += ["AREA:hit#00ee00:Hits", "AREA:miss#ee0000:Misses"]
    system(*args)
  end

  def do_miss_rate(fn, range)
    args = common_args fn, 'Cache Miss Rate', range
    args += %w(-o -l .05 -u 50 --units=si)
    args += mk_var 'total', 'cmd_get', :max
    args += mk_var 'misses', 'get_misses', :max
    args += ["CDEF:rate=misses,total,/,100,*"]
    args += ["VDEF:vrate=rate,AVERAGE"]
    args += ["AREA:rate#000000:Miss Rate"]
    args += ['GPRINT:vrate:Average\: %0.2lf%%\n']
    args += ['HRULE:3#009900:Target']
    system(*args)
  end

  def do_bytes(fn, range)
    args = common_args fn, 'Bytes in Cache', range
    args += mk_var 'bytes', 'bytes', :max
    args += ["AREA:bytes#000000:Bytes"]
    system(*args)
  end

  def do_items(fn, range)
    args = common_args fn, 'Items in Cache', range
    args += mk_var 'items', 'curr_items', :max
    args += ["AREA:items#000000:Items"]
    system(*args)
  end
end

if $0 == __FILE__

  servers=$*.empty? ? SERVERS.map {|x| x + ".rrd"} : $*

  g=MemcacheGrapher.new(servers, 400, 200)
  g.do_hit_misses 'hitsmisses.png', 'now - 24 hours'
  g.do_miss_rate 'missrate.png', 'now - 24 hours'
  g.do_bytes 'bytes.png', 'now - 24 hours'
  g.do_items 'items.png', 'now - 24 hours'
end
