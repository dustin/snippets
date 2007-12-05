#!/usr/bin/env ruby

require 'yaml'
require 'net/http'

# A list of distinct colors.
COLORS=%w(007700 770000 000077 770077 777777 000000)

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

  # XXX:  Make a better color algorithm
  def mk_color(offset)
    COLORS[offset]
  end

end

class MemcacheGrapher < RrdGrapher
  def do_hit_misses(fn, range)
    args = common_args fn, 'Cache Requests/m', range
    args += mk_var 'hit', 'get_hits', :max, ',60,*'
    args += mk_var 'miss', 'get_misses', :max, ',-60,*'
    args += ["AREA:hit#00ee00:Hits", "AREA:miss#ee0000:Misses"]
    args += ['HRULE:0#000000']
    system(*args)
  end

  def do_hit_misses_per_server(fn, range)
    args = common_args fn, 'Cache Requests/m', range
    f_hash=Hash[*@files.zip((1..@files.length).to_a).flatten]
    @files.each do |f|
      h=f_hash[f]
      c=mk_color h
      sn=File.basename(f, ".rrd")
      args += %W(DEF:getraw#{h}=#{f}:cmd_get:MAX
                DEF:setraw#{h}=#{f}:cmd_set:MAX
                CDEF:get#{h}=getraw#{h},60,*
                CDEF:set#{h}=setraw#{h},-60,*
                LINE:get#{h}##{c}:#{sn}\ Hits
                LINE:set#{h}##{c}:#{sn}\ Misses\\n)
    end
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

  def draw_all(suffix, range)
    do_hit_misses "hitsmisses_#{suffix}.png", range
    do_hit_misses_per_server "hitsmisses_s_#{suffix}.png", range
    do_miss_rate "missrate_#{suffix}.png", range
    do_bytes "bytes_#{suffix}.png", range
    do_items "items_#{suffix}.png", range
  end

end

class LinuxGrapher < RrdGrapher

  def initialize(files, width=640, height=400, prefix=nil)
    super(files, width, height)
    @prefix=prefix
  end

  def do_cpu(fn, range)
    args = common_args fn, 'CPU Utilization', range
    args += mk_var 'user', 'cpu_user', :max
    args += mk_var 'nice', 'cpu_nice', :max
    args += mk_var 'sys', 'cpu_sys', :max
    # THis doesn't quite give the right results.
    # args += mk_var 'idle', 'cpu_idle', :max
    args << "CDEF:idle=100,user,nice,sys,-,-,-"
    args << "AREA:user#000077:User"
    args << "STACK:nice#0000ff:Nice"
    args << "STACK:sys#770000:System"
    args << "STACK:idle#00ee00:Idle"
    system(*args)
  end

  def do_paging(fn, range)
    args = common_args fn, 'Paging Activity', range
    args += mk_var 'pgpgin', 'pgpgin', :max
    args += mk_var 'pgpgout', 'pgpgout', :max
    args << "CDEF:out=pgpgout,-1,*"
    args << "AREA:pgpgin#000077:Paging In"
    args << "AREA:out#770000:Paging Out"
    system(*args)
  end

  def do_swapping(fn, range)
    args = common_args fn, 'Swapping Activity', range
    args += mk_var 'pswpin', 'pswpin', :max
    args += mk_var 'pswpout', 'pswpout', :max
    args << "CDEF:out=pswpout,-1,*"
    args << "AREA:pswpin#000077:Swapping In"
    args << "AREA:out#770000:Swapping Out"
    system(*args)
  end

  def do_ctx(fn, range)
    args = common_args fn, 'Context Switches', range
    args += mk_var 'ctxt', 'ctxt', :max
    args << "AREA:ctxt#770000:Context Switches"
    system(*args)
  end

  def do_load(fn, range)
    args = common_args fn, 'Load Average', range
    args += %W(-X 0)
    args += mk_var 'load5', 'load5', :max
    args << "CDEF:l5=load5,100,/"
    args << "AREA:l5#770000:Load Average"
    system(*args)
  end

  def draw_all(suffix, range)
    do_cpu prefix("cpu_#{suffix}.png"), range
    do_paging prefix("paging_#{suffix}.png"), range
    do_swapping prefix("swapping_#{suffix}.png"), range
    do_ctx prefix("ctx_#{suffix}.png"), range
    do_load prefix("load_#{suffix}.png"), range
  end

  private

  def prefix(s)
    @prefix == nil ? s : @prefix + s
  end

end

if $0 == __FILE__

  conf=YAML.load_file($*[0])

  width=400
  height=200

  graphers = []

  mcg=MemcacheGrapher.new(conf['memcached'].map {|x| x + ".rrd"}, width, height)
  graphers << mcg

  if conf['linux']
    hostfiles=conf['linux'].map{|h| URI.parse(h['url']).host + ".rrd"}
    lg=LinuxGrapher.new(hostfiles, width, height)

    graphers << lg

    conf['linux'].each do |h|
      hn = URI.parse(h['url']).host
      graphers << LinuxGrapher.new([hn + ".rrd"], width, height,
        "hosts/" + hn + "_")
    end
  end


  # Draw all the graphs.
  graphers.each {|g| g.draw_all 'day', 'now - 24 hours' }
end
