#!/usr/bin/env ruby -w

require 'thread'
require 'rrd_schema'
require 'sysinfo_rrd'
require 'memcache_rrd'

QUEUE=Queue.new

module RRDSchema
  alias send_rrd_cmd_stupid send_rrd_cmd
  def send_rrd_cmd(s)
    QUEUE << s
  end
end

class MonitorDaemon

  def initialize
    @threads = []
  end

  def add_task(freq)
    # I'm going to punt and just make threads that loop and stuff.
    @threads << Thread.new do
      while true
        yield QUEUE
        sleep freq
      end
    end
  end

  def for_responses
    while true
      yield QUEUE.pop
    end
  end

  def add_proc_task(freq, url, user, pass)
    lh=LinuxHostInfo.new(url, user, pass)
    h=LinuxHostRRD.new(lh)
    add_task(freq) { |q| h.rrd_inserts }
  end

  def add_memcached_task(freq, servers)
    m=MemCacheRRD.new(servers)
    add_task(freq) { |q| m.rrd_inserts }
  end

end

if $0 == __FILE__
  md=MonitorDaemon.new

  md.add_proc_task 60, 'http://repo.dev.caring.com:8182/', 'rrd', 'rrdt00l'
  md.add_memcached_task 60, %w(mem01.stag.caring.com mem02.stag.caring.com)

  md.for_responses do |response|
    puts response
    $stdout.flush
  end
end
