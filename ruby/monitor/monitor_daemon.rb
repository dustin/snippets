#!/usr/bin/env ruby -w

require 'thread'
require 'yaml'

require 'rrd_schema'
require 'memcache_rrd'

class Job

  attr_reader :name, :freq, :next

  def initialize(freq, name, block)
    @name=name
    @freq=freq
    @block=block

    initial_delay=[freq, 15].min / 2
    @next=Time.now.to_i + initial_delay
  end

  def run
    @next=Time.now.to_i + @freq
    @block.call
  end

end

class MonitorDaemon

  def initialize
    @jobs = []
  end

  def add_task(freq, name, &block)
    @jobs << Job.new(freq, name, block)
  end

  def run_all
    while true
      now = Time.now.to_i
      @jobs.select {|j| j.next <= now}.each do |j|
        begin
          j.run
          $stdout.flush
        rescue => e
          $stderr.puts "Problem running #{j.name}:  #{e}"
        end
      end

      nextup=@jobs.min {|a,b| a.next <=> b.next}
      now = Time.now.to_i
      delay = nextup.next - now
      sleep delay if delay > 0
    end
  end

  def add_proc_task(freq, url, user, pass)
    lh=LinuxHostInfo.new(url, user, pass)
    h=LinuxHostRRD.new(lh)
    add_task(freq, "proc #{url}") { h.rrd_inserts }
  end

  def add_memcached_task(freq, servers)
    m=MemCacheRRD.new(servers)
    add_task(freq, "memcached [#{servers.join(', ')}]") { m.rrd_inserts }
  end

  def add_rails_poll(freq, c)
    add_task(freq, "rails poll") do
      h=c['host']
      u=c['user']
      p=c['pass'] || ''
      d=c['database']
      RailsLogRRD.new(RailsLog.new(h, u, p, d)).rrd_inserts
    end
  end

end

if $0 == __FILE__
  raise "Need config file" if $*.empty?
  conf=YAML.load_file($*[0])

  md=MonitorDaemon.new

  if conf['linux']
    require 'sysinfo_rrd'
    conf['linux'].each do |h|
      md.add_proc_task 60, h['url'], h['user'], h['pass']
    end
  end

  conf['memcached'].each do |server|
    md.add_memcached_task 60, [server]
  end

  if conf['db']
    require 'rails_log'
    md.add_rails_poll 300, conf['db']
  end

  md.run_all
end
