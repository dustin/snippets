#!/usr/bin/env ruby -w

class LinuxHostInfo

  def loadavg
    open("/proc/loadavg") { |f| f.read.split[0..2].map {|x| x.to_f} }
  end

  def cpustat
    process_lines("/proc/stat") { |s| s.map {|x| x.to_i} }
  end

  def vmstat
    process_lines("/proc/vmstat") { |s| s[0].to_i }
  end

  def meminfo
    process_lines("/proc/meminfo") do |s|
      raise "Unhandled multiplier:  #{s[1]}" if s[1] != "kB"
      s[0].to_i * 1024
    end
  end

  private

  def process_lines(fn)
    rv={}
    IO.foreach(fn) do |l|
      stuff=l.split(/:?\s+/)
      rv[stuff[0]]=yield stuff[1..-1]
    end
    rv
  end

end
