#!/usr/bin/env ruby -w

require 'net/http'

# Get Linux proc info via http
class LinuxHostInfo

  def initialize(base_url, user=nil, pass=nil)
    @base_url=base_url
    @user=user
    @pass=pass
  end

  def loadavg
    get("loadavg") { |line| line.split[0..2].map {|x| x.to_f} }
  end

  def cpustat
    process_lines("stat") { |s| s.map {|x| x.to_i} }
  end

  def vmstat
    process_lines("vmstat") { |s| s[0].to_i }
  end

  def meminfo
    process_lines("meminfo") do |s|
      raise "Unhandled multiplier:  #{s[1]}" if s[1] != "kB"
      s[0].to_i * 1024
    end
  end

  private

  def get(f)
    url = URI.parse("#{@base_url}#{f}")
    req = Net::HTTP::Get.new(url.path)
    req.basic_auth @user, @pass if (@user and @pass)
    res=Net::HTTP.new(url.host, url.port).start { |http| http.request(req) }
    case res
      when Net::HTTPSuccess
        #ok
      else
        res.error!
    end

    yield res.body
  end

  def process_lines(f)
    rv={}
    get(f) do |body|
      body.split("\n").each do |l|
      stuff=l.split(/:?\s+/)
      rv[stuff[0]]=yield stuff[1..-1]
      end
    end
    rv
  end

end
