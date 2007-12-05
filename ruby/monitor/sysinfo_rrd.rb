#!/usr/bin/env ruby -w

require 'net/http'

require 'rrd_schema'

# Get Linux proc info via http
class LinuxHostInfo

  attr_reader :base_url

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

  def all_stats
    l1, l5, l15 = self.loadavg
    rv={'load1' => l1, 'load5' => l5, 'load15' => l15}
    [:cpustat, :vmstat, :meminfo].each do |which|
      h=self.send(which)
      rv.update(h)
    end
    return rv
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

RRD_TYPES=Hash.new RRDSchema::COUNTER
%w(load1 load5 load15).each { |s| RRD_TYPES[s]=RRDSchema::GAUGE }

CPU_KEYS=%w(cpu_user cpu_nice cpu_sys cpu_idle cpu_iow cpu_irq cpu_softirq)
COPIED_KEYS=%w(ctxt SwapFree SwapTotal Active MemFree
  pswpout pswpin pgpgin pgpgout)
LOAD_KEYS=%w(load1 load5 load15)

ALL_KEYS=CPU_KEYS + COPIED_KEYS + LOAD_KEYS

class LinuxHostRRD
  include RRDSchema

  def initialize(lh)
    @filename=URI.parse(lh.base_url).host + ".rrd"
    @lh=lh
  end

  def rrd_inserts
    h=all_stats_rrd
    vals=ALL_KEYS.map { |k| h[k] }

    create_rrd ALL_KEYS, @filename, RRD_TYPES
    send_rrd_cmd "update #{@filename} -t #{ALL_KEYS.join(':')} N:#{vals.join(':')}"
  end

  def all_stats_rrd
    h=@lh.all_stats
    rv=Hash[*(CPU_KEYS.zip(h['cpu'])).flatten]
    COPIED_KEYS.each { |k| rv[k] = h[k].instance_of?(Array) ? h[k][0] : h[k] }
    LOAD_KEYS.each { |k| rv[k] = (h[k] * 100).to_i }
    rv
  end
end

if $0 == __FILE__
  lh=LinuxHostInfo.new('http://repo.dev.caring.com:8182/', 'rrd', 'rrdt00l')
  h=LinuxHostRRD.new(lh)
  h.rrd_inserts
end
