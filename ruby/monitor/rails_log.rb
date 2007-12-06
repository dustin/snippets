#!/usr/bin/env ruby -w

require 'rubygems'
require 'mysql'

require 'rrd_schema'

FIELDS=%w(count req ren db)

class RailsLog

  def initialize(server, username, password, database)
    @connection=Mysql.connect(server, username, password, database)
  end

  def get_times
    rows=@connection.query <<QUERY
      select
          host, pid, count(*),
          sum(request_time), sum(render_time), sum(db_time)
        from logs
        group by host, pid
QUERY
    rv={}
    rows.each do |r|
      rv[r[0] + "." + r[1]] = [r[2].to_i] + r[3..-1].map {|x| x.to_f}
    end
    rv
  end

end

class RailsLogRRD
  include RRDSchema

  def initialize(rl)
    @rl=rl
  end

  def rrd_inserts
    @rl.get_times.each_pair do |fn,v|
      create_rrd(FIELDS, fn + ".rrd", Hash.new(RRDSchema::COUNTER), 300)
      vals=[v[0]] + v[1..-1].map {|x| (x * 1000).to_i}
      send_rrd_cmd "update #{fn} -t #{FIELDS.join(':')} N:#{vals.join(':')}"
    end
  end
end

if $0 == __FILE__
  # p RailsLog.new(*$*).get_times
  RailsLogRRD.new(RailsLog.new(*$*)).rrd_inserts
end
