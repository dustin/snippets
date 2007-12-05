#!/usr/bin/env ruby -w

require 'rubygems'
require 'MemCache'

require 'rrd_schema'

IGNORED_STATS=%w(pid time version)
DECIMAL_STAT=%w(rusage_system rusage_user)

# Set up the types by item type.  Default is counter, but there are some
# specific gauges.
TYPES=Hash.new RRDSchema::COUNTER
%w(curr_items total_items bytes curr_connections total_connections
  connection_structures limit_maxbytes).each { |s| TYPES[s]=RRDSchema::GAUGE }

class MemCacheRRD

  include RRDSchema

  def initialize(servers)
    @servers=servers
  end

  def stat_stuff
    stats=MemCache.new(@servers).stats
    # Prune unwanted stats
    fields=stat_keys(stats).reject {|k| IGNORED_STATS.include? k}
    # Adjust the names so they're all short enough
    short_fields=fields.map {|fn| fn.gsub(/connection/, 'conn') }

    stats.each_pair do |server, stats|
      yield short_fields, server, fields.map {|k| stat_find(stats, k)}
    end
  end

  def rrd_inserts
    stat_stuff do |fields, server, stats|
      fn=fname server
      # Optionally create one
      create_rrd fields, fname(server), TYPES
      send_rrd_cmd "update #{fn} -t #{fields.join(':')} N:#{stats.join(':')}"
    end
  end

  def rrd_create
    stat_stuff {|f,s,st| create_rrd f, fname(s), TYPES}
  end

  private

  def stat_keys(all_stats)
    # all_stats is a hash of {server_name => {stat_key => stat_value}}
    all_stats.to_a[0][1].keys
  end

  def fname(server)
    server.split(/:/)[0] + '.rrd'
  end

  def stat_find(stats, k)
    if DECIMAL_STAT.include? k
      (stats[k].to_f * 100).to_i
    else
      stats[k]
    end
  end

end

if $0 == __FILE__
  MemCacheRRD.new($*).rrd_inserts
end
