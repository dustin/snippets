#!/usr/bin/env ruby -w

require 'rubygems'
require 'MemCache'

IGNORED_STATS=%w(pid time version)
DECIMAL_STAT=%w(rusage_system rusage_user)
MAX_NAME_LEN=19

ROLLUP=['RRA:MIN:0.5:1:4032', 'RRA:AVERAGE:0.5:1:4032', 'RRA:MAX:0.5:1:4032',
  'RRA:MIN:0.5:12:1152', 'RRA:AVERAGE:0.5:12:1152', 'RRA:MAX:0.5:12:1152',
  'RRA:MIN:0.5:288:1825', 'RRA:AVERAGE:0.5:288:1825', 'RRA:MAX:0.5:288:1825']

GAUGE='DS:#{fieldname}:GAUGE:1800:0:U'
COUNTER='DS:#{fieldname}:COUNTER:1800:0:1000000'

# Set up the types by item type.  Default is counter, but there are some
# specific gauges.
TYPES=Hash.new COUNTER
%w(curr_items total_items bytes curr_connections total_connections
  connection_structures limit_maxbytes).each { |s| TYPES[s]=GAUGE }

class MemCacheRRD

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
      rrd_create_one fields, server, stats if not File.exists? fn
      puts "update #{fn} -t #{fields.join(':')} N:#{stats.join(':')}"
    end
  end

  def rrd_create
    stat_stuff {|f,s,st| rrd_create_one f, s, st}
  end

  private

  def rrd_create_one(fields, server, stats)
      if not File.exists? fname(server)
        rrd_fields=fields.map do |fieldname|
          eval('"' + TYPES[fieldname] + '"')
        end
        recipe=rrd_fields + ROLLUP
        puts "create #{fname server} -s 300 #{recipe.join(' ')}"
      end
  end

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
