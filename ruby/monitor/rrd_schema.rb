#!/usr/bin/env ruby -w

module RRDSchema
  MAX_NAME_LEN=19

  ROLLUP=['RRA:MIN:0.5:1:4032', 'RRA:AVERAGE:0.5:1:4032', 'RRA:MAX:0.5:1:4032',
    'RRA:MIN:0.5:12:1152', 'RRA:AVERAGE:0.5:12:1152', 'RRA:MAX:0.5:12:1152',
    'RRA:MIN:0.5:288:1825', 'RRA:AVERAGE:0.5:288:1825', 'RRA:MAX:0.5:288:1825']

  GAUGE='DS:#{fieldname}:GAUGE:1800:0:U'
  COUNTER='DS:#{fieldname}:COUNTER:1800:0:1000000'

  def create_rrd(fields, filename, types, poll=60)
    if not File.exists? filename
      rrd_fields=fields.map do |fieldname|
        eval('"' + types[fieldname] + '"')
      end
      recipe=rrd_fields + ROLLUP
      send_rrd_cmd "create #{filename} -s #{poll} #{recipe.join(' ')}"
    end
  end

  def send_rrd_cmd(s)
    puts s
  end

end
