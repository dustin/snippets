#!/usr/bin/env ruby -w

module RRDSchema
  MAX_NAME_LEN=19

  ROLLUP=['RRA:MIN:0.5:1:4032', 'RRA:AVERAGE:0.5:1:4032', 'RRA:MAX:0.5:1:4032',
    'RRA:MIN:0.5:12:1152', 'RRA:AVERAGE:0.5:12:1152', 'RRA:MAX:0.5:12:1152',
    'RRA:MIN:0.5:288:1825', 'RRA:AVERAGE:0.5:288:1825', 'RRA:MAX:0.5:288:1825']

  GAUGE='DS:#{fieldname}:GAUGE:1800:0:U'
  COUNTER='DS:#{fieldname}:COUNTER:1800:0:1000000'

  def create_rrd(fields, filename, types, poll=300)
    if not File.exists? filename
      rrd_fields=fields.map do |fieldname|
        eval('"' + types[fieldname] + '"')
      end
      recipe=rrd_fields + ROLLUP
      puts "create #{filename} -s #{poll} #{recipe.join(' ')}"
    end
  end

end
