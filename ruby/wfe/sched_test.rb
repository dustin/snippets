#!/usr/bin/env ruby -w

require 'rubygems'
require 'openwfe/util/scheduler'

scheduler = OpenWFE::Scheduler.new
scheduler.start

scheduler.schedule_every('3s') do
  puts "hello!"
end

# Wait for it...
scheduler.join
