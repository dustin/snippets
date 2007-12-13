#!/usr/bin/env ruby -w

require 'gearman'

servers=$*[0]

worker = Gearman::Worker.new(servers.split(','), 'test')
worker.add_ability('hostname') do |data,job|
  1.upto(10) do |x|
    job.report_status(x, 10)
    sleep(1)
  end
  `hostname`
end

loop { worker.work }
