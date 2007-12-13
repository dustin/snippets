#!/usr/bin/env ruby -w

require 'gearman'

servers=$*[0]

client = Gearman::Client.new(servers.split(','), 'test')
taskset = Gearman::TaskSet.new(client)
task = Gearman::Task.new 'hostname'
task.on_complete { |h| puts "The hostname was #{h}" }
task.on_status { |n,d| puts "Got #{n}/#{d}" }

taskset.add_task(task)
taskset.wait(15)
