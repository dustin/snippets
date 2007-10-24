#!/usr/bin/env ruby -w

require "generator"
require "histo"
require "PP"

h = Hash.histo Generator.new{|g| ObjectSpace.each_object{|o| g.yield(o.class) }}

PP.pp h
