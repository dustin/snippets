#!/usr/bin/env ruby -w

# Make HTML for the monitoring stuff

require 'set'
require 'find'

def analyze_dir(dir="img")
  rv={}
  rv['types']=Hash.new {|c, x| c[x] = Set.new ; c[x]}
  rv['times']=Set.new
  rv['hosts']=Hash.new {|c, x| c[x] = Set.new ; c[x]}
  Find.find(dir) do |f|
    if /\.png$/.match(f)
      fn=f[(dir.length+1)..-5]

      stuff=fn.split(/_/)
      if /^hosts\//.match(fn)
        # It's a host file.  Trim of hosts/ (6 bytes) and add the types
        rv['hosts'][stuff[0][6..-1]] << stuff[1]
      else
        # It's a regular file
        rv['types'][stuff[0]] << stuff[1]
        rv['times'] << stuff[-1]
      end
    end
  end
  rv
end

class HtmlFile

  def initialize(title, imgstuff)
    @title=title
    @imgstuff=imgstuff
  end

  def starthtml
    return <<EOF
<html>
  <head>
    <title>#{@title}</title>
    <link rel="stylesheet" type="text/css" href="style.css"/>
  </head>
<body>
  <h1>#{@title}</h1>
EOF
  end

  def do_body
    return []
  end

  def endhtml
     return %Q{<p><a href="index.html">Top</a></p></body></html>}
  end

  def link_list(links)
    ["<ul>\n"] +
      links.map {|t, u| %Q{  <li><a href="#{u}">#{t}</a></li>\n}} +
      ["</ul>\n"] 
  end

  def to_html
    rv = []
    rv << starthtml
    rv << do_body
    rv << endhtml

    rv.flatten.join("")
  end

end

class Index < HtmlFile
  def do_body
    link_list([['Systems', 'sys.html'], ['Memcached', 'mc.html'],
      ['Rails log', 'rl.html']])
  end
end

class SomeType < HtmlFile
  def initialize(title, imgstuff, type)
    super title, imgstuff
    @type=type
  end
  def do_body
    rv = ["<p>"]
    @imgstuff['types'][@type].each do |subtype|
      rv << %Q{<img src="imgs/#{@type}_#{subtype}_day.png"/>}
    end
    rv << "</p>"

    # Host nav
    rv << link_list(@imgstuff['hosts'][@type].map do |h|
      [h, "#{@type}_#{h}.html"]
      end)
    rv
  end
end

class SomeTypeHost < HtmlFile
  def initialize(title, imgstuff, sysname, type)
    super title, imgstuff
    @sysname=sysname
    @type=type
  end

  def do_body
    rv = ["<p>"]
    @imgstuff['types'][@type].each do |subtype|
      rv << %Q{<img src="imgs/hosts/#{@type}_#{@sysname}_#{@type}_#{subtype}_day.png"/>}
    end
    rv << "</p>"

    # Host nav
    rv << link_list([["All #{@type}", "#{@type}.html"]] +
      @imgstuff['hosts'][@type].map {|h| [h, "#{@type}_#{h}.html"]})
    rv
  end
end

class Rails < HtmlFile
  def do_body
    rv = ["<p>"]
    @imgstuff['types']['rl'].each do |subtype|
      rv << %Q{<img src="imgs/sys_#{subtype}_day.png"/>}
    end
    rv << "</p>"

    # Host nav
    rv << link_list(@imgstuff['hosts']['rl'].map {|h| [h, "rl_#{h}.html"]})
    rv
  end
end

class Memcached < HtmlFile
  def do_body
    @imgstuff['types']['mc'].map {|t| %Q{<img src="imgs/mc_#{t}_day.png"/>} }
  end
end

def write_file(outdir, filename, fileob)
  open(File.join(outdir, filename), "w") do |f|
    f.puts fileob.to_html
  end
end

def mk_html(imgstuff, outdir)
  p imgstuff
  write_file outdir, "index.html", Index.new("Monitoring Stuff", imgstuff)

  # Handle systems
  write_file outdir, "sys.html", SomeType.new("System Stuff", imgstuff, 'sys')
  imgstuff['hosts']['sys'].each do |host|
    write_file(outdir, "sys_#{host}.html",
      SomeTypeHost.new("System stuff for #{host}", imgstuff, host, 'sys'))
  end

  # Memcached
  write_file outdir, "mc.html", Memcached.new("Memcached Stuff", imgstuff)

  # Handle Mongrels
  write_file outdir, "rl.html", SomeType.new("Rails Stuff", imgstuff, 'rl')
  imgstuff['hosts']['rl'].each do |host|
    write_file(outdir, "rl_#{host}.html",
      SomeTypeHost.new("System stuff for #{host}", imgstuff, host, 'rl'))
  end
end

if $0 == __FILE__
  stuff = analyze_dir($*[0] + "/imgs")
  mk_html stuff, $*[0]
end
