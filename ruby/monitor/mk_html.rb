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

  def img_link(htmlfile, prefix)
    %Q{<a href="#{htmlfile}"><img alt="#{prefix}" src="imgs/#{prefix}_day.png"/></a>}
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
      rv << img_link("#{@type}_#{subtype}_all.html", "#{@type}_#{subtype}")
    end
    rv << "</p>"

    # Host nav
    rv << link_list(@imgstuff['hosts'][@type].map do |h|
      [h, "#{@type}_#{h}.html"]
      end)
    rv
  end
end

class SomeTypeAll < HtmlFile
  def initialize(title, imgstuff, type, subtype)
    super "#{title} for #{subtype}", imgstuff
    @type=type
    @subtype=subtype
  end
  def do_body
    rv = ["<p>"]
    %w(day week month).each do |t|
      rv << "<h2>#{t}</h2>\n"
      rv << %Q{<img alt="#{t}" src="imgs/#{@type}_#{@subtype}_#{t}.png"/>}
    end
    rv << "</p>"

    # Host nav
    rv << %Q{<p><a href="#{@type}.html">All</a></p>}
  end
end

class SomeTypeHostAll < HtmlFile
  def initialize(title, imgstuff, type, subtype, host)
    super "#{title} for #{subtype} for #{host}", imgstuff
    @type=type
    @subtype=subtype
    @host=host
  end
  def do_body
    rv = ["<p>"]
    %w(day week month).each do |t|
      rv << "<h2>#{t}</h2>\n"
      rv << %Q{<img alt="#{t}" src="imgs/hosts/#{@type}_#{@host}_#{@type}_#{@subtype}_#{t}.png"/>}
    end
    rv << "</p>"

    # Host nav
    rv << %Q{<p><a href="#{@type}_#{@host}.html">All</a></p>}
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
      rv << img_link("host_#{@sysname}_#{@type}_#{subtype}_all.html",
        "hosts/#{@type}_#{@sysname}_#{@type}_#{subtype}")
    end
    rv << "</p>"

    # Host nav
    rv << link_list([["All #{@type}", "#{@type}.html"]] +
      @imgstuff['hosts'][@type].map {|h| [h, "#{@type}_#{h}.html"]})
    rv
  end
end

class Memcached < HtmlFile
  def do_body
    @imgstuff['types']['mc'].map do |t|
      img_link("mc_#{t}_all.html", "mc_#{t}")
    end
  end
end

class MemcachedAll < HtmlFile
  def initialize(title, imgstuff, subtype)
    super "#{title} for #{subtype}", imgstuff
    @type='mc'
    @subtype=subtype
  end
  def do_body
    rv = ["<p>"]
    %w(day week month).each do |t|
      rv << "<h2>#{t}</h2>\n"
      rv << %Q{<img alt="#{t}" src="imgs/#{@type}_#{@subtype}_#{t}.png"/>}
    end
    rv << "</p>"

    # Host nav
    rv << %Q{<p><a href="#{@type}.html">All</a></p>}
  end
end

def write_file(outdir, filename, fileob)
  open(File.join(outdir, filename), "w") do |f|
    f.puts fileob.to_html
  end
end

def mk_html_with_types(imgstuff, outdir, title, type)
  write_file outdir, "#{type}.html", SomeType.new(title, imgstuff, type)

  imgstuff['types'][type].each do |subtype|
    write_file(outdir, "#{type}_#{subtype}_all.html",
      SomeTypeAll.new(title, imgstuff, type, subtype))
  end

  imgstuff['hosts'][type].each do |host|
    write_file(outdir, "#{type}_#{host}.html",
      SomeTypeHost.new("#{title} for #{host}", imgstuff, host, type))

    imgstuff['types'][type].each do |subtype|
      write_file(outdir, "host_#{host}_#{type}_#{subtype}_all.html",
        SomeTypeHostAll.new(title, imgstuff, type, subtype, host))
    end
  end
end

def mk_html(imgstuff, outdir)
  write_file outdir, "index.html", Index.new("Monitoring Stuff", imgstuff)

  # Handle systems
  mk_html_with_types(imgstuff, outdir, "System Stuff", 'sys')

  # Memcached
  write_file outdir, "mc.html", Memcached.new("Memcached Stuff", imgstuff)
  imgstuff['types']['mc'].each do |subtype|
    write_file(outdir, "mc_#{subtype}_all.html",
      MemcachedAll.new("Memcached Stuff", imgstuff, subtype))
  end

  # Handle Mongrels
  mk_html_with_types(imgstuff, outdir, "Rails Stuff", 'rl')
end

if $0 == __FILE__
  stuff = analyze_dir($*[0] + "/imgs")
  mk_html stuff, $*[0]
end
