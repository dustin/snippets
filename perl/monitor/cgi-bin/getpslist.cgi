#!/usr/local/bin/perl
# $Id: getpslist.cgi,v 1.2 1997/12/14 21:31:44 dustin Exp $

push(@INC, "/home/monitor/lib");
require 'statlib.pl';

sub bything
{
    my(@a, @b);

    @a=(@{$a});
    @b=(@{$b});

    return($a[1] <=> $b[1]);
}

sub domachine
{
    my($machine, %h)=@_;
    my(@a, $thing);

    print <<EOF;
<a name="$machine"></a>
<h3>Process List For $machine</h3>

<table border="1">

<tr>
    <td>PID</td>
    <td>Parent</td>
    <td>User</td>
    <td>Command</td>
</tr>
EOF

    foreach $thing (sort bything (@{$h{$machine}}))
    {
        @a=(@{$thing});

	print "<tr><td>$a[1]</td><td>$a[2]</td>";
        print "<td>$a[0]</td><td>$a[7]</td></tr>\n";
    }

    print <<EOF;

</table>
EOF
}

@machines=("arthur");

print <<EOF;
Content-type: text/html

<ul>
EOF

%h=&pslistInit($machine);

foreach $machine (keys(%h))
{
    print "<li><a href=\"#$machine\">$machine</a></li>\n";
}

print "</ul>\n";

foreach $machine (keys(%h))
{
    domachine($machine, %h);
}
