#!/usr/bin/perl
# $Id: scandisk.cgi,v 1.1 1997/12/12 21:36:01 dustin Exp $

push(@INC, "/home/monitor/lib");
require "statlib.pl";

sub pulldisk
{
    my($host, $disk, $open)=@_;

    if($open==1)
    {
        $SIG{'ALRM'}= 'timeout';
        &openhost($host, 6013);
    }

    print S "disk $disk\r\n";

    return(split(/;/, <S>));
}

sub getdiskinfo
{
    my($n, $warn, $crit, $warntype, $crittype, $against, $disk, $host);
    my($i, $j, $open);
    my(@lol, @a, @append, @misc);

    @lol=list_diskinfos();

    for $i ( 0.. $#lol)
    {
        $host=$lol[$i][0][0];
        print <<EOF;
<table border="3">
<tr><th colspan="5">$host</th></tr>
<tr><td>Mounted on</td><td>Total</td><td>Used</td><td>Free</td><td>% used</td>
</tr>
EOF

        # Start loop of disks for a particular host
        for $j (0 .. $#{$lol[$i]} )
        {
            ($disk, $host, $warn, $crit)=@{$lol[$i][$j]};

            # This is to tell whether to open a connection to the host or not
            if($j==0)
            {
                $open=1;
            }
            else
            {
                $open=0;
            }

            # Go get the info
            @a=pulldisk($disk, $host, $open);

            # Set warn information
            $warn=~/([0-9]+)(.)/;
            $warn=$1;
            $warntype=$2;
            if($warntype=~/[Mm]/)
            {
                $warn *= 1024;
                $warntype="k";
            }

            # Set critical information
            $crit=~/([0-9]+)(.)/;
            $crit=$1;
            $crittype=$2;
            if($crittype=~/[Mm]/)
            {
                $crit *= 1024;
                $crittype="k";
            }

            # Clean out arrays.
            @append=();
            for(0..$#a)
            {
                $misc[$_]="";
            }


            # Set field to show warnings
            if($warntype eq "%")
            {
                $against=4;

                # Set colors for warnings.
                if($a[$against] >= $warn)
                {
                    if($a[$against] >= $crit)
                    {
                        $misc[$against]="bgcolor=\"#ff0000\"";
                        $append[$against]="(crit at $crit$crittype)";
                    }
                    else
                    {
                        $misc[$against]="bgcolor=\"#ffff7f\"";
                        $append[$against]="(warn at $warn$warntype)";
                    }
                }

            }
            else
            {
                $against=3;

                # Set colors for warnings.
                if($a[$against] <= $warn)
                {
                    if($a[$against] <= $crit)
                    {
                        $misc[$against]="bgcolor=\"#ff0000\"";
                        $append[$against]="(crit at $crit$crittype)";
                    }
                    else
                    {
                        $misc[$against]="bgcolor=\"#ffff7f\"";
                        $append[$against]="(warn at $warn$warntype)";
                    }
                }
            }

            # Display the info
            print "<tr>";
            for(0..$#a)
            {
                print "<td $misc[$_]>$a[$_]$thing[$_] $append[$_]</td>";
            }
            print "</tr>\n";
        }

        # end of host.  End table, end connection.
        print "</table></p>\n\n";
        print S "quit\r\n";
        close(S);
    }
}

print <<EOF;
Content-type: text/html

EOF

&getdiskinfo();
