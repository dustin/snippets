# Copyright (c) 1997  Dustin Sallings
#
# $Id: Collapse.pm,v 1.5 1997/12/31 09:51:50 dustin Exp $

package Collapse;

use strict;
use CGI;

sub new
{
    shift;
    my($a, $h)=@_;
    my $self = {};

    $self->{ary}=[$a];
    $self->{expand}=$h;
    $self->{html_expand}="Expand";
    $self->{html_collapse}="Collapse";

    # foreach (keys(%{$self->{expand}}))
    # {
    #    print "Will expand $_\n";
    # }

    bless($self);
    return($self);
}

# Return the database handler for Postgres.

sub get_ary
{
    my($self)=shift;
    return($self->{ary});
}

sub set_html_expand
{
    my($self)=shift;
    $self->{html_expand}=shift;
}

sub set_html_collapse
{
    my($self)=shift;
    $self->{html_collapse}=shift;
}

sub print_text
{
    my($self)=shift;

    _show_array_text($self, "", -1, $self->{ary});
}

sub print_html
{
    my($self)=shift;

    _show_array_html($self, "", -1, $self->{ary});
}

sub _show_array_html
{
    my($self, $prepend, $depth, $ary)=@_;
    my($test, $label, $pad, $url, $q);
    my(%expand)=%{$self->{expand}};

    $q=CGI->new;

    if(ref($ary))
    {
        $test=$prepend;
        $pad="";
        if($depth>=0)
        {
            for(1..$depth)
            {
                $pad.="    ";
            }

            if(ref($ary->[0]))
            {
                $label="";
            }
            else
            {
                $label=shift(@{$ary});
            }
        }

        if( ( $test eq "" ) || defined($expand{$test}))
        {
	    delete($expand{$test});
	    $url =$q->url . "?expand=" . join(',', keys(%expand));
	    $expand{$test}=1;
	    $url="<a href=\"$url\">$self->{html_collapse}</a>";
            print "$pad<li>$url \&nbsp; $label<br>\n" if($depth>=0);

	    print "$pad<ul>\n" if($depth>=0);
            foreach(0..@{$ary}-1)
            {
		if((ref($ary->[$_]) eq "ARRAY") || length($ary->[$_]))
		{
                    _show_array_html($self,
			($prepend eq "" ? $_ : "$prepend:$_"),
                        $depth+1, @{$ary}[$_]);
		}
            }
	    print "$pad</ul>\n" if($depth>=0);
        }
        else
        {
	    $url =$q->url . "?expand=" . join(',', keys(%expand), $test);
	    $url="<a href=\"$url\">$self->{html_expand}</a>";
            print "$pad<li>$url \&nbsp; $label<br>\n" if($depth>=0);
        }
	print "$pad</li>\n" if($depth>=0);
    }
    else
    {
        for(1..$depth)
        {
            print "    ";
        }
        print "<li>$ary</li>\n";
    }
}

sub _show_array_text
{
    my($self, $prepend, $depth, $ary)=@_;
    my($test, $label, $tmp);
    my(%expand)=%{$self->{expand}};

    if(ref($ary))
    {
        $test=$prepend;
        if( ( $test eq "" ) || defined($expand{$test}))
        {
            if($depth>0)
            {
                for(1..$depth)
                {
                    print "    ";
                }
                if(ref($ary->[0]))
                {
                    $label="Unknown";
                }
                else
                {
                    $label=shift(@{$ary});
                }
                print "** $label **\n";
            }

            foreach(0..@{$ary}-1)
            {
		if($depth>0)
		{
		    $tmp=($prepend eq "" ? $_ : "$prepend:$_");
		}
		else
		{
		    $tmp="";
		}
                _show_array_text($self, $tmp, $depth+1, @{$ary}[$_]);
            }

            if($depth>=0)
            {
                for(1..$depth)
                {
                    print "    ";
                }
                print "**close**\n";
            }
        }
        else
        {
            print "Not expanding $test\n";
        }
    }
    else
    {
        for(1..$depth)
        {
            print "    ";
        }
        print "$ary";
    }
}

1;

__END__

=head1 NAME

Collapse.pm - Expandolapse

=head1 USAGE

use Collapse;

=head1 REQUIREMENTS

This was built under perl 5.004, but may not require it.  It does,
however, require CGI.pm (you have that anyway, right?)

=head1 METHODS

=item new( [@a], [%h] )

    Create a new collapse object using array @a for the list of lists to
create the boxes, and %h for the list to expand.

    The data in @a is lists of lists.  The first element can be plain
text, and will be treated as the label for the section, however, this is
optional, depending on your needs.

=item get_ary

    Return the array.

=item print_text

    Print the array in text format.

=item print_html

    Print the array as html unordered lists.

=item set_html_expand($text)

    Set the text for the Expand button in HTML output.

=item set_html_collapse($text)

    Set the text for the Collapse button in HTML output.

=cut
