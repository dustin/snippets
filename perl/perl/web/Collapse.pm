# Copyright (c) 1997  Dustin Sallings
#
# $Id: Collapse.pm,v 1.1 1997/12/31 08:43:46 dustin Exp $

package Collapse;

use strict;

sub new
{
    shift;
    my($a, $h)=@_;
    my $self = {};

    $self->{ary}=[$a];
    $self->{expand}=$h;

    foreach (keys(%{$self->{expand}}))
    {
	print "Will expand $_\n";
    }

    bless($self);
    return($self);
}

# Return the database handler for Postgres.

sub get_ary
{
    my($self)=shift;
    return($self->{ary});
}

sub print_ary_text
{
    my($self)=shift;

    _show_array_text($self, "", -1, $self->{ary});
}

sub print_ary_html
{
    my($self)=shift;

    _show_array_html($self, "", -1, $self->{ary});
}

sub _show_array_html
{
    my($self, $prepend, $depth, $ary)=@_;
    my($test, $label, $pad);
    my(%expand)=%{$self->{expand}};

    if(ref($ary))
    {
	$test=$prepend;
	if( ( $test eq "" ) || defined($expand{$test}))
	{
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
                print "$pad<li>$label<br>\n$pad<ul>\n";
	    }

	    foreach(0..@{$ary}-1)
	    {
	        _show_array_html($self, ($prepend eq "" ? $_ : "$prepend:$_"),
		    $depth+1, @{$ary}[$_]);
	    }

            print "$pad</ul></li>\n";
	}
	else
	{
	    print "<!-- Not expanding $test -->\n";
	}
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
    my($test, $label);
    my(%expand)=%{$self->{expand}};

    if(ref($ary))
    {
	$test=$prepend;
	if( ( $test eq "" ) || defined($expand{$test}))
	{
	    if($depth>=0)
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
	        _show_array_text($self, ($prepend eq "" ? $_ : "$prepend:$_"),
		    $depth+1, @{$ary}[$_]);
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

use Collapse;     # Core Billit libraries (includes Postgres.pm and CGI.pm)

=head1 METHODS

=item new( [@a], [%h] )

    Create a new collapse object using array @a for the list of lists to
create the boxes, and %h for the list to expand.

=item get_ary

    Return the array.

=item print_ary_text

    Print the array in text format.

=item print_ary_html

    Print the array as html unordered lists.

=cut
