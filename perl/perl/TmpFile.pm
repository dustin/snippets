# Generic database object.
#
# $Id: TmpFile.pm,v 1.1 2000/05/27 08:28:34 dustin Exp $

=pod

=head1 NAME

Bynd::TmpFile - Routines to obtain temporary files and names

=head1 SYNOPSIS

  use Bynd::TmpFile;

  $tmpfile=Bynd::TmpFile->new;

  - or -

  use Bynd::TmpFile;

  # Use this method only when absolutely necessary
  $tmpfilename Bynd::TmpFile->new_name;

=head1 EXAMPLE

  use Bynd::TmpFile;

  $tmpfile=Bynd::TmpFile->new;

  print $tmpfile "blah!\n";

  # Rewind the file
  $tmpfile->seek(0, 0);

  print <$tmpfile>;


=head1 DESCRIPTION

This module provides a more secure mechanism for temporary files and
filenames.

=cut

use IO::File;
use MD5;
use strict;


{
	package SPY::TmpFile;

	# Call count
	use vars qw($COUNT);
	$COUNT=0;

	sub new {
		return(IO::File->new_tmpfile);
	}

	sub new_name {
		my($md, $fn);
		$md=MD5->new;$md=MD5->new;
		$md->add($<);
		$md->add($();
		$md->add($>);
		$md->add($));
		$md->add($^T);
		$md->add($!);
		$md->add($0);
		$md->add($COUNT);
		$md->add($$);
		$md->add(rand());
		$md->add(time());

		# Increase call count
		$COUNT++;

		$fn="/tmp/tmpfile." . $md->hexdigest . "$$";
		return($fn);
	}
}

1;

__END__
=pod

=back

=head1 AUTHOR

Dustin Sallings <dustin@beyond.com>

=head1 VERSION

$Id: TmpFile.pm,v 1.1 2000/05/27 08:28:34 dustin Exp $
