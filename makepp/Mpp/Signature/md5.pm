# $Id: md5.pm,v 1.11 2009/02/10 22:55:50 pfeiffer Exp $
use strict;
package Mpp::Signature::md5;

use Mpp::Signature;
use Digest::MD5;

our @ISA = 'Mpp::Signature';

=head1 NAME

Mpp::Signature::md5 -- Mpp::Signature class based on MD5 checksum

=head1 DESCRIPTION

Unlike the usual signature class, this class computes an MD5 checksum of all
input files.  The date on your source file may change, but if you use this
signature class, makepp will realize that no rebuild is necessary as long as
the contents have not changed.  Just make sure that no build time is actually
written into the files themselves.

For C/C++ sources, Mpp::Signature::c_compilation_md5 is more suitable, since it
ignores comments and whitespace.  That compilation method is used by default
if the statement is recognized as C compilation.

=cut

our $md5 = bless \@ISA;		# Make the singleton object.

#
# The only subroutine we need to override is the signature method; we use
# exact matching of MD5 signatures.
#
sub signature {
  my $finfo = $_[1];		# Name the argument.

  my $stored_cksum = Mpp::File::build_info_string( $finfo, "MD5_SUM");

  unless ($stored_cksum) {	# Don't bother resumming if we
				# already know the answer.

    my $fname = Mpp::File::absolute_filename( $finfo );
    if(-f $fname && open my $infile, $fname) {
#      $Mpp::warn_level and
#	print "Computing MD5 sum of $fname\n";
      my $ctx = Digest::MD5->new;	# Make a context for computing.
      $ctx->addfile( $infile );	# Read in the whole file.
      close $infile;

      # Digest key and format needs to match Mpp::BuildCache::copy_from_cache
      $stored_cksum = $ctx->b64digest;
      Mpp::File::set_build_info_string( $finfo, "MD5_SUM", $stored_cksum);
				# Store the checksum so we don't have to do
				# that again.
    }
    else {			# Can't open the file?
      $stored_cksum = '';
    }
  }

  return $stored_cksum;
}

1;
