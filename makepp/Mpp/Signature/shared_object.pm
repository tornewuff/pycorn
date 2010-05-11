# $Id: shared_object.pm,v 1.11 2009/02/09 22:07:39 pfeiffer Exp $
use strict;
package Mpp::Signature::shared_object;

use Mpp::Signature::c_compilation_md5;
our @ISA = 'Mpp::Signature::c_compilation_md5';

=head1 NAME

Mpp::Signature::shared_object -- a signature class that ignores changes to shared objects

=head1 DESCRIPTION

Normally, if you build a shared object, you do not have to relink anything
that depends on that shared object.  This class sets the signature of a shared
object to the same value, regardless of its contents.

For files which are not shared objects, the signature is the same as for C<c_compilation_md5>.

=cut

our $shared_object = bless \@ISA; # Make the singleton object.

# Things that can be overridden by a derived class:
sub build_info_key() { 'SHARED_OBJECT' }

#
# The only subroutine we need to override is the signature method; we use
# exact matching of MD5 signatures.
#
sub signature {
  my ($self, $finfo) = @_;	# Name the arguments.
  if ($finfo->{NAME} =~ /\.s[oal]$/) {
    my $sig = &signature_shared_lib;
    defined($sig) and return $sig;
  }

  &Mpp::Signature::c_compilation_md5::signature;
}

#
# This function calculates the MD5 sum of the interface of a shared library.
# Normally, when a shared library changes, one does not need to relink;
# it's only if the externally visible variables change that a relink might
# be necessary.	 This function computes an MD5 checksum on the externally
# visible symbols.
#
# This will only work with GNU binutils.
#
# This function may trigger rebuilds too often, because not every change that
# affects a shared library's external symbol table requires a rebuild.	But
# it's probably impossible to figure out which changes don't require a rebuild
# without actually doing full symbol resolution.
#
# Arguments:
# a) The singleton object.
# b) The file info for the shared library.
#
sub signature_shared_lib {
  my ($self, $finfo) = @_;

  my $cksum = Mpp::File::build_info_string( $finfo, build_info_key );
  return $cksum if $cksum;	# Don't bother rescanning if the
				# file hasn't changed.	(The build info is
				# discarded if the file date changes, so this
				# flushes our cached signature.)

  open my $nm, 'nm -P ' . Mpp::File::absolute_filename( $finfo ) . ' 2>/dev/null |' or
    return undef;
  local $_;
  my @symbols;
  my $n_symbols = 0;
  while( <$nm> ) {  # Keep reading lines.
    if( s/\s+([A-Z]) .*/$1\01/s ) { # Uppercase seems to more or less cover the
                                 # non-portable Gnu -D opt.  Someone who
                                 # understands this might fine tune the types
                                 # that should really go into the sig --
                                 # portably!
      ++$n_symbols if ord( $1 ) != ord 'U';
      push @symbols, $_;
    }
  }
  return undef unless $n_symbols; # If that didn't work, then fall back to the
				# regular signature.
  $cksum = Digest::MD5::md5_base64( sort @symbols );

  Mpp::File::set_build_info_string( $finfo, build_info_key, $cksum );
  $cksum;
}

1;
