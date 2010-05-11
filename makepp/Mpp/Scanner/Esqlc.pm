# $Id: Esqlc.pm,v 1.10 2009/02/09 22:45:23 pfeiffer Exp $

=head1 NAME

Mpp::Scanner::Esqlc - makepp scanner for Embedded SQL C files

=head1 DESCRIPTION

Scans a C file for C<EXEC SQL INCLUDE>s, C<$include>s and C<#include>s.

Tags are:

=over 6

=item user

File scanned due to an EXEC SQL INCLUDE "filename" or $INCLUDE "filename"
directive.

=item sys

File scanned due to an EXEC SQL INCLUDE E<lt>filenameE<lt>, EXEC SQL INCLUDE
filename or $INCLUDE E<lt>filenameE<lt> directive.

=item sql


=back

=cut

use strict;
package Mpp::Scanner::Esqlc;

use Mpp::Scanner::C;
our @ISA = 'Mpp::Scanner::C';

sub get_directive {
  if( s/^\s*(?:EXEC\s+SQL\s+|\$\s*)INCLUDE\s+//i ) {
    'sql';
  } elsif( s/^\s*EXEC\s+ORACLE\s+// ) {
      if( s/^(DEFINE|IFN?DEF)(\s+\w+)\s*;/$2/i || s/^(ELSE|ENDIF)\s*;//i ) {
	lc $1;
      }
  } else {
    &Mpp::Scanner::C::get_directive;
  }
}

sub other_directive {
  my( $self, $cp, $finfo, $conditional, $tag, $scanworthy ) = @_;
  return 0 unless $tag eq 'sql';
  $_ = $self->expand_macros($_) if $conditional;
  $$scanworthy = 1;
  if( s/^(<)(.+?)>\s*;?\s*$// or s/^(['"]?)(.+?)\1\s*;?\s*$// ) {
    $tag = ($1 eq '<') ? 'sys' : 'user';
    my $file = $1 ? $2 : lc $2; # downcase unquoted file
    $self->include( $cp, $tag, $file, $finfo )
      or undef;
  }
}

1;
