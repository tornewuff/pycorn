# $Id: Vera.pm,v 1.7 2009/02/09 22:45:23 pfeiffer Exp $

=head1 NAME

Mpp::Scanner::Vera - makepp scanner for Vera files

=head1 DESCRIPTION

Scans a Vera file for C<#include>'s.

Tags are:

=over 6

=item user

File scanned due to an #include "filename"  or `include "filename" directive.

=item sys

File scanned due to an #include E<lt>filenameE<gt> directive.

=back

=cut

use strict;
package Mpp::Scanner::Vera;

use Mpp::Scanner::C;
our @ISA = qw/Mpp::Scanner::C/;

sub get_macro {
  if(/\G(\`*)([a-z_]\w*)/igc) {
    my ($ticks, $key) = ($1, $2);
    if($ticks) {
      return (substr($ticks, -1), '`', $key);
    }
    else {
      return ("", "", $key);
    }
  }
  return;
}

sub get_directive {
  if(s/^(\s*\#\s*|\s*\`)(\w+)\s+//) {
    return $2;
  }
  return;
}

1;
