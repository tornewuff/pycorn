# $Id: architecture_independent.pm,v 1.6 2009/02/09 22:07:39 pfeiffer Exp $

=head1 NAME

architecture_independent -- A build check method that ignores changes in architecture

=head1 USAGE

   build_check architecture_independent

=head1 DESCRIPTION

The C<architecture_independent> method is exactly like the C<exact_match>
method except that it will not cause a rebuild if the architecture changes.
This method is intended for architecture independent files.

Some files that you might think are architecture independent are actually not.
For example, the output of solaris lex will not compile on linux, or at least
it wouldn't last time I tried.

=cut

package Mpp::BuildCheck::architecture_independent;

use strict;

use Mpp::BuildCheck::exact_match;
our @ISA = 'Mpp::BuildCheck::exact_match';

our $architecture_independent = bless [0, 1]; # Make the singleton object with options.

1;
