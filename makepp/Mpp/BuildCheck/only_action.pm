# $Id: only_action.pm,v 1.3 2009/02/09 22:07:39 pfeiffer Exp $

=head1 NAME

only_action -- A build check method that only looks at the command.

=head1 USAGE

   build_check only_action

=head1 DESCRIPTION

The C<only_action> method will only cause a rebuild if the command string
changes.  This method is intended to be used for commands that create an
output file based on the names of their inputs, rather than the contents, like
C<&echo $(inputs) -o $(output)> or C<&ln -fr $(input) $(output)> .

=cut

use strict;
package Mpp::BuildCheck::only_action;

use Mpp::BuildCheck::exact_match;
our @ISA = 'Mpp::BuildCheck::exact_match';

our $only_action = bless [0, 1, 1]; # Make the singleton object with options.

1;
