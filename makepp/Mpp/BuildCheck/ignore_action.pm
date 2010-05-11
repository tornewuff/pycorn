# $Id: ignore_action.pm,v 1.5 2009/02/09 22:07:39 pfeiffer Exp $

=head1 NAME

ignore_action -- A build check method that ignores changes in the command.

=head1 USAGE

   build_check ignore_action

=head1 DESCRIPTION

The C<ignore_action> method is exactly like the C<exact_match> method except
that it will not cause a rebuild if the command string changes.  This method
is intended to be used (along with the variable $(changed_inputs)) for
commands that update an output file rather than remake it from scratch.  For
example,

   libxyz.a : *.o : build_check ignore_action
         ar ru $(output) $(changed_inputs)

is one way that you can update an archive.  (There are a number of reasons why
updating an archive can lead to incorrect builds, however.  See the makepp
cookbook for details.)

=cut

use strict;
package Mpp::BuildCheck::ignore_action;

use Mpp::BuildCheck::exact_match;
our @ISA = 'Mpp::BuildCheck::exact_match';

our $ignore_action = bless [1];	# Make the singleton object with options.

1;
