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
package BuildCheck::only_action;

use BuildCheck::exact_match;
our @ISA = 'BuildCheck::exact_match';

our $only_action = bless \@ISA; # Make the singleton object.

sub build_check {
  BuildCheck::exact_match::build_check( @_, 0, 1, 1 );
                                # Pass the only_action flag to
                                # exact_match's build check method.
}

sub build_check_from_build_info {
  BuildCheck::exact_match::build_check_from_build_info( @_, 0, 1, 1 );
}

sub build_cache_key {
  BuildCheck::exact_match::build_cache_key( @_, 0, 1, 1 );
}

1;
