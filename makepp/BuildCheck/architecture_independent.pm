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

package BuildCheck::architecture_independent;

use strict;

use BuildCheck::exact_match;
our @ISA = qw(BuildCheck::exact_match);

our $architecture_independent = bless \@ISA;

sub build_check {
  BuildCheck::exact_match::build_check( @_, 0, 1 );
                                # Pass the ignore_architecture flag to
                                # exact_match's build check method.
}

sub build_check_from_build_info {
  BuildCheck::exact_match::build_check_from_build_info( @_, 0, 1 );
}

sub build_cache_key {
  BuildCheck::exact_match::build_cache_key( @_, 0, 1 );
}

1;
