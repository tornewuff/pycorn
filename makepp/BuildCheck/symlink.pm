=head1 NAME

symlink -- A build check method that checks if target points to dependency.

=head1 USAGE

   build_check symlink

=head1 DESCRIPTION

The C<symlink> method will only execute the action if the target is not a
symlink to the dependency.  It is exactly like the C<exact_match> method when
it comes to repositories or build_cache.  For example,

   $(ROOT)/include/%.h : %.h : build_check symlink
         &ln -fr $(input) $(output)

publishes a file, but does not repeat this when the file changes (i.e. a
generated header).

=cut

package BuildCheck::symlink;

warn "`:build_check symlink' is deprecated, makepp now automatically handles symlinks.\n";

use strict;

use BuildCheck::exact_match;
our @ISA = 'BuildCheck::exact_match';

our $symlink = bless \@ISA;	# Make the singleton object.

sub build_check {
  # TODO: We need a means of finding the first (or better yet, parametrizably
  # any) explicit dependency.  But our arg is alas sorted, and when we get
  # called the rule doesn't (yet?) contain this info.  Maybe this function
  # should precalculate it?
  my @deps = grep !exists $_->{IS_PHONY}, @{$_[2]};
  @deps == 1 or die 'build_check symlink needs exactly one non-phony dependency to check ',
    FileInfo::absolute_filename( $_[1] ), " against\n";
  FileInfo::stat_array $_[1];	# This sets LINK_DEREF if needed.
  not $_[1]->{LINK_DEREF} && $_[1]->{LINK_DEREF} == $deps[0];
				# Either not a symlink, or a different one.
}

1;
