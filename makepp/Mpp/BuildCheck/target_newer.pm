# $Id: target_newer.pm,v 1.10 2009/02/10 22:55:49 pfeiffer Exp $
use strict;
package Mpp::BuildCheck::target_newer;

use Mpp::BuildCheck;
our @ISA = qw(Mpp::BuildCheck);

=head1 NAME

Mpp::BuildCheck::target_newer -- a build check class that uses the traditional Make algorithm

=head1 USAGE

This is the default build check class for a few special cases, e.g., for rules
that build Makefile or Makefile.in.  Otherwise, if you want to use it, you
must specify it on the command line or explicitly in rules:

   target : dependencies
	  : build_check target_newer
	$(commands_to_build)

=head1 DESCRIPTION

This class forces a recompilation if the target is older than any of its
dependencies.  It also does not require the command to be the same as on the
last build, nor does it it make sure that the architecture is the same.	 This
is generally not a reliable way to build things, but it is occasionally useful
for special purpose things where a target may be modified by some commands not
executed under make's control.	For example, if you want your makefile to pull
the latest version out of an RCS archive, but only if the RCS archive is more
recent, you could have a rule like this:

   %: %,v
    : build_check target_newer
	co $(FIRST_DEPENDENCY)

If you did not add the "C<:build_check target_newer>", the rule would not work
as expected.  If you checked the file out of the RCS archive, then modified
it, B<makepp>'s default rule would notice that the file's signature did not
match the signature from the last build, so it would check it out for you
again.

=cut

our $target_newer = bless \@ISA;	# Make the singleton object.

my $already_warned_about_signature_methods = 0;

sub build_check {
  my ($self, $tinfo, $sorted_dependencies, $command_string,
      $build_cwd, $sig_method, $env) = @_;
			        # Ignore the arguments we don't need.
  warn "signature methods for the target_newer build_check method are ignored\n"
    if $sig_method != $Mpp::Signature::signature &&
    !$already_warned_about_signature_methods++;

  if( !Mpp::File::file_exists $tinfo ) {	# If there's no target, then we need to
				# rebuild.
    Mpp::log BUILD_NONE => $tinfo
      if $Mpp::log_level;
    return 1;
  }

  my @changed_deps = $self->changed_dependencies($tinfo, $sig_method, $build_cwd, @$sorted_dependencies);

  if (@changed_deps) {
    Mpp::log BUILD_OLD => $tinfo, \@changed_deps
      if $Mpp::log_level;
    return "DEPENDENCIES";
  }

  Mpp::log UP_TO_DATE => $tinfo
    if $Mpp::log_level;
  return undef;			# No rebuild necessary.
}

sub build_check_from_build_info {
  my ($self, $rep_entry, $sorted_dependencies, $build_cwd, undef, $env) =
    @_;
			        # Ignore the arguments we don't need.
#
# See if this file is newer than all of the dependencies.  That's all we can
# check for this file with this signature method.
#
  my $target_mtime = Mpp::File::build_info_string( $rep_entry, 'SIGNATURE' );
  $target_mtime =~ s/,.*//;	# Strip out the file size, leaving the file
                                # date.
  foreach my $dep (@$sorted_dependencies) {
    return 1 if Mpp::File::file_mtime( $dep ) > $target_mtime;
  }
  return undef;
}

#
# Return a list of the dependencies that have changed:
#
sub changed_dependencies {
  my ($self, $target, undef, $build_cwd, @dependencies) = @_;
  my( @changed, $mtime );

  foreach (@dependencies) {
    next if Mpp::File::assume_unchanged( $_ );
    push @changed, $_
      if $_->{ASSUME_CHANGED} ||
	Mpp::File::file_mtime( $_ ) > ($mtime ||= Mpp::File::file_mtime $target);
  }

  return @changed;
}

1;
