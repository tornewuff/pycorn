# $Id: exact_match.pm,v 1.28 2008/07/30 23:17:01 pfeiffer Exp $
use strict;
package BuildCheck::exact_match;

use BuildCheck;
use FileInfo;
use Signature;

our @ISA = 'BuildCheck';

=head1 NAME

BuildCheck::exact_match -- a signature class that requires that dependencies
have exactly the same signature they did on the last build

=head1 USAGE

This is the default build check class.  You don't have to do anything to use
it unless you override it from the command line.  You can specify it
explicitly in the Makefile using

   target : dependencies
	  : build_check exact_match
	$(commands_to_build)


=head1 DESCRIPTION

This class forces a recompilation if any of the following are different from
the last time this particular rule was executed:

=over 4

=item *

The build command.

=item *

The architecture this is running on (e.g., i386, sparc, alpha, etc.).

=item *

The signatures of each dependency.

=item *

The values of the each envirnoment variable in the list of environmental
dependencies.

=back

If you want to avoid checking some of those things, you can implement
another BuildCheck class in terms of this class by passing additional
optional arguments to the build_check, build_check_from_build_info, and
build_cache_key methods as follows:

=over 4

=item 1. $ignore_action

Don't force a recompilation if only the build command changed.

=item 2. $ignore_architecture

Don't force a recompilation if only the architecture changed.

=back

It is strongly recommended that you pass the base argument list as C<@_>,
because future versions of makepp may append additional parameters to the
argument list.  See F<BuildCheck/architecture_independent.pm> for an example.

=cut

my $last_compilation_warning_x86 = 0;

our $exact_match = bless \@ISA; # Make the singleton object.

#
# See documentation in Signature.pm for details of the arguments and
# intended side effects.
#
# This subroutine takes two additional arguments, $ignore_action, and
# $ignore_architecture.  These will never be used when it is called from
# the main build code.  They are intended to be used by a derived class
# whose build_check method calls this class's build_check method.
#
sub _check_env {
  my( $tinfo, $env, $log_level ) = @_;
  my( $env_deps, $env_vals ) =
    FileInfo::build_info_string( $tinfo, qw(ENV_DEPS ENV_VALS) );
  my( @old_env_list, @old_env_vals );
  if( defined $env_deps ) {
    @old_env_list = split /\01/, $env_deps, -1;
    @old_env_vals = split /\01/, $env_vals, -1;
  }
  @old_env_vals = '' if !@old_env_vals && @old_env_list == 1; # There is no delimiter when the single value is the empty string.
  my @new_env_list = sort keys %$env;
  my @new_env_vals = @{$env}{@new_env_list};
  my @save_env_list = @new_env_list;
  while(@old_env_list || @new_env_list) {
    my ($old, $new) = (shift(@old_env_list), shift(@new_env_list));
    if($old && $new) {
      if( $old lt $new ) {
        undef $new;
      } elsif( $old gt $new ) {
        undef $old;
      }
    }
    if( $log_level ) {
      ::log BUILD_ENV_DEL => $tinfo, $old
	if !$new;
      ::log BUILD_ENV_ADD => $tinfo, $new
	if !$old;
    }
    return 1 if !$new || !$old;
  }
  die unless @new_env_vals == @old_env_vals && @old_env_vals == @save_env_list;
  while(@save_env_list) {
    my $name = shift(@save_env_list);
    my ($old, $new) = (shift(@old_env_vals), shift(@new_env_vals));
    if( $old ne $new ) {
      ::log BUILD_ENV => $tinfo, $name, $new, $old
	if $log_level;
      return 1;
    }
  }
  return;
}
sub build_check {
  my (
    undef, $tinfo, $sorted_dependencies, $command_string, $build_cwd,
    $sig_method, $env, $ignore_action, $ignore_architecture, $only_action
  ) = @_;

#
# Do some quick checks that depend on the targets alone:
# Note that we don't have to check the signature, because if it doesn't match,
# the build info file will be invalid anyway.  See FileInfo::load_build_info.
#
  my( $last_cmd, $arch, $sorted_deps, $dep_sigs, $symlink ) =
    FileInfo::build_info_string( $tinfo, qw(COMMAND ARCH SORTED_DEPS DEP_SIGS SYMLINK) );
  if( !defined $tinfo->{BUILD_INFO} ) {
    ::log BUILD_NONE => $tinfo
      if $::log_level;
    return 1;
  }
  if( !%{$tinfo->{BUILD_INFO}} && !$only_action ) {
    ::log BUILD_INVALID => $tinfo
      if $::log_level;
    return 1;
  }

  if( !$ignore_action and !$last_cmd || $command_string ne $last_cmd ) {
    ::log BUILD_CMD => $tinfo, $last_cmd, $command_string
      if $::log_level;

    return 1;
  }

#
# We used to make sure the build cwd is the same, but we don't bother any
# more because usually if the build cwd changes, the build command has to
# change too.
#

  $arch ||= '';			# SYMLINK only gets loaded if it is still valid.
  if( !$symlink && !$ignore_architecture && ::ARCHITECTURE ne $arch ) {
#
# The pentium architectures are all more or less equivalent, but have different
# architecture flags.  Give a warning (so at least the user is not surprised
# about recompilation).
#
    warn "last compilation was on the $arch architecture,
  and this is on " . ::ARCHITECTURE . ".
  These are technically different and force a recompilation of everything,
  but this may not be what you want.  The difference is most likely caused
  by running a different copy of perl.\n"
      if $::warn_level &&
      ::ARCHITECTURE =~ /^i[34567]86-linux/ && $arch =~ // &&
      !$last_compilation_warning_x86++;

    ::log BUILD_ARCH => $tinfo, $arch, ::ARCHITECTURE
      if $::log_level;
    return 1;
  }

  return undef if $only_action || $symlink;

# Check the environmental dependencies
  return 1 if _check_env $tinfo, $env, $::log_level;

#
# If we get here, we have to scan through all of the dependencies
# to see if any of them has changed.
#
  my @old_dep_list = map
    exists $build_cwd->{DIRCONTENTS} && $build_cwd->{DIRCONTENTS}{$_} || FileInfo::path_file_info( $_, $build_cwd ),
    split /\01/, $sorted_deps;

  if (@old_dep_list != @$sorted_dependencies) { # Different # of dependencies?
    report_changed_dependencies(
      \@old_dep_list, $sorted_dependencies, $tinfo, $build_cwd
    );
    return 1;
  }

  my $changed_dependencies;
  my @old_dep_sigs = split /\01/, $dep_sigs, -1;
  for (my $depidx = 0; $depidx < @$sorted_dependencies; ++$depidx) {
				# Scan through the dependencies one at a time:
    my $dep = $sorted_dependencies->[$depidx]; # Access the fileinfo.
    next if FileInfo::assume_unchanged( $dep ); # Skip if this file is one of the ones we
				# don't want to check at all.
    if ($dep->{ASSUME_CHANGED}) {   # Marked by --assume-new option?
      ::log BUILD_MARK_NEW => $tinfo, $dep
	if $::log_level;
      return 1;
    }
    if ($old_dep_list[$depidx] != $dep) {
      report_changed_dependencies(
        \@old_dep_list, $sorted_dependencies, $tinfo, $build_cwd
      );
      return 1;
    }
    next if $changed_dependencies;

    my $sig = $sig_method->signature($dep);
    if (!defined($sig) || $sig ne ($old_dep_sigs[$depidx] || '')) {
      ::log BUILD_CHANGED => $tinfo, $dep
	if $::log_level;
      $changed_dependencies = 1;
    }
  }
  $changed_dependencies ?
    'DEPENDENCIES' :
    undef;		# No build necessary.
}

#
# This subroutine is used only for printing to the log file.  It analyzes
# two lists of files and figures out exactly what's different between them.
#
sub report_changed_dependencies {
  $::log_level or return;	# Don't do anything if not logging.

  my( $old_deps, $new_deps, $tinfo ) = @_;

  my %old_deps = map +(int, $_), @$old_deps; # Int is cheapest printable ref representation
  my @not_in_old_deps;
  foreach (@$new_deps) {
    push @not_in_old_deps, $_	# Record any files which are new.
      if !delete $old_deps{int()}; # Forget common files.
  }

  ::log BUILD_DEP_DEL => $tinfo, [values %old_deps]
    if %old_deps;		# Anything left in old list?
  ::log BUILD_DEP_ADD => $tinfo, \@not_in_old_deps
    if @not_in_old_deps;
}

#
# Same as build_check() except looks only at the build info, not at the
# file info of the target.  This is used when deciding whether to import
# a file from a repository or a build cache.
#
# This is basically a stripped down version of build_check() that doesn't
# log as much stuff.
#
sub build_check_from_build_info {
  my (
    undef, $bc_entry, $sorted_dependencies, $command_string, undef,
    $sig_method, $env, $ignore_action, $ignore_architecture, $only_action
  ) = @_;

  my( $last_cmd, $arch, $dep_sigs ) =
    FileInfo::build_info_string( $bc_entry, qw(COMMAND ARCH DEP_SIGS) );
  !$ignore_action and !$last_cmd || $command_string ne $last_cmd
    and return 1;

  return undef if $only_action;

  !$ignore_architecture and ::ARCHITECTURE ne ($arch || '')
    and return 1;

  return 1 if _check_env $bc_entry, $env;

  my @old_dep_sigs = split /\01/, $dep_sigs, -1;
  return 1 if @old_dep_sigs != @$sorted_dependencies;
                                # Not useable if # of dependencies is different.
  for (my $depidx = 0; $depidx < @old_dep_sigs; ++$depidx) {
    return 1 if $old_dep_sigs[$depidx] ne $sig_method->signature($sorted_dependencies->[$depidx]);
  }

  undef;		# This file may be imported.
}

#
# This subroutine supplies the build_cache_key method.  This is a string that
# is used to look up or store files in the build cache.  The string is
# basically an MD5 hash of everything that goes into the build check.
#
# This subroutine ignores the assume_unchanged and ASSUME_CHANGED settings.
# These don't work with build caches.
#
# As with build_check(), this subroutine takes two additional arguments
# ($ignore_action and $ignore_architecture) that are intended to be used
# only by derived classes.
#
sub build_cache_key {
  $::has_md5_signatures or return undef; # Disable the build cache if
                                # the MD5 method is not available.

  my (
    undef, $tinfo, $sorted_dependencies, $key, $build_cwd, $sig_method,
    $env, $ignore_action, $ignore_architecture, $only_action
  ) = @_;			# Copy the build command directly into key.

  $key = '' if $ignore_action;	# Remove the build command.
  $key .= $ignore_architecture ? "\01" : "\01" . ::ARCHITECTURE;
				# Architecture is also important.

  if( !$only_action ) {
    for( @$sorted_dependencies ) {
      my $content_based_signature = $sig_method->signature( $_ );
      unless( Signature::is_content_based($content_based_signature) ) {
	require Signature::md5;
	# If the signature isn't already content-based, then use MD5.
	$content_based_signature = Signature::md5::signature( $Signature::md5::md5, $_ );
      }
      $key .= "\01$content_based_signature";
				# Delimit the dependency signatures too.
    }

    $key .= "\01$_\02$env->{$_}"
      for sort keys %$env;
  }

  # Add relative path of the file's dir to distinguish it from
  # copies in different locations in the tree, unless it's in cwd.
  $key .= "\01" . FileInfo::relative_filename $tinfo->{'..'}, $build_cwd
    if $tinfo->{'..'} != $build_cwd;

  if( FileInfo::case_sensitive_filenames ) {
    $key = Digest::MD5::md5_base64( $key );
    $key =~ tr|/|%|;		# Make it file system compatible.
  } else {
    $key = Digest::MD5::md5_hex( $key );
  }
  $key . "_" . $tinfo->{NAME};
                                # Append the name we're looking for to the
                                # digest so it's easy to remove junk from the
                                # build cache that isn't wanted.
}

#
# Adjust the DEP_SIGS build info field after retrieving from a build cache.
# Because the timestamps of the dependencies may not match those of the
# files from which the target in the build cache was created, even if those
# files were identical in content, we need to call this routine to adjust
# the DEP_SIGS from the build info file.  This is safe only because lookup
# success implies substitutability.
#
sub update_dep_sigs {
  my( undef, $output_finfo, $rule ) = @_;
  my %deps;
  my( $sorted_deps, $dep_sigs ) =
    FileInfo::build_info_string( $output_finfo, qw(SORTED_DEPS DEP_SIGS) );
  my @deps = split /\01/, $sorted_deps;
  @deps{@deps} = split /\01/, $dep_sigs, -1;
  my $sig_method = $rule->signature_method;
  my $dir = $rule->makefile->{CWD};
  for my $dep (@deps) {
    unless( Signature::is_content_based($deps{$dep}) ) {
      $deps{$dep} = $sig_method->signature(file_info($dep, $dir));
    }
  }
  FileInfo::set_build_info_string( $output_finfo, 'DEP_SIGS',
				   join("\01", map $deps{$_}, @deps) );
}

#
# This subroutine supplies the changed_dependencies method.  See BuildCheck.pm
# for details.
#
sub changed_dependencies {
  my( undef, $tinfo, $signature_method, $build_cwd, @dep_list ) = @_;

  my( $old_dep_str, $dep_sigs ) =
    FileInfo::build_info_string( $tinfo, qw(SORTED_DEPS DEP_SIGS) );
  return @dep_list unless $old_dep_str; # No info available: all are different.

  my %old_deps;
  @old_deps{map file_info($_, $build_cwd), split /\01/, $old_dep_str} =
    split /\01/, $dep_sigs, -1;
                                # Build a hash that allows us to get the old
                                # signature from the fileinfo.

  my @changed_deps;
  foreach my $dep (@dep_list) {
    next if FileInfo::assume_unchanged $dep; # Skip if we're not supposed to check this.

    my $old_sig = $old_deps{$dep}; # Get original signature.
    !defined($old_sig) || $dep->{ASSUME_CHANGED} ||
      $old_sig ne ($signature_method->signature($dep) || '') and
        push @changed_deps, $dep;
  }

  @changed_deps;
}

1;
