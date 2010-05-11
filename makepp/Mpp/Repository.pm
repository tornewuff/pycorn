# $Id: Repository.pm,v 1.5 2009/02/10 22:55:49 pfeiffer Exp $

=head1 NAME

Mpp::Repository - Makepp repository functionality

=head1 DESCRIPTION

This file groups all the functions needed only if repositories are actually used.
The actual functionality is also dispersed in makepp and other modules.

=cut

package Mpp::Repository;	# For CPAN scanner.
package Mpp::File;		# Use this a lot.

use Mpp::FileOpt;

sub symlink {
  CORE::symlink relative_filename( $_[1], $_[0]{'..'} ),
		&absolute_filename_nolink
    or die 'error linking ' . absolute_filename( $_[1] ) . ' to ' . &absolute_filename . "--$!\n";
  undef $_[0]{EXISTS};		# We know this file exists now.
}

#
# Because automake/autoconfig is so #@%!(#%&!)#$&(^)$(^&!( hacked up, there
# doesn't seem to be any way to support repositories without simply putting
# the files in automatically.  Automake simply does not properly list
# dependencies in the makefile, and there's no way around it except to
# list special cases.  These are the files that automake/autoconf appear
# to need that might not be correctly linked in based on instructions in
# the makefile.
#
my %automake_garbage;
@automake_garbage{qw(Makefile.in Makefile.am acconfig.h acinclude.m4 aclocal.m4
config.h config.h.in config.status configure configure.in configure.files
libtool stamp-h stamp-h.in install.sh install-sh missing mkinstalldirs)} = ();

sub load_single {
  #my( $finfo, $destfinfo ) = @_; # Name the arguments.
  return if $_[0]{NAME} =~ /\.la$/ || # Skip libtool stuff--we don't handle it very gracefully right now.
    &dont_read;

  if( exists $automake_garbage{$_[0]{NAME}} && exists $_[0]{'..'}{DIRCONTENTS}{'Makefile.in'} ||
      $_[0]{'..'}{NAME} eq 'admin' && exists $_[0]{'..'}{'..'}{DIRCONTENTS}{'Makefile.in'} ) {
				# Is this automake's crap?
				# Admin directory contains a number of vital files.
    unless( file_exists $_[1] ) { # Link it in if it's not already there.
      &mkdir( $_[1]{'..'} );	# Make the destination directory.
      &symlink( $_[1], $_[0] );	# Add the symbolic link.
      may_have_changed $_[1]; # File exists now.
    }
  } else {
    push @{$_[1]{ALTERNATE_VERSIONS}}, $_[0];
				# Mark this as a possible source.
  }
  publish $_[1], $Mpp::rm_stale_files;
				# This thing exists now, so wildcards can match it.
}

sub load_recurse {
  # $top is the top of the destination tree
  my( $dirinfo, $destdirinfo, $top, $prune_code ) = @_; # Name the arguments.

  # Ignore .makepp directories (and anything else the caller wants)
  return if $dirinfo->{NAME} eq $build_info_subdir ||
    $prune_code && &$prune_code( $dirinfo ) ||
    &dont_read;

  warn "repositories are ignored by make subprocesses when --traditional-recursive-make is in effect\n"
    if defined $Mpp::Recursive::traditional;

  # Handle empty directories properly
  &mark_as_directory;
  mark_as_directory $destdirinfo;

  if( file_exists $destdirinfo ) { # Local directory exists?
    is_writable $destdirinfo or return; # Not writable?  This is a signal
				# not to try to incorporate anything from
				# the repository.
  } else {                      # Local directory does not exist?
    undef $destdirinfo->{DIR_IN_REPOSITORY}; # Remember by placeholder that it did exist
                                # in some repository, so it should spring into existence as needed.
  }

  local $Mpp::Glob::allow_dot_files = 1; # Temporarily allow dot files, because
				# we need to look into .libs directories
				# for libtool support.
#
# Scan the directory.  For speed reasons, this depends on some internals of
# the Mpp::File package.
#
  $dirinfo->{READDIR} or &read_directory;
				# Load all the files in the directory.

  foreach( values %{$dirinfo->{DIRCONTENTS}} ) {
    next if $_ == $top;
    my $dest_finfo = file_info $_->{NAME}, $destdirinfo;
    if( is_dir $_ ) {
      load_recurse( $_, $dest_finfo, $top, $prune_code );
      push @{$dest_finfo->{ALTERNATE_VERSIONS}}, $_;
      publish $dest_finfo, $Mpp::rm_stale_files;
				# This thing exists now, so wildcards can match it.
    } elsif( file_exists $_ ) {
      load_single $_, $dest_finfo;
    }
  }
}

#
#  load $dir, $destdir;
#
# For every file in the repository directory, this sets up a build command for
# a corresponding file in the destination directory.  If the file isn't
# available in the destination directory, then when it is needed, makepp will
# check whether it is in the repository or not.	 If so, a soft link is
# made in the destination directory to the repository directory.
#
# If the optional third argument is specified, then it is taken as a coderef
# to which each dirinfo in $dir is passed. If it returns nonzero, then
# the directory is ignored.
#
sub Mpp::Repository::load {
  my( $dirinfo, $destdirinfo, $prune_code ) = @_; # Name the arguments.
  Mpp::log REP_LOAD => $dirinfo, $destdirinfo
    if $log_level;

  # TBD: Using a repository manifest (in addition to the possibility that
  # it's wrong) is still pretty slow. It would be better to do lazy computation
  # of the ALTERNATE_VERSIONS for only the files that we need.
  # TBD: Don't do this if --nouse_repository_manifest is specified
  if( is_dir &dereference ) {
    my $finfo = file_info '.repository_manifest', $dirinfo;
    if( is_readable $finfo ) {
      my $fname = absolute_filename $finfo;
      open MANIFEST, '<', $fname or die "Failed to read $fname--$!";
      Mpp::log REP_MANIFEST => $finfo
	if $Mpp::log_level;
      # NOTE: In the manifest file, each "Makefile.in" file must be the first
      # listed entry of the directory that contains it, or else this won't
      # handle automake stuff properly.
      while( <MANIFEST> ) {
        chomp;
        # TBD: These calls to load_single are expensive. Need to
        # find out why, and see if we can get around it.
        load_single file_info( $_, $dirinfo ), file_info $_, $destdirinfo;
      }
      return;
    }
    load_recurse $dirinfo, $destdirinfo, $destdirinfo, $prune_code;
  } elsif( &file_exists ) {
    &load_single;
  } else {
    die 'repository ' . &absolute_filename . " doesn't exist\n";
  }
}



sub Mpp::Repository::no_valid_alt_versions {
  return 1 unless exists $_[0]{ALTERNATE_VERSIONS};
  return unless $Mpp::rm_stale_files;
  was_built_by_makepp $_
    or return
    for @{$_[0]{ALTERNATE_VERSIONS}};
  1;
}


=head2 get

  $status = get $repository_file, $finfo;

Links a file in from a repository into the current directory.

Returns 0 if successful, nonzero if something went wrong.

=cut

sub Mpp::Repository::get {
  my( $dest_finfo, $src_finfo ) = @_;

  if( $dest_finfo->{DIRCONTENTS} ) { # Is this a directory?
    &mkdir;			# Just make it, don't soft link to it.
    return 0;			# Indicate success (TBD: how to tell if it
				# failed?)
  }

  # Don't link to the repository if the source doesn't exist (even if it
  # can be built, because we'll build it locally instead).
  return 0 unless exists_or_can_be_built_norecurse $src_finfo, undef, 1;

  &mkdir( $dest_finfo->{'..'} ); # Make the directory (and its parents) if it doesn't exist.

  Mpp::log REP_LINK => @_
    if $Mpp::log_level;

  &check_for_change;		# Flush $dest_finfo->{LINK_DEREF} to be safe
				# (in particular, to make sure that it wasn't
				# deleted by a 'clean' target). NOTE: Do this
				# *before* changing build info.

  # NOTE: Even if the symlink is already correct, we need to copy over the
  # build info, because it may have changed since the link was created.
  my $binfo = $src_finfo->{BUILD_INFO} ||=
    load_build_info_file($src_finfo) || {};
				# Get the build information for the old file.
  my %build_info = %$binfo;	# Make a copy of everything.
  $build_info{FROM_REPOSITORY} = relative_filename( $src_finfo, $dest_finfo->{'..'} );
				# Remember that we got it from a repository.
  undef $dest_finfo->{NEEDS_BUILD_UPDATE}; # Remember to update the build info.
  $dest_finfo->{BUILD_INFO} = \%build_info;
  push @build_infos_to_update, $dest_finfo;
				# Update it soon.

  if( dont_read $src_finfo ) {
    Mpp::print_error 'Cannot link ', $src_finfo, ' to ', $dest_finfo,
      ' because the former is marked for dont-read';
    return 1;			# Indicate failure.
  }
  if( &dont_read ) {
    Mpp::print_error 'Cannot link ', $src_finfo, ' to ', $dest_finfo,
      ' because the latter is marked for dont-read';
    return 1;			# Indicate failure.
  }

  my $changed = 1;
  if( &is_symbolic_link ) { # If it's already a symbolic link,
				# maybe it's correct.
    $dest_finfo->{LINK_DEREF} or &dereference;
				# Get the link value.
    $dest_finfo->{LINK_DEREF} == $src_finfo and $changed = 0;
				# If it's already right, don't do anything.
  }

  if( $changed ) {
    unless( &in_sandbox ) {
      warn $Mpp::sandbox_warn_flag ? '' : 'error: ',
	'Cannot link ', absolute_filename( $src_finfo ), ' to ', &absolute_filename,
	" because the latter is marked for out-of-sandbox\n";
      return 1 unless $Mpp::sandbox_warn_flag;	# Indicate failure.
    }
    &unlink;			# Get rid of anything that might already
				# be there.
    if( !defined $Mpp::symlink_in_rep_as_file && is_symbolic_link $src_finfo ) {
				# Must not fetch symlinks from repository, because
				# they can point to another file in the repository
				# of which we have a different version locally.
      $build_info{SYMLINK} = readlink absolute_filename $src_finfo;
      if( CORE::symlink $build_info{SYMLINK}, &absolute_filename ) {
	undef $dest_finfo->{EXISTS}; # We know this file exists now.
      } else {
	$@ = "$!";
      }
    } else {
      eval { &symlink };	# Make the link.
    }
    if ($@) {			# Did something go wrong?
      Mpp::print_error 'Cannot link ', $src_finfo, ' to ', $dest_finfo, ":\n$@";
      return 1;			# Indicate failure.
    }
    ++$Mpp::rep_hits;
  } else {
    Mpp::log 'REP_EXISTING'
      if $Mpp::log_level;
  }

  # NOTE: This has to happen *after* the file exists (or else the build info
  # won't be saved), but *before* calling may_have_changed (which erases the
  # build info). Bad things could happen if it were possible for
  # update_build_infos to be called after NEEDS_BUILD_UPDATE is set, but
  # before now.
  &update_build_infos;		# Update it now.  This way, the file is marked
				# as coming from a repository even if the
				# build command is aborted.  Next time around
				# we'll know that it came from a repository
				# and we can delete it appropriately.

  if ($changed) {
    my $build_info = $dest_finfo->{BUILD_INFO}; # Don't flush the build info.
				# (If we lose the build info, then we don't
				# clean up this file in
				# cleanup_temporary_links. TODO: is that still needed?)
    &may_have_changed;		# Flush the stat array cache.
    $dest_finfo->{BUILD_INFO} = $build_info;
    $dest_finfo->{SIGNATURE} = signature( $src_finfo );
				# Have to have the current signature or else
				# the build info will get discarded anyway.
  }

  0;				# Indicate success.
}


#
# This is the actual functions which overloads the stubs.
#
no warnings 'redefine';

sub Mpp::Subs::s_repository {
  my( $text_line, $makefile, $makefile_line ) = @_; # Name the arguments.

  foreach my $rdir ( split ' ', $makefile->expand_text( $text_line, $makefile_line )) {
				# Get a list of repository directories.
    if( $rdir =~ /^([^=]+)=(.*)/ ) { # Destination directory specified?
      my $rinfo = file_info $2, $makefile->{CWD};
      my $dst_info = file_info $1, $makefile->{CWD};
      Mpp::Repository::load $rinfo, $dst_info;
    } else {
      my $rinfo = file_info $rdir, $makefile->{CWD};
				# Get the fileinfo structure.
      Mpp::Repository::load $rinfo, $makefile->{CWD};
				# Load all the files.
    }
  }
}

1;
