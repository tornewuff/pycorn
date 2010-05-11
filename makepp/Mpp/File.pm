# $Id: File.pm,v 1.90 2009/02/11 23:22:37 pfeiffer Exp $

package Mpp::File;
require Exporter;
use Cwd;
use POSIX qw(S_ISDIR);

#use English;
# Don't ever include this!  This turns out to slow down
# Mpp::Signature::c_compilation_md5::md5sum_c_tokens by at least three orders
# of magnitude!

@ISA = 'Exporter';
@EXPORT = qw(file_info path_file_info chdir absolute_filename absolute_filename_nolink relative_filename file_exists
	     touched_filesystem may_have_changed stat_array is_dir is_or_will_be_dir dereference case_sensitive_filenames $CWD_INFO);

use strict;
use Mpp::Text ();

=head1 NAME

Mpp::File -- cached information about files and directories

=head1 USAGE

Note that we show both a functional and an object oriented syntax here.  Since
the latter has a measurable overhead, and as this module is very heavily used,
the functional syntax is to be preferred.

  use Mpp::File;
  chdir($new_dir);		# Changes to new directory and keeps track
				# of the directory name in the variable
				# $CWD_INFO.
  $finfo = file_info('filename');

  build_handle( $finfo );
  $finfo->build_handle;		# Returns the handle for the process that is
				# building (or has built) the file.
  set_build_handle($finfo,$handle);
  $finfo->set_build_handle($handle); # Sets the handle for the process
				# that's currently building the file.


  $build_rule = build_rule( $finfo ); # Returns the rule for building the file,
				# if such a rule exists.
  set_build_rule($finfo,$rule);
  $finfo->set_build_rule($rule); # Set the rule used to build this file.

  build_info_string($finfo,'key',...);
  $finfo->build_info_string('key',...);
				# Returns a piece of information from the build
				# info file, if there is one.
  set_build_info_string($finfo, 'key', 'value');
  $finfo->set_build_info_string('key', 'value');
  Mpp::File::update_build_infos(); # Flushes build info cache to disk.

  if (exists_or_can_be_built( $finfo )) {}
  if ($finfo->exists_or_can_be_built) {}
  my $dir_finfo = parent( $finfo );
  my $dir_finfo = $finfo->parent; # The directory containing this file.
  $name = absolute_filename( $finfo );
  $name = $finfo->absolute_filename;	# Returns absolute file name.
				# If more than one name can be used
				# (because of soft links), the shortest
				# possible one is used to avoid problems with
				# the automounter.
  $name = relative_filename( $finfo );
  $name = $finfo->relative_filename;
  $relative_fname = relative_filename( $finfo, 'dir');
  $relative_fname = $finfo->relative_filename('dir');
  $relative_fname = relative_filename($finfo, $dirinfo);
  $relative_fname = $finfo->relative_filename($dirinfo);
				# Returns name relative to given directory.

  may_have_changed( $finfo );
  $finfo->may_have_changed;	# Indicate that the file may have changed on
				# disk, so invalidate (or check) cached info.

  if (file_exists( $finfo )) { ... }
  if ($finfo->file_exists) { ... }
  $date = file_mtime( $finfo );
  $date = $finfo->file_mtime;

  if (is_dir( $finfo )) { ... }
  if ($finfo->is_dir) { ... }
  if (is_writable( $finfo )) { ... }
  if ($finfo->is_writable) { ... }
  if (is_executable( $finfo )) { ... }
  if ($finfo->is_executable) { ... }
  if (is_symbolic_link( $finfo )) { ... }
  if ($finfo->is_symbolic_link) { ... }

  Mpp::File::unlink( $finfo );
  $finfo->unlink;		# Deletes the file.

  my $link_finfo = dereference( $finfo );
  my $link_finfo = $finfo->dereference;
				# Dereference a symbolic link.

  read_directory( $finfo );
  $finfo->read_directory;	# Try to (re)read the contents of a directory.

  my $stat_array = stat_array( $finfo ); # Return the array returned by stat().
  my $stat_array = $finfo->stat_array; # Return the array returned by stat().
  my $lstat_array = lstat_array( $finfo ); # Return the array returned by lstat().
  my $lstat_array = $finfo->lstat_array; # Return the array returned by lstat().
  relative_filename('file', 'dir'); # Returns relative name of file
				# with respect to the directory.

=head1 DESCRIPTION

Mpp::File is an efficient way to avoid re-statting files and rereading
directories.  For each known file, there is a Mpp::File structure that
describes what is known about the file.	 You may add arbitrary
additional information to the structure.

Mpp::File is designed so it won't be confused by soft-linked directories.
However, it will have problems if a soft link initially points to one
directory and then is changed to point to a different directory, or if files
are referred to thruogh a symbolic link to a directory before the symbolic
link is actually created.  Generally speaking, it's not a good idea to modify
existing soft links.

Mpp::File can be used alone.  Some supplemental routines useful only in the
context of makepp are found in Mpp/FileOpt.pm, and in fact that file
overrides some of the routines here.

=cut

#
# We only store the subset of (l)stat values which makepp needs.  This reduces
# makepp's memory footprint by 1.5% and execution time by 4% compared to the
# 13-element array.
#
# These consts correspont to real stat indexes 2, 3, 4, 7, 9, 0 as use in lstat_array!
BEGIN {
  *STAT_MODE =  \&Mpp::Text::CONST0;
  *STAT_NLINK = \&Mpp::Text::CONST1;
  *STAT_UID =   \&Mpp::Text::CONST2;
  *STAT_SIZE =  \&Mpp::Text::CONST3;
  *STAT_MTIME = \&Mpp::Text::CONST4;
  *STAT_DEV =   \&Mpp::Text::CONST5; # Added by lstat only for dirs.
}

=head2 case_sensitive_filenames

True if we think makepp should treat filenames as case sensitive.

At present, makepp can be either 100% case insensitive, converting all
filenames to lower case, or 100% case sensitive.  Makepp currently cannot
handle some files coming from a case-insensitive file system and other files
coming from a case-sensitive file system.

This routine is just a guess.  We look at the current directory to see if it
looks case sensitive, and switch makepp into the appropriate mode.

=head2 $stat_exe_separate

On Windows this is true if you can't stat 'xyz', when only 'xyz.exe' exists.
That is ActiveState at least until 5.10.0 and possibly older Cygwin versions.

=cut

our $stat_exe_separate;
BEGIN {
  my $done;
  if( exists $ENV{MAKEPP_CASE_SENSITIVE_FILENAMES} ) {
    *case_sensitive_filenames = $ENV{MAKEPP_CASE_SENSITIVE_FILENAMES} ? \&Mpp::Text::CONST1 : \&Mpp::Text::CONST0;
    return if !Mpp::is_windows;
    $done = 1;
  }
  my $test_fname = '.makepp_test';
  substr $test_fname, 12, -1, substr rand, 1 while
    -e $test_fname || -e uc $test_fname or
    Mpp::is_windows and -e "$test_fname.exe" || -e uc "$test_fname.exe";
  $test_fname .= '.exe' if Mpp::is_windows;
  if( open my $fh, '>', $test_fname ) { # Create the file.
    close $fh;			# For unlinking on Windows.
  } else {
    $stat_exe_separate = Mpp::is_windows > 0;
    *case_sensitive_filenames = Mpp::is_windows ? \&Mpp::Text::CONST0 : \&Mpp::Text::CONST1
      unless $done;
    return;
				# If that doesn't work for some reason, assume
				# we are case insensitive if windows, and case
				# sensitive for unix.
  }

  *case_sensitive_filenames = -e uc $test_fname ? \&Mpp::Text::CONST0 : \&Mpp::Text::CONST1
    unless $done;
				# Look for it with different case.
  $stat_exe_separate = !-e substr $test_fname, 0, -4 if Mpp::is_windows;
  unlink $test_fname;
}

# Set this (probably locally) to attempt to avoid lstat calls by reading the
# directory first if the cached directory listing might be stale.  This may
# or may not speed things up.
our( $read_dir_before_lstat, $root, $CWD_INFO );
my $epoch = 2; # Counter that determines whether dir listings are up-to-date.
our $empty_array = [];		# Only have one, instead of a new one each time
my @ids_for_check;

our $directory_first_reference_hook; # This coderef is called on the first call to
				# exists_or_can_be_built with any file within
				# a given directory, with that directory's
				# Mpp::File object as an arg. It should restore
				# the cwd if it changes it.

#
# All of the information is stored in the structure below.  $root is an
# associative array indexed by the top-level directories.  In addition, for
# every directory that we stat, we store the device and inode number so we
# won't be confused by symbolic links with directories.
#
$root = bless { NAME => '',
		FULLNAME => '',
		DIRCONTENTS => {},
		EXISTS => undef
	       };

#
# Here are all the possible keys that can be contained in a Mpp::File
# structure.  Of course, not all Mpp::File structures will have all of these
# fields.  As usual in OOP, these fields should not be explicitly accessed
# except by member functions; this documentation is provided as internals
# documentation of the Mpp::File class.
#
# Key		Meaning
# ..		A reference to the Mpp::File of the parent directory whose
#		DIRCONTENTS field contains this file.
# BUILD_HANDLE	A Fork::Process handle for the process that is currently
#		building or has already built this file.
# BUILD_INFO	If build information has been loaded, this hash contains the
#		key/value pairs.  See build_info_string() and
#		set_build_info_string().
# RULE		The rule object for the file, if known.
# DIRCONTENTS	If this is a directory, this contains a reference to another
#		hash of the files in the directory.  The key for the hash
#		is the filename.  Sometimes files which aren't directories
#		can have a DIRCONTENTS field too.  This occurs when they are
#		referenced as a directory, i.e., 'filename/filename'.
#		Usually this is for a directory that doesn't exist yet but
#		will be created.
#		The DIRCONTENTS field is only created by the subroutine
#		mark_as_directory().  This is so the wildcard routines are
#		reliably informed that a new directory exists.	See the
#		documentation for Mpp::Glob::wildcard_action for details.
# EXISTS	Exists iff we know the file exists (either because we lstatted it,
#		or because its name was in the directory).
# IS_PHONY	Exists iff this has been tagged as a phony target.
# LINK_DEREF	Exists iff this is a soft link.  False if we have not dereferenced
#		it, else the cached value of the symbolic link.
# LSTAT		A reference to the array returned by lstat.
# NAME		The name of the file (without any directories).
# PUBLISHED	True if we've alerted any waiting wildcard subroutines that
#		this file exists.
# READDIR	Nonzero if we've tried to read this directory.
# ALTERNATE_VERSIONS
#		For files that can be imported from a repository, this field
#		contains a reference to the Mpp::File structs for the file in
#		the repositories.
# SCANNED_FOR_SUBDIRS
#		Exists iff this is a directory and we have found all of the
#		subdirectories under the current directory, i.e., we don't
#		need to stat any more files to see if they are subdirectories.
# FULLNAME	The absolute filename cached for performance.
# WILDCARD_ROUTINES
#		For a directory, this is a list of subroutines to be called
#		whenever a new file springs into existence in this directory
#		or any subdirectory.  These routines are used so that wildcards
#		can match files which didn't exist when the wildcard was
#		invoked.  See Mpp::Glob::wildcard_action() and Mpp::File::publish()
#		for details.
# <number>	Used in directory finfos, to store the relative path to another
#		directory, of which this is the integral address.
#

=head2 absolute_filename

  $str = absolute_filename( $fileinfo );

Returns the absolute file name.

=cut

sub absolute_filename {
  my $fstr = $_[0] == $root ? '/' :
    $_[0]{FULLNAME} ||		# Cached name?
    $_[0]{'..'}{FULLNAME} . '/' . $_[0]{NAME};
				# All directories already have a cached name.

  if( Mpp::is_windows ) {
    $fstr =~ s@^/(?=[A-Za-z]:)@@s;
				# Convert /C: to C:.  We converted the other
				# way so we could use unix file name syntax
				# everywhere.
  }
  $fstr;
}

#
# A variant of absolute_filename that ignores symbolic links.  This is intended
# for internal use, when we need to supply a name to the operating system,
# especially when we lstat the file.
#
sub absolute_filename_nolink {
  my $fileinfo = $_[0];		# Locate the file.

  my $ret_str = $fileinfo->{NAME};

  return '/' if !defined($ret_str) or	# Ugly workaround for unknown problem.  (Forum 2005-05-23)
    $fileinfo == $root;
				# Special case this one.
  for (;;) {
    $fileinfo = $fileinfo->{'..'};
    last if $fileinfo == $root; # Quit when we reached the top.

    $ret_str = $fileinfo->{NAME} . "/$ret_str";
				# Add another directory.
  }

  return $ret_str
    if Mpp::is_windows and
    $ret_str =~ /^[A-Za-z]:/s;	# Leave initial C: without /.

  "/$ret_str";
}

=head2 chdir('new dir')

  chdir('new dir')
  chdir($dirinfo)

Changes to the indicated directory, and keeps track of the change in the
variable $CWD_INFO.  Dies with a message if the chdir failed.

You can pass a Mpp::File structure describing the directory instead of the
directory name itself if that is more convenient.

This subroutine is automatically exported into any packages that
use Mpp::File, so your chdirs will work automatically.

=cut

sub chdir {
  my $newdir = $_[0];		# Access the directory.
  ref($newdir) eq 'Mpp::File' or $newdir = &file_info;
				# Get the Mpp::File structure.
  return 0 if $newdir == $CWD_INFO; # Don't do the chdir if we're already there.
  my $status = CORE::chdir absolute_filename_nolink( $newdir );

  unless ($status) {
    if( exists $newdir->{ALTERNATE_VERSIONS} ) { # Was it from a repository?
      &mkdir( $newdir );	 # Make it.
      $status = CORE::chdir absolute_filename_nolink( $newdir );
    }
    $status or
      die ("chdir: can't cd to directory " . absolute_filename( $newdir ) .
	   "--$!\n");
  }

  publish($newdir);		# Make sure we know about this directory.

  $CWD_INFO = $newdir; # Store the new directory if that succeded.
}


=head2 dereference

  $finfo = dereference( $fileinfo );

If the file is a symbolic link, this returns a Mpp::File structure for the file
it points to.  If the symbolic link points to another symbolic link, returns
what that link points to.  If the file is not a symbolic link, returns the
original Mpp::File structure.

=cut

sub dereference {
  my $finfo = $_[0];		# Get the argument as a Mpp::File struct.
  $finfo->{LSTAT} or &lstat_array;	# Get the flags.
  for( 0..20 ) {
    exists $finfo->{LINK_DEREF} or return $finfo;
				# Not a symbolic link.
#    return $finfo if exists $finfo->{ALTERNATE_VERSIONS};
				# Treat a repository link as not a link.
    $finfo = $finfo->{LINK_DEREF} ||= # Have we already dereferenced it?
      path_file_info( readlink absolute_filename_nolink( $finfo ), $finfo->{'..'} );
    $finfo->{LSTAT} or lstat_array( $finfo );
  }
  die 'symlink: infinite loop trying to resolve symbolic link ', &absolute_filename, "\n";
}

=head2 file_exists

  if (file_exists( $file_info )) { ... }

Returns true (actually, returns the Mpp::File structure) if the file exists,
and undef if it doesn't.

=cut

sub file_exists {
  exists $_[0]{EXISTS} or	# See if we already know whether it exists.
    &lstat_array;		# Stat it to see if it exists.	This will set
				# the EXISTS flag.
  exists $_[0]{EXISTS} ? $_[0] : undef;
}

=head2 file_info

  $finfo = file_info('filename');
  $finfo = file_info('filename', $dirinfo);

Returns the Mpp::File structure for the given file.  If no Mpp::File
structure exists, creates a new one for it.

The optional second argument specifies a directory the file name
should be relative to.	By default, this is the current directory.

  foreach (@{dir_contents( $finfo )}) {
    exists_or_can_be_built( $_ ) or next;	# Skip if file doesn't exist.
				# (Files which don't exist can have Mpp::File
				# entries, if you happened to call
				# file_info on them explicitly.)
    # do your thing here.
  }

If you want to iterate through all the files which are in a directory,
not just the ones encountered previously, then call $dirinfo->read_directory
before using the above code snippet.

=cut

sub file_info {
  goto &path_file_info if Mpp::is_windows ? $_[0] =~ /[\/\\]/ : $_[0] =~ /\//;
  my $dinfo = $_[1] || $CWD_INFO;
  unless( exists $dinfo->{DIRCONTENTS} ) {
				# If the DIRCONTENTS field doesn't exist, then
				# we haven't checked yet whether the parent is
				# a directory or not.
    if( is_symbolic_link( $dinfo )) { # Follow symbolic links.
      my $orig_dinfo = $dinfo;
      $dinfo = dereference $dinfo; # Get where it points to.
      mark_as_directory( $dinfo ); # Remember that this is a directory.
      $orig_dinfo->{DIRCONTENTS} = $dinfo->{DIRCONTENTS};
				# Set the DIRCONTENTS field of the soft link
				# to point to the DIRCONTENTS of the actual
				# directory.
    } else {
      mark_as_directory( $dinfo ); # Let the wildcard routines know that we
				# discovered a new directory.
      publish( $dinfo );	# Alert any wildcard routines.
    }
  }
  if( 3 > length $_[0] ) {
    if( $_[0] eq '..' ) {	# Go up a directory?
      return $dinfo = $dinfo->{'..'} || $root; # Don't go up above the root.
    } elsif( $_[0] eq '.' ) {	# Do nothing in same directory.
      return $dinfo;
    }
  }
  $dinfo->{DIRCONTENTS}{case_sensitive_filenames ? $_[0] : lc $_[0]} ||=
    bless { NAME => case_sensitive_filenames ? $_[0] : lc $_[0], '..' => exists $dinfo->{LINK_DEREF} ? dereference $dinfo : $dinfo };
}

=head2 path_file_info

Does the work of file_info when I<filename> contains directory separators.  You can call this
explicitly in places where I<filename> is (almost) sure to have directory separators.

=cut

sub path_file_info {
  my $file = case_sensitive_filenames ? $_[0] : lc $_[0];
				# Copy the file name only if we continue.
				# Switch to all lower case to avoid
				# confounds with mix case.
  my $dinfo;			# The fileinfo we start from.

  $file =~ tr|\\|/| if Mpp::is_windows; # 'C:\temp/foo' is a valid file name

  if( $file =~ s@^/+@@s ) {
    if( Mpp::is_windows && length( $file ) + 2 == length( $_[0] ) && $file =~ s@^([^/]+/[^/]+)/?@@s ) {
				# If we get a //server/share syntax, treat the
				# "/server/share" piece as one directory, since
				# it is never legal to try to access //server
				# without specifying a share.
				# This has the odd effect of making the top
				# level directory's filename actually have
				# a couple of slashes in it, but that's ok.
      my $share = "/$1";
      if( -e $share ) {		# False alarm, e.g. //bin/ls
	substr $file, 0, 0, $1;
	$dinfo = $root;
      } else {
	$dinfo = $root->{DIRCONTENTS}{$share} ||=
	  bless { NAME => $share, '..' => $root };
	unless( exists $dinfo->{DIRCONTENTS} ) {
	  mark_as_directory($dinfo); # Let the wildcard routines know that we
				# discovered a new directory.
	  publish($dinfo);	# Alert any wildcard routines.
	}
      }
    } else {
      $dinfo = $root;
    }
  } elsif( Mpp::is_windows && $file =~ /^[A-Z]:/is ) {
    $dinfo = $root;		# Treat "C:" as if it's in the root directory.
  } else {
    $dinfo = $_[1] || $CWD_INFO;
  }

  for( split /\/+/, $file ) { # Handle each piece of the filename.
#
# Also, we now know the parent is (or possibly will be) a directory, so we
# need to publish it as a directory.  This is necessary so wildcard routines
# install themselves appropriately in the directory.
#
    unless( exists $dinfo->{DIRCONTENTS} ) {
				# If the DIRCONTENTS field doesn't exist, then
				# we haven't checked yet whether the parent is
				# a directory or not.
      if( is_symbolic_link( $dinfo )) { # Follow symbolic links.
	my $orig_dinfo = $dinfo;
	$dinfo = dereference $dinfo; # Get where it points to.
	mark_as_directory( $dinfo ); # Remember that this is a directory.
	$orig_dinfo->{DIRCONTENTS} = $dinfo->{DIRCONTENTS};
				# Set the DIRCONTENTS field of the soft link
				# to point to the DIRCONTENTS of the actual
				# directory.
      } else {
	mark_as_directory( $dinfo ); # Let the wildcard routines know that we
				# discovered a new directory.
	publish( $dinfo );	# Alert any wildcard routines.
      }
    }

#
# At this point, $dinfo points to the the parent directory.  Now handle the
# file:
#
    if( 3 > length ) {
      if( $_ eq '..' ) {		# Go up a directory?
	$dinfo = $dinfo->{'..'} || $root; # Don't go up above the root.
	next;
      } elsif( $_ eq '.' ) {	# Do nothing in same directory.
	next;
      }
    }
    $dinfo = ($dinfo->{DIRCONTENTS}{$_} ||=
	      bless { NAME => $_, '..' => exists $dinfo->{LINK_DEREF} ? dereference $dinfo : $dinfo });
				# Point to the entry for the file, or make one
				# if there is not one already.
  }

  $dinfo;
}

=head2 file_mtime

  $date = file_mtime( $file_info );
  $date = file_mtime('filename');

Returns the last modification time for the given file.	If the file is a
symbolic link, this returns the modification for the file the link refers to.
Return undef if it doesn't exist.

=cut

sub file_mtime { (&stat_array)->[STAT_MTIME] }

=head2 is_dir

  if (is_dir( $fileinfo )) { ... }

Returns true (actually, returns the fileinfo structure) if the given file is
actually a directory.  Does not return true for soft links that point to
directories.  (If you want to do that, you can call is_symbolic_link and then
follow the symbolic link using dereference.)  See under C<file_info> for how
to examine the contents of the directory.

is_dir() only returns true for directories that currently exist.  You can
create Mpp::File structures for directories that don't exist yet; to check for
this kind of directory, use is_or_will_be_dir().

=cut

sub is_dir { S_ISDIR( (&lstat_array)->[STAT_MODE] || 0 ) ? $_[0] : undef }

=head2 is_or_will_be_dir

  if (is_or_will_be_dir( $fileinfo )) { ... }

Returns true (actually, returns the fileinfo structure) if the given file is
actually a directory, or if it will be a directory (because file_info() was
called using it as a directory).  Also returns true for soft links that point
to directories.

=cut
sub is_or_will_be_dir {
  my $dinfo = $_[0];
  my $result = $dinfo->{DIRCONTENTS} ||
    (($dinfo->{LSTAT} || &lstat_array),
     S_ISDIR( (exists $dinfo->{LINK_DEREF} ? &dereference : $dinfo)->{LSTAT}[STAT_MODE] || 0 )) ?
      $dinfo : undef;
  if( $directory_first_reference_hook ) {
    $dinfo=$dinfo->{'..'} unless $result;
    my $changed;
    while( $dinfo && !$dinfo->{REFERENCED} ) {
      $dinfo->{REFERENCED} = 1;
      # TBD: Maybe we ought to call these in reverse order?
      &$directory_first_reference_hook( $dinfo );
      $changed = 1;
      $dinfo = $dinfo->{'..'};
    }
    goto &is_or_will_be_dir if $changed;
  }
  $result;
}


=head2 is_executable

  if (is_executable( $finfo )) { ... }

Returns true (actually, returns the Mpp::File structure) if the given file is
executable by this user.  We don't actually handle the group executable
bit correctly right now, since it's a pain to find out what groups this
user is actually in.

=cut

sub is_executable {
  my $stat = &stat_array;	# Get the status info.
  @$stat or return undef;	# File doesn't exist.
  ($stat->[STAT_MODE] & (011)) || # User or group executable?
    (($stat->[STAT_MODE] & 0100) && # User executable?
      $stat->[STAT_UID] == $>) and return $_[0]; # We're the owner?
				# It's executable.
  undef;
}

=head2 is_readable

  if (is_readable( $finfo )) { ... }

Returns true if the given file or directory can be read.

=cut

sub is_readable {
  my $finfo = $_[0];
  return $finfo->{IS_READABLE}
    if exists $finfo->{IS_READABLE}; # Use cached value.

  undef $finfo->{IS_READABLE};	# Assume it won't be readable.
  my $stat = &stat_array; # Get stat info.
  return $finfo->{IS_READABLE}
    if exists $finfo->{IS_READABLE}; # Use value possibly just set.
  $stat && (($stat->[STAT_MODE] || 0) & 0444) or return undef;
				# If no one can read the file, then it's
				# definitely not readable.
  # If it's a pipe, then simply trust the read permissions. This could be
  # wrong, but we would detach a prospective writer if we opened it ourself,
  # so this is the best we can do.
  if(-p &absolute_filename) {
    return $finfo->{IS_READABLE} = &have_read_permission;
  }
#
# Checking for readability is very complicated and file-system
# dependent, so we just try to open the file.
#
  if( S_ISDIR $stat->[STAT_MODE] ) {	# A directory?
    opendir my $fh, &absolute_filename or return undef;
  } else {
    open my $fh, &absolute_filename or return undef;
  }

  $finfo->{IS_READABLE} = 1; # File is readable.
}

=head2 have_read_permission

  if (have_read_permission( $finfo )) { ... }

Returns true if the given file or directory has its read permission set
for the effective user ID.
This is not the same as is_readable, because there are other reasons that
you might not be able to read the file.

=cut

sub have_read_permission {
  if (exists($_[0]{HAVE_READ_PERMISSION})) { # Use cached value.
    return $_[0]{HAVE_READ_PERMISSION};
  }
  $_[0]{HAVE_READ_PERMISSION} = -r &absolute_filename;
}

=head2 is_symbolic_link

  if (is_symbolic_link( $finfo )) { ... }

Returns true (actually, returns the Mpp::File structure) if the given file
is a symbolic link.

=cut

sub is_symbolic_link {
  exists $_[0]{LSTAT} || &lstat_array; # Get status info.
  exists $_[0]{LINK_DEREF} ? $_[0] : undef;
}

=head2 is_writable

  if (is_writable( $dirinfo )) { ... }

Returns true if the given directory can be written to.	Because of the
complexity of testing for permission, we test by actually trying to
write a file to that directory.

=cut

sub is_writable {
  my $dirinfo = $_[0];		# Access the fileinfo struct.
  if (exists($dirinfo->{IS_WRITABLE})) { # Did we try this test before?
    return $dirinfo->{IS_WRITABLE}; # Use the cached value.
  }

#
# For some reason, on cygwin it is possible to write to a directory whose
# mode is 0000, so trying to create a file in the directory is not a valid
# test.	 So we explicitly test the mode instead of trying to open a file.
# If the mode says it's read only for all users, then we don't bother
# with the file test.  This at least makes the documented way of inhibiting
# repository inputs work.
#
  my $dirstat = &stat_array;
  @$dirstat == 0 and return;
  ($dirstat->[STAT_MODE] & 0222) == 0 and
    return $dirinfo->{IS_WRITABLE} = 0;

  my $test_fname = &absolute_filename_nolink . '/.makepp_test';
  my $len = length $test_fname;
  substr $test_fname, $len, -1, substr rand, 1 while
    -e $test_fname;		# Try to create a file with an unlikely name
				# which goes away automatically at the end.

  local( $>, $) ) = @ids_for_check # Check with a different UID because root
				# can write too much.  See setting of ids_for_check
				# for an explanation of why.
    if !$> && $ids_for_check[0]; # Are we running as root?

  if( open my $fh, '>', $test_fname ) { # Can we create such a file?
      close $fh;
      unlink $test_fname;
      $dirinfo->{IS_WRITABLE} = 1;
  } else {
      undef $dirinfo->{IS_WRITABLE};
  }
}

=head2 is_writable_owner

   if (is_writable_user( $finfo ))

Determines if a given file is writable by its owner by just checking the
mode bits.  This does not test whether the current user is the owner.

=cut

sub is_writable_owner { ((&stat_array)->[STAT_MODE] || 0) & 0200 }

=head2 touched_filesystem

A static method with no arguments.  Call this to notify this package that
some code has been run that might have changed something (especially in the
case of adding files) that we might care about without updating the database.

=cut

sub touched_filesystem {
  die unless ++$epoch;		# Detect wraps, just in case
}

=head2 lstat_array

   $statinfo = stat_array( $fileinfo );
   $uid = $statinfo->[STAT_UID];	# Or whatever field you're interested in.

Returns the array of values returned by the C<lstat> function on the file.
The values are cached, so calling this repeatedly entails only minimal extra
overhead.

=cut

sub lstat_array {
  my $fileinfo = $_[0];		# Get the fileinfo structure.

  my $stat_arr = $fileinfo->{LSTAT}; # Get the cached value.
  unless( defined $stat_arr ) {	# No cached value?
    if( $read_dir_before_lstat ) {
      $fileinfo->{'..'} and
	($fileinfo->{'..'}{READDIR} || 0) != $epoch and
	read_directory( $fileinfo->{'..'} );
      exists $fileinfo->{EXISTS} or
	return $fileinfo->{LSTAT} = $empty_array;
    }
    if( lstat &absolute_filename_nolink ) { # Restat the file, and cache the info.
				# File actually exists?
      $stat_arr = $fileinfo->{LSTAT} = [(lstat _)[2, 3, 4, 7, 9]]; # These must correspond to STAT_* above!
      if( -l _ ) {		# Profit from the open stat structure, unless it's a symlink.
	undef $fileinfo->{LINK_DEREF};
      } else {
	$fileinfo->{HAVE_READ_PERMISSION} = -r _; # Let Perl figure out readability
	$fileinfo->{IS_READABLE} = -r _ if -p _;
#
# When a file has been created or changed, it's possible that it has become
# a directory.	If this is true, then we definitely need to tag it as a
# directory so all the wildcard routines know about it.	 Otherwise we'll miss
# a lot of files.
#
	if( -d _ ) {		# Now it's a directory?
	  $fileinfo->{LSTAT}[STAT_DEV] = (lstat _)[0]; # Add this field.
	  $fileinfo->{DIRCONTENTS} or # Previously known as a dir?
	    &mark_as_directory;	# Tell the wildcard system about it.
	}
      }
      until( exists $fileinfo->{EXISTS} ) {
	undef $fileinfo->{EXISTS};
	publish( $fileinfo );	# If we now know the file exists but we didn't
				# use to know that, activate any waiting
				# subroutines.
	$fileinfo = $fileinfo->{'..'};
      }
    } else {
      $stat_arr = $fileinfo->{LSTAT} = $empty_array;
    }
  }
  $stat_arr;
}

=head2 may_have_changed

  may_have_changed( $finfo );

Indicates that a file may have changed, so that any cached values (such as the
signature or the file time) are invalid.

=cut
sub may_have_changed {
  my $finfo = $_[0];

  # If the parent directory has been read, then make sure that we will
  # re-read it before we bypass any lstat's.
  $finfo->{'..'}{READDIR} &&= $epoch-1;

  delete @{$finfo}{qw(LINK_DEREF LSTAT EXISTS IS_READABLE HAVE_READ_PERMISSION
		      BUILD_INFO NEEDS_BUILD_UPDATE IS_WRITABLE)};
}

=head2 check_for_change

  check_for_change( $finfo );

Like may_have_changed, indicates that a file may have changed, but retains
the build info unless the signature actually changed.
This is used in place of may_have_changed in order to prevent the unnecessary
destruction of build info, which is expensive to compute in some cases.

=cut
sub check_for_change {
  return if $Mpp::gullible;
  my $finfo = $_[0];

  my $sig_date_size;
  !exists $finfo->{IS_PHONY} && $finfo->{LSTAT} and
    $sig_date_size = &signature; # Get the current signature.  Don't
				# get it if the info isn't already cached.
  $sig_date_size ||= $finfo->{BUILD_INFO}{SIGNATURE};
				# If we linked a file in from the repository,
				# we might not have the signature available,
				# but we don't want to throw away the build
				# info below.

  # If the parent directory has been read, then make sure that we will
  # re-read it before we bypass any lstat's.
  $finfo->{'..'}{READDIR} &&= $epoch-1;

  delete @{$finfo}{qw(LINK_DEREF LSTAT EXISTS IS_READABLE HAVE_READ_PERMISSION IS_WRITABLE)};
  if (($sig_date_size || '') ne (&signature || '')) {
				# Get the signature again.  If it hasn't
				# changed, then don't dump the build info.
    warn '`'.&absolute_filename."' changed without my knowledge\n".
      "but you got lucky this time because its signature changed\n" if $sig_date_size;
    delete @{$finfo}{qw(BUILD_INFO NEEDS_BUILD_UPDATE)};
  }
}

=head2 mkdir

   Mpp::File::mkdir( $dirinfo );

Unless it exists, makes the directory specified by the Mpp::File structure (and
any parent directories that are necessary).

=cut
sub mkdir {
  my $dirinfo = $_[0];

  return if &is_dir;		# If it's already a directory, don't do
				# anything.
  &mkdir( $dirinfo->{'..'} );	# Make sure the parent exists.
  CORE::mkdir &absolute_filename_nolink, 0777;
				# Make the directory.
  &may_have_changed;		# Restat it.
  # Need to discard the LSTAT of the parent directory, because the number
  # of links to it has changed, and Mpp::Glob uses that.
  # TODO: Is it harmful to just increment it, instead of forcing a restat?
  delete(($dirinfo->{'..'} || {})->{LSTAT});
  $dirinfo->{IS_WRITABLE} = 1;	# This directory is now writable.
  $dirinfo->{IS_READABLE} = 1;
}

=head2 parent

   $dirinfo = parent( $finfo );

Returns the directory containing the file.

=cut
sub parent { $_[0]{'..'} }

=head2 read_directory

  read_directory( $dirinfo );

Rereads the given directory so we know what files are actually in it.

=cut

sub read_directory {
  my $dirinfo = $_[0];		# Find the directory.

  delete @{$_}{qw(LINK_DEREF EXISTS IS_READABLE LSTAT HAVE_READ_PERMISSION IS_WRITABLE)}
    for values %{$dirinfo->{DIRCONTENTS}};
				# Forget what we used to know about which
				# files exist.

  opendir my $dirhandle, &absolute_filename_nolink or return;
				# Just quit if we can't read the directory.
				# This can happen for directories which don't
				# exist yet, or directories which are unreadable.

  &mark_as_directory;		# Make sure we know this is a directory.
  foreach( readdir $dirhandle ) {
    next if $_ eq '.' || $_ eq '..'; # Skip the standard subdirectories.
    case_sensitive_filenames or tr/A-Z/a-z/;
    my $finfo = ($dirinfo->{DIRCONTENTS}{$_} ||=
		 bless { NAME => $_, '..' => $dirinfo });
				# Get the file info structure, or make
				# one if there isn't one available.
    undef $finfo->{EXISTS};	# Remember that this file exists.
    publish($finfo);		# Activate any wildcard routines.
  }

  delete dereference( $dirinfo )->{LSTAT};
  $dirinfo->{READDIR} = $epoch;	# Remember that we read this directory.
}

=head2 relative_filename

  $str = relative_filename( $fileinfo ); # Relative to current directory.
  $str = relative_filename( $fileinfo, $dirinfo);
  $n = relative_filename( $fileinfo, $dirinfo, $distance ); # Only count hops.

Return a file name relative to the given directory, if specified.
If no directory is specified, uses the current working directory.
If the directory and the file have no directories in common (e.g.,
like '/home/mystuff/stuff' and '/usr/local/bin'), then an absolute
file name is returned.

=cut

sub relative_filename {
  my $fileinfo = $_[0];		# Get the filename.
  return '/' if $fileinfo == $root && !$_[2]; # Special case this.

  my $dir = $_[1] || $CWD_INFO;

  return $_[2] ? 0 : '.' if $fileinfo == $dir; # Take care of this annoying special case
				# first.
  return $_[2] ? 0 : $fileinfo->{NAME} if $fileinfo->{'..'} == $dir;
				# Optimize for the special case where the
				# file is in the given directory.


  unless( $_[2] ) {
    # NOTE: We know that $fileinfo != $root here.
    return $dir->{int $fileinfo} if exists $dir->{int $fileinfo};
    return $dir->{int $fileinfo->{'..'}} . '/' . $fileinfo->{NAME}
      if exists $dir->{int $fileinfo->{'..'}};
  }

#
# Find the longest common prefix in the directories.  We can't take the
# string returned from absolute_filename, since that uses symbolic links,
# and .. doesn't do what you'd expect if symbolic links are involved.  So
# we compare only the physical directories.
#
  my( $orig_dir, @dirs1, @dirs2 ) = $dir;
  # Get a list of directories for the file.
  while( $fileinfo != ($fileinfo->{ROOT} || $root) ) {
    push @dirs1, $fileinfo;
    $fileinfo = $fileinfo->{'..'}; # Go to the parent.
    goto DONE if $fileinfo == $dir;
				# Found it, no need to home in from the other side.
  }

  # Get a list of directories for the dir we want to be relative to.
  while( $dir != ($dir->{ROOT} || $root) ) {
    push @dirs2, $dir;
    $dir = $dir->{'..'};
  }

  unless( $fileinfo->{ROOT} && $dir->{ROOT} && $fileinfo->{ROOT} == $dir->{ROOT} ) {
				# Not in same subtree, so go up to system root
    while( $fileinfo != $root ) {
      push @dirs1, $fileinfo;
      $fileinfo = $fileinfo->{'..'}; # Go to the parent.
    }
    while( $dir != $root ) {
      push @dirs2, $dir;
      $dir = $dir->{'..'};
    }
    return ($orig_dir->{int $_[0]{'..'}} = $_[0]{'..'}{FULLNAME}) . '/' . $_[0]{NAME}
				# May as well use absolute filename,
      unless $_[2] or @dirs1 && @dirs2 && $dirs2[-1] == $dirs1[-1];
				# when only thing in common is the root.
  }

  while (@dirs1 && @dirs2) {	# Peel off the top level directories
    last if $dirs2[-1] != $dirs1[-1]; # until we find a difference.
    pop @dirs2;
    pop @dirs1;
  }

 DONE:
  if( $_[2] ) {
    @dirs1 + @dirs2;
  } elsif( @dirs1 ) {
    my $file = shift @dirs1;
    ($orig_dir->{int $_[0]{'..'}} = join '/', ('..') x @dirs2, map $_->{NAME}, reverse @dirs1) .
      '/' . $file->{NAME};
				# Form the relative filename.
  } else {
    $orig_dir->{int $_[0]} = join '/', ('..') x @dirs2;
				# Form the relative filename.
  }
}

=head2 stat_array

   $statinfo = stat_array( $fileinfo );
   $statinfo = stat_array('filename');
   $uid = $statinfo->[STAT_SIZE];	# Or whatever field you're interested in.

Returns the array of values returned by the C<stat> function on the file.  The
values are cached, so calling this repeatedly entails only minimal extra
overhead.

If the file is a symbolic link, this returns the stat values for the file the
link refers to.

=cut

sub stat_array {
  my $finfo = $_[0];
  my $stat_arr = $finfo->{LSTAT} || &lstat_array; # Get lstat value.

  for( 0..20 ) {
    @$stat_arr or return $empty_array;
    exists $finfo->{LINK_DEREF} or return $stat_arr;

    $finfo = $finfo->{LINK_DEREF} ||= # Have we already dereferenced it?
      file_info( readlink absolute_filename_nolink( $finfo ), $finfo->{'..'} );
				# Get what it refers to (and if it's
				# a link, get what that refers to also).
    $stat_arr = $finfo->{LSTAT} || lstat_array( $finfo );
  }
  die 'symlink: infinite loop trying to resolve symbolic link ', &absolute_filename, "\n";
}

=head2 dir_stat_array

Similar to stat_array, except that you need to call this instead if it's
a directory and you need to get accurate timestamps or link counts.

=cut
sub dir_stat_array {
  # If READDIR is current, then LSTAT is also guaranteed to be current.
  # Otherwise, we make READDIR current, which updates LSTAT.
  &read_directory
    unless $_[0]{READDIR} && $_[0]{READDIR} == $epoch;
  goto &stat_array;
}

=head2 unlink

   Mpp::File::unlink( $fileinfo );

Removes the file and marks it in the cache as non-existent.

=cut

sub unlink {
  my $fileinfo = $_[0];		# Get the Mpp::File struct.

  CORE::unlink &absolute_filename_nolink; # Delete the file.
  delete @{$_[0]}{qw(EXISTS LSTAT SIGNATURE LINK_DEREF HAVE_READ_PERMISSION IS_READABLE)};
				# Mark the file as non-existent.  Don't
				# delete the fileinfo struct because it
				# might contain make build info or other stuff.
}

###############################################################################
#
# Internal subroutines (don't call these):
#

#
# This subroutine is called as soon as we discover that a file is actually
# a directory.	Knowing that something is actually a directory is very
# important for the wildcard routines, especially those with wildcards
# like '**/*.cxx'.
#
# Argument: the Mpp::File structure of the thing we discovered is a directory.
#
sub mark_as_directory {
  return if $_[0] == $root;
  $_[0]{DIRCONTENTS} ||= {}; # Mark as a directory.
  $_[0]{FULLNAME} = &absolute_filename;
				# We cache the absolute filename for all
				# directories for performance reasons.
}

#
# Subroutine to activate any wildcard routines which might be waiting to hear
# about this file.  This subroutine is called whenever a file might possibly
# be new.
#
# In order to allow wildcard subroutines to be run on files created during
# execution, and not just on files which existed when the wildcard was
# first seen, we store up a subroutines in each directory to be activated
# when a new file matches a given pattern.  This subroutine is responsible for
# activating them.
#
# A file will be published even if it's been published before if $level
# (default 0) is greater than the level with which it was previously published.
# Makepp uses this to implement --rm-stale, because a file needs to be
# re-published if it looked like a stale file the last time it was published,
# but a rule for the file was learned.
#
sub publish {
  return if exists $_[0]{IS_PHONY} # Don't do anything if it's a phony target.
    or exists $_[0]{PUBLISHED} && $_[0]{PUBLISHED} > ($_[1] || 0);
				# Don't do anything if we already published
				# this file.
  my( $finfo ) = @_;
  $finfo->{PUBLISHED} = ($_[1] || 0) + 1;

  my $fname = $finfo->{NAME};	# Form the relative filename.
  my $dirinfo = $finfo->{'..'};	# Find the directory that contains it.
  my $leaf = 1;
  my $stale;

  while ($dirinfo) {		# Go until we hit the top level.
    for( @{$dirinfo->{WILDCARD_ROUTINES}} ) {
				# Check each wildcard match specified to start
				# in this directory.
      # my( $re, $wild_rtn, $deep ) = @$_;
      next unless $leaf || $_->[2];
      next if $fname !~ $_->[0];
      if( $Mpp::rm_stale_files ) {
	$stale = &is_stale unless defined $stale;
	next if $stale;
	$finfo->{PUBLISHED} = 2;
      }
      $_->[1]( $finfo, 1 );	# Call the wildcard action routine if it matches.
    }

    substr $fname, 0, 0, $dirinfo->{NAME} . '/';
				# Form the relative filename of the next level.
    $dirinfo = $dirinfo->{'..'}; # Go up a level.
    undef $leaf;
  }
}

$CWD_INFO = file_info cwd;
				# Store the current directory so we know how
				# to handle relative file names.
#
# One unfortunate complication with the way we scan for include files in
# makepp is that when the user switches to root to do the 'make install',
# a different set of directories is now readable.  This may cause directories
# which used to be non-writable to become writable, which means that makepp
# will scan them for include files.  This means that the list of dependencies
# may change, and therefore recompilation may be forced.  We try to get around
# this with a special purpose hack where if we're running as root, we
# actually do the check with the UID and GID of whoever owns the directory.
#
@ids_for_check = (stat absolute_filename $CWD_INFO)[4, 5]
				# Use the IDs of whoever owns the current directory,
  unless $>;			# if we running as root?

$ENV{HOME} ||= (Mpp::is_windows > 0 ? $ENV{USERPROFILE} : eval { (getpwuid $<)[7] }) || '.';
dereference file_info $ENV{HOME};
				# Make sure we get a symbolic name for the home directory.

1;


__END__

Here is a review of which functions are safe to call as &fn, i.e. reusing your
own stack.  This is to make sure that no problem arises if the calling
function may itself have been called with more arguments.  Therefore functions
with a variable number of arguments should be called with an explicit
parameter list.  That way the calling function doesn't inherit the weakness of
a variable list.  The function name can be evited, calling either
absolute_filename or relative_filename with 2 args.

Primary functions, i.e. which call no others as &fn, so extra args are ok,
unless there are optional args:

absolute_filename (1 arg)
absolute_filename_nolink (1 arg)
assume_unchanged (1 arg)
build_info_fname (1 arg)
dont_build (1 arg)
dont_read (1 arg)
in_sandbox (1 arg)
mark_build_info_for_update (1 arg)
may_have_changed (1 arg)
parent (1 arg)
publish (1 or 2 args)
touched_filesystem (0 args)
update_build_infos (0 args)

Secondary functions, which have been checked to hand down no further args, no
matter how many extra args get passed in:

check_for_change (1 arg)
dereference (1 arg)
dir_stat_array (1 arg)
file_exists (1 arg)
have_read_permission (1 arg)
is_dir (1 arg)
is_or_will_be_dir (1 arg)
is_stale (1 arg)
is_symbolic_link (1 arg)
is_writable_owner (1 arg)
load_build_info_file (1 arg)
lstat_array (1 arg)
mark_as_directory (1 arg)
mkdir (1 arg)
name (1 or 2 args)
read_directory (1 arg)
relative_filename (1 to 3 args)
signature (1 arg)
stat_array (1 arg)
unlink (1 arg)
was_built_by_makepp (1 arg)

=head1 AUTHOR

Gary Holt (holt@lnc.usc.edu)

=cut
