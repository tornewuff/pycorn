# $Id: RecursiveMake.pm,v 1.1 2008/06/01 21:55:24 pfeiffer Exp $
#
# This file groups all the functions needed only if repositories are actually used.
# The actual functionality is also dispersed in makepp and other modules.
#

package RecursiveMake;

use FileInfo;
use TextSubs;
use MakeEvent qw(wait_for read_wait);

our $traditional;		# 1 if we invoke makepp recursively, undef if
				# we call the recursive_makepp stub and do
				# the build in the parent process.

#
# Set up our socket for listening to recursive make requests.  We don't do
# this unless we actually detect the use of the $(MAKE) variable.
#
our( $socket, $socket_name );
sub setup_socket {
  return if $socket_name; # Don't do anything if we've already
				# made the socket.
#
# Get a temp name that goes away at the end, so we don't clutter up /tmp.
#
  $socket_name = Makesubs::f_mktemp '/tmp/makepp.';
				# Name of socket for listening to recursive
				# make requests.  This is exported to the
				# environment by Rule::execute.
  require IO::Socket;
  $socket =
    IO::Socket::UNIX->new(Local => $socket_name,
			  Type => eval 'use IO::Socket; SOCK_STREAM',
			  Listen => 2) or
				# Make the socket.
    die "$progname: can't create socket $socket_name\n";
  chmod 0600, $socket_name;
				# Don't let other people access it.
  read_wait $socket, \&connection;
}


#
# This subroutine is called whenever a connection is made to the recursive
# make socket.
#
sub connection {
  my $connected_socket = $_[0]->accept(); # Make the connection.
  return unless $connected_socket; # Skip failed accepts.  I guess this might
				# happen if the other process has already
				# exited.
#
# Set up a few data items about this stream.  These will be passed thruogh
# the closure to the actual read routine.
#
  my $whole_command = '';	# Where we accumulate the whole command
				# from the recursive make process.
  my $read_sub;
  $read_sub = sub {
#
# This subroutine is called whenever we get a line of text through our
# recursive make socket.
    my $fh = $_[0];		# Access the file handle.
    my $line;
    if (sysread($fh, $line, 8192) == 0) {	# Try to read.
      $fh->close;		# If we got 0 bytes, it means the other end
				# closed the socket.
      return;
    }
    $whole_command .= $line;	# Append the line.
    if ($whole_command =~ s/^(.*)\01END\01//s) {
				# Do we have the whole command
				# now?
      my @lines = split(/\n/, $1); # Access each of the pieces.
      my @words = unquote_split_on_whitespace shift @lines;
				# First one is the set of arguments to
				# parse_command.
      my %this_ENV;		# Remaining lines are environment variables.
      foreach (@lines) {
	if( s/^([^=]+)=// ) {	# Correct format?
	  $this_ENV{$1} = unquote $_; # Store it.
	} else {
	  die "illegal command received from recursive make process:\n$_\n";
	}
      }
#
# We've now got all of the environment and command words.  Start executing
# them.
#
      chdir shift @words;	# Move to the appropriate directory.
      MakeEvent::Process::adjust_max_processes(1); # Allow one more process to
				# run simultaneously.
      my $status;
      eval {
	local @ARGV = @words;
        $status = wait_for ::parse_command_line %this_ENV;
				# Build all the targets.
      };
      MakeEvent::Process::adjust_max_processes(-1); # Undo our increment above.
      my $msg = $@;
      if ($msg) {               # Some error occured that caused a die?
        $status ||= 1;          # Force an error code.
      }
      print $fh ($status || '0') . " $msg";
				# Send the result to the recursive make
				# process.
      close $fh;                # Force a close immediately.
    }
    else {
      read_wait $fh, $read_sub;	# Prepare to read another line.
    }
  };

  read_wait $connected_socket, $read_sub; # Start the initial read.
  read_wait $_[0], \&connection; # Requeue listening.
}

#
# This is the actual function which overloads the stub.
#
no warnings 'redefine';

#
# $(MAKE) needs to expand to the name of the program we use to replace a
# recursive make invocation.  We pretend it's a function with no arguments.
#
our $command;			# Once this is set, we know we can potentially have recursion.
sub Makesubs::f_MAKE {
  if( defined $traditional ) {	# Do it the bozo way?
    unless( defined $command ) { # Haven't figured it out yet?
      $command = $0;		# Get the name of the program.
      unless( $command =~ m@^/@ ) { # Not absolute?
#
# We have to search the path to figure out where we came from.
#
	foreach( TextSubs::split_path(), '.' ) {
	  my $finfo = file_info "$_/$0", $::original_cwd;
	  if( file_exists $finfo ) { # Is this our file?
	    $command = FileInfo::absolute_filename $finfo;
	    last;
	  }
	}
      }
    }
    ::PERL . ' ' . $command . ' --recursive_makepp';
				# All the rest of the info is passed in the
				# MAKEFLAGS environment variable.
				# The --recursive option is just a flag that
				# helps the build subroutine identify this as
				# a recursive make command.  It doesn't
				# actually do anything.
  } else {
    die "makepp: recursive make without --traditional-recursive-make only supported on Cygwin Perl\n"
      if ::is_windows < -1 || ::is_windows > 0;

    my $makefile = $_[1];	# Get the makefile we're run from.

    $command ||= ::PERL . ' ' .
      FileInfo::absolute_filename( file_info $::datadir, $::original_cwd ) .
	'/recursive_makepp';
				# Sometimes we can be run as ../makepp, and
				# if we didn't hard code the paths into
				# makepp, the directories may be relative.
				# However, since recursive make is usually
				# invoked in a separate directory, the
				# path must be absolute.
    $makefile->cleanup_vars;
    join ' ', $command,
      map { "$_=" . requote $makefile->{COMMAND_LINE_VARS}{$_} } keys %{$makefile->{COMMAND_LINE_VARS}};
  }
}

1;
