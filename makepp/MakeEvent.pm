# use strict qw(vars subs);

# $Id: MakeEvent.pm,v 1.22 2008/05/17 14:38:05 pfeiffer Exp $

package MakeEvent;

require Exporter;
our @ISA = 'Exporter';

our @EXPORT_OK = qw(wait_for when_done read_wait);

=head1 NAME

MakeEvent -- event loop for makepp

=head1 USAGE

  use MakeEvent;

  $handle = new MakeEvent::Process STDOUT => ">> /tmp/junk", STDIN => undef,
    "shell command to execute";
  $handle = new MakeEvent::Process sub { ... };

  $handle = when_done $handle1, $handle2, ..., sub {
  # Code that gets executed when the previous handles have finished.
  };

  $status = $handle->status;

  $status = wait_for $handle1, $handle2, ...;

  read_wait FILEHANDLE, sub { ... };
				# Called when there is data to be read on
				# the given file handle.

=head1 DESCRIPTION

MakeEvent provides a way of multi-threading perl code without actually using
any of the thread extensions.  It relies on perl closures instead.  So it's a
little harder to write event-driven code than it would be if your code were
threaded, but not much.

It also supports waiting for input availability on an arbitrary set of
channels using the IO::Select module.  Currently, it does not support waiting
to write or waiting for exceptions.

=cut

#
# Some global variables:
#
our $max_proc = 1;		# Set this to be the maximum number of
				# subprocesses to run simultaneously.
				# By default, we can only run one, which
				# means that when we try to start up a second
				# one, we wait for the first.
our $n_external_processes = 0;	# Number of processes currently running.

our $fork_level = 0;		# How many times we've forked.

#
# Some internal variables:
#
my $child_exited;		# 1 if we've received a SIGCHLD.

my $read_vec;			# Vector of file handles that we are listening
				# to.
my @read_handles;		# The file handles or FileHandles or globs
				# that we're waiting for, indexed by the
				# fileno (same as index into $read_vec).
my @read_subs;			# The read subroutines associated with each
				# of the handles in @read_handles (also
				# indexed by fileno).

=head2 MakeEvent::event_loop

  &MakeEvent::event_loop;

This is the main event loop.  It waits for one event, processes it, and
returns.  You probably don't want to call this function directly; most likely,
you want MakeEvent::wait_until.

=cut

sub event_loop {
  unless( defined $child_exited ) { # Do not call select() if we already have stuff
				# to do.  That can cause a hang.
#
# Check for file handles which can be read.  We used to use IO::Select but
# it's buggy (doesn't even bother to call the select function if no
# handles have been specified--not a friendly interface!).
#
    my $r = $read_vec;		# Make a modifiable copy of the list of file
				# handles to wait for.
    my $n_handles = select $r, undef, undef, 5;
				# Supply a 5s timeout, so we do not wait
				# forever if the signal happened to come
				# between when we tested select_finished_subs
				# and when we called select.
    if( $n_handles > 0 ) {	# Data available on any handles?
				# Scan backwards to find out which handles it
				# might have been on, since we are more likely
				# to be waiting on later file handles.
      for (my $fileno = @read_handles; $fileno >= 0; --$fileno) {
	if (vec($r, $fileno, 1)) { # This bit returned on?
	  my $read_sub = $read_subs[$fileno]; # Get the subroutine.
	  my $fh = $read_handles[$fileno];
	  vec($read_vec, $fileno, 1) = 0; # Do not wait for it again (unless it
	  undef $read_subs[$fileno]; # is requeued).
	  undef $read_handles[$fileno];
	  defined $read_sub and &$read_sub($fh); # Call the subroutine.
	}
      }
    }
  }

#
# Check for other kinds of interruptions:
#
  &MakeEvent::Process::process_reaper
    if defined $child_exited;	# Need to wait() on child processes?
}

=head2 read_wait

  read_wait FILE_HANDLE, sub { ... };

Queue a subroutine to be activated whenever there is data on the given file
handle (or IO::Handle object, or anything that can be supplied as an argument
to IO::Select::new.

This is a one-shot queue.  You must call read_wait again if you want the
subroutine to be called again.

=cut

sub read_wait {
  my ($fh, $subr) = @_;		# Name the arguments.

  my $fileno = fileno($fh) || $fh->fileno;
  $read_vec ||= '';		# Avoid usage of undefined variable errors.
  defined $fileno or die "internal error";
  vec($read_vec, $fileno, 1) = 1; # Wait on this file handle.
  $read_handles[$fileno] = $fh;
  $read_subs[$fileno] = $subr;
}


#
# This is an internal subroutine which is called whenever a process exits,
# or whenever a subroutine that was waiting is called.	It handles any
# errors, and activates whoever was waiting for the subroutine.
#
# Arguments:
# a) The handle.
# b) The status.
#
sub process_finished {
  my ($handle, $status) = @_;	# Name the arguments.

  $handle->{STATUS} ||= $status; # Store the new status value.	Note that we
				# use ||= rather than =.
				# It's possible that the status value could
				# have been set by a previous error in one of
				# the things we were waiting for.  (See the
				# code below for error status.)	 In this
				# case, we want to set the status to error
				# if any of the things it was waiting for
				# had errors.

  if( $handle->{STATUS} && exists $handle->{ERROR_HANDLER} ) {
				# Is there an error and an error handler in
				# this handle?
    if( ref( $handle->{ERROR_HANDLER} ) eq 'CODE' ) {
      $handle->{CODE} = delete $handle->{ERROR_HANDLER};
				# Replace the subroutine with the error
				# handler instead.
      $handle->{ARGS} = [ delete $handle->{STATUS} ]; # Pass the status code to
				# the error handler, and get rid of it so
				# there's a place to put the return code from
				# the error handler.
      MakeEvent::WaitingSubroutine::start( $handle, 1 );
				# Start off the error handler despite status.
    } else {
      $handle->{STATUS} = $handle->{ERROR_HANDLER};
    }
  }

  my $waiters = $handle->{WAITING_FOR};

  if( exists $handle->{STATUS} ) {
    $status = $handle->{STATUS};
    %$handle = ();
    $handle->{STATUS} = $status;
  } else {
    %$handle = ();
  }				# Get rid of everything but the status, in
				# order to save memory, without creating new ref.
  return unless $waiters;	# Don't do anything if no one was waiting
				# for this.

  if( $handle->{STATUS} ) {	# Was there some error?
#
# If there was an error, don't activate routines which were waiting;
# activate the error routines, if any.
#
    foreach my $waiter (@$waiters) {
      $waiter->{STATUS} ||= $handle->{STATUS}; # Remember the error.
      if( --$waiter->{WAIT_COUNT} == 0 ) { # Were we the last thing waited for?
	if( exists $waiter->{ERROR_HANDLER} ) {
	  if( ref( $waiter->{ERROR_HANDLER} ) eq 'CODE' ) {
	    $waiter->{CODE} = delete $waiter->{ERROR_HANDLER};
				# Activate the error handler instead of the subroutine.
	    $waiter->{ARGS} = [ $waiter->{STATUS} ];
				# Set the status value.
	    MakeEvent::WaitingSubroutine::start( $waiter, 1 );
				# Start off the error handler despite status
				# it may already have gotten from another
				# dependency.
	  } else {
	    $waiter->{STATUS} = delete $waiter->{ERROR_HANDLER};
	    $waiter->{CODE} = \&TextSubs::CONST0;
	    MakeEvent::WaitingSubroutine::start( $waiter, 1 );
	  }
	} else {		# Just pass the error to this routine's caller,
	  process_finished($waiter, $handle->{STATUS}); # without invoking the routine.
	}
      }
    }
  } else {
#
# No error.  Activate whoever was waiting for this waiter.
#
    foreach my $waiter (@$waiters) {
      --$waiter->{WAIT_COUNT} == 0 and # Last thing this one was waiting for?
	$waiter->start;		# Start it up.
    }
  }
}

=head2 when_done

  $handle = when_done $handle1, $handle2, ..., sub { ... };
  $handle = when_done $handle1, $handle2, ..., sub { ... }, [subroutine args];
  $handle = when_done $handle1, $handle2, ..., sub { ... }, ERROR => sub { ... };
  $handle = when_done $handle1, $handle2, ..., sub { ... }, ERROR => $scalar;
  $handle = when_done $handle1, $handle2, ..., sub { ... }, [subroutine args], ERROR => sub { ... };
  $handle = when_done $handle1, $handle2, ..., sub { ... }, [subroutine args], ERROR => $scalar;

Calls the specified subroutine when the processes have finished or the other
subroutines have been called.  Scalar arguments are interpreted as handles, a
reference to a subroutine is treated as a subroutine to call, and a reference
to a list is a set of arguments to pass to the subroutine.

Ordinarily you would pass arguments into the subroutine via perl's
closure mechanism, but there do appear to be some bugs in this (in
perl 5.8.0) and so it is possible to pass arguments via an explicit
argument list.

The subroutine should return any of the following values:

=over 4

=item 0

0 indicates success, as with unix processes.

=item a non-zero value

A non-zero value indicates failure, and causes the error handler of any
subroutine waiting to be called.

=item a list of handles

Doesn't activate anyone waiting for this subroutine until each handle in
the list of handles has finished.

=item 'ERROR', then a subroutine reference

The subroutine is called if an error status was returned by any of the
handles.  On entry to the subroutine, $_[0] is the error status code.  The
subroutine should return a status code just like the usual when_done
subroutines.

=item 'ERROR', then a scalar

Assign the scalar as the status, instead of the propagated one.

=back

You can also specify an error handler, which is called if activity on any
of the given handles returns an error.	(It is not called if the subroutines
themselves return an error; the error handler of whoever is waiting on them
is called.)  The error handler should return the same type of status code as
the main routine.  The error handler is called with the same arguments as
the subroutines.

Instead of specifying the handles, you may also specify status codes.  (This
means that instead of keeping the handle object around, you can just store the
status code when the handle has finished.)

=cut

sub when_done {
  my @handles;
  my $subr;
  my $status;			# True if we were passed an error status
				# code as one of the arguments.
#
# Parse the arguments:
#
  for( @_ ) {
    if( $subr ) {
      if( ref eq 'ARRAY' ) {	# Subroutine arguments.
	$subr->{ARGS} = $_;
      } elsif( $_ eq 'ERROR' ) { # Indicates the error subroutine?
	$subr->{ERROR_HANDLER} = $_[-1];
	last;
      }
    } elsif( defined ) {	# Skip undef values--undef means success with no waiting.
      if( ref =~ /^MakeEvent::/s ) { # Is this a handle?
	if( exists $_->{STATUS} ) { # Did this handle already finish?
	  $status ||= $_->{STATUS};
	} else {		# No, we have to wait for it.
	  push @handles, $_;
	}
      } elsif( ref eq 'CODE' ) { # Is this a subroutine?
	$subr = new MakeEvent::WaitingSubroutine $_; # Store it.
      } else {			# Must be an error status code.
	$status ||= $_;
      }
    }
  }

#
# Now queue up the subroutines:
#

  $subr->{ARGS} ||= [];
  push @{$_->{WAITING_FOR}}, $subr for @handles;
				# Indicate that it is waiting for each one of these handles.
  $subr->{WAIT_COUNT} = @handles; # So we know when they are done.

  $subr->{STATUS} = $status if $status;
				# Store the proper error status.
  if( @handles ) {		# Waiting for anything now?
  } elsif( $status ) {		# No, did we already find an error?
    process_finished( $subr, $status ); # Don't even call the routine.
  } else {			# No, start it immediately.
    MakeEvent::WaitingSubroutine::start( $subr );
  }

  $subr;			# Return the handle for the subroutine so if
                                # someone waits for it, all the others have
                                # also finished.
}

=head2 wait_for

  $status = wait_for $handle1, $handle2, ...;

Waits for the processes or subroutines associated with the specified handles
to finish, and returns the status.

=cut

sub wait_for {
  my $status;
  when_done @_, sub {
    $status = 0;		# No error.
  },
  ERROR => sub {		# Called if there was an error.
    defined( $status = $_[0] )	# Store the status value.
      or goto DONE;		# Short circuit is more expensive.
  };

  defined $child_exited ? &MakeEvent::Process::process_reaper : &event_loop
    until defined $status;	# Wait until our subroutine is called.

 DONE:
  $status;
}

###############################################################################
#
# Structure used to keep track of processes that we've started up:
#

package MakeEvent::Process;

use TextSubs;

my @pending_processes;		# Where we store the processes that we want
				# to run but can't because too many others
				# are already running.
my %running_processes;		# A hash of processes that we start, indexed
				# by the PID.

#
# Start a new process, and return a structure which can be used to control it.
# Arguments:
# 1) The code to run.
# 4-...) Arguments to the subroutine.
#
# If the last two arguments are 'ERROR' and a subroutine, then the subroutine
# is called if the command returned an error.
#

#
sub new {
  my( $class, $subr, @args ) = @_; # Get the class, command & args to execute.

  my $errorsub;
  if (@args >= 2 && $args[-2] eq 'ERROR') { # Error handling routine?
    $errorsub = pop @args;
    pop @args;                     # Remove the 'ERROR' argument too.
  }

  my $proc = bless { CODE => $subr, PARAMS => \@args }, $class;
				# Store the information.
  $errorsub and $proc->{ERROR_HANDLER} = $errorsub;

#
# See if we can start this job (and maybe another one) immediately:
#
  if( $MakeEvent::n_external_processes < $MakeEvent::max_proc ) {
    start( $proc );
    start( shift @pending_processes )
      while @pending_processes && $MakeEvent::n_external_processes < $MakeEvent::max_proc;
  } else {
    push @pending_processes, $proc; # Queue it up.
  }

#
# If we can't start it immediately, wait until we can.	We don't want to get
# too far ahead of the build.
#
  &MakeEvent::event_loop while @pending_processes;

  $proc;
}

#
# Increment or decrement the maximum number of processes that can run.
# Arguments:
# a) The number to add to the maximum number of processes.
#
sub adjust_max_processes {
  $MakeEvent::max_proc += $_[0]; # Adjust the number of processes.

  start( shift @pending_processes )
    while @pending_processes && $MakeEvent::n_external_processes < $MakeEvent::max_proc;
}

#
# Return the status code:
#
sub status {
  exists $_[0]{STATUS} or
    die ref( $_[0] ) . "::status called too early";
  $_[0]{STATUS};
}

use POSIX qw(:signal_h :errno_h :sys_wait_h);

#
# This subroutine actually does the forking and starts the process.
#
sub start {
  my $self = $_[0];
  MakeEvent::process_finished( $self ), return if $self->{STATUS};
  if( ::is_windows > 0 ) {	# On Win ActiveState, we don't fork because the
				# operating system doesn't support this well.
    if (@{$self->{PARAMS}}) {
      die "makepp: internal error: parameters to MakeEvent::Process not supported on windows\n";
    }

    my $cmd = $self->{CODE};	# Get the thing to execute.
    my $status = 0;
    if( ref $cmd ) {
      $status = &$cmd();	# Call the subroutine.
    } else {			# Is this a string to execute as shell cmd?
      system format_exec_args $cmd;
      if( $? > 255 ) {		# Non-zero exit status?
	$status = $? >> 8;	# Use that as the status.
      } elsif( $? & 127 ) {	# Exited with a signal?
	$status = "signal " . ($? & 127);
      }
    }
    MakeEvent::process_finished($self, $status);
				# Store the status code.
    return;
  }

  my $pid;
  $SIG{CHLD} = sub { $child_exited = 1 }; # Call the reaper subroutine in the
				# mainline code.

  &::flush_log if ::is_perl_5_6;
  if( $pid = fork ) {		# In the parent process?
    $running_processes{$pid} = $self; # Store this for later.
    ++$MakeEvent::n_external_processes; # Keep track of how many things are running.
    return;
  }

#
# In the child process:
#
  $SIG{CHLD} = 'DEFAULT';	# Put the signal handler back the way it used
				# to be.  (Note: setting this to IGNORE rather
				# than DEFAULT causes a lot of hard-to-explain
				# errors; evidently automatic reaping of
				# processes interferes with system().)

  ++$MakeEvent::fork_level;
#
# Process the parameters.  These are instructions on how to set up
# STDIN, STDOUT, and STDERR.
#
  for (my $par_idx = 0; $par_idx < @{$self->{PARAMS}}; $par_idx += 2) {
    my $fh = $self->{PARAMS}[$par_idx]; # Get which file handle this is.
    close $fh;			# Close down whatever it used to be.
    if (defined($self->{PARAMS}[$par_idx+1])) {
      unless (open($fh, $self->{PARAMS}[$par_idx+1])) {
	my $errorcode = "$!";
	open(TTY, ">/dev/tty");
	print TTY "could not open $fh as " . $self->{PARAMS}[$par_idx+1] . "--$errorcode\n";
	exit 1;
      }
    }
  }

  my $cmd = $self->{CODE};	# Get the thing to execute.
  my $result = 0;
  if( ref $cmd ) {		# Is this a subroutine?
    local @Makesubs::temp_files; # Might call f_mktemp, but _exit bypasses END
    $result = &$cmd();		# Call the subroutine.
    unlink $_ for @Makesubs::temp_files;
  } elsif( ::is_windows ) {	# Fork is only emulated, hence we can't exec
    system format_exec_args $cmd;
    $result = int( $? / 256 ) || 255 if $?;
  } else {
    exec format_exec_args $cmd;
    die "exec $cmd failed--$!\n";
  }

  close $_ for @::close_fhs;
  POSIX::_exit $result;
}

#
# This subroutine is responsible for reaping dead children.  (Unix is
# pretty morbid.)  It grabs the status value for each child, and starts
# up anything which was waiting for them.
#
# This is not called as a signal handler.  The signal handler for SIGCHLD
# makes the event loop call us by setting a flag.
# Note that signals can be coalesced (i.e., if two children died, there might
# be only one SIGCHLD).	 I don't know what bozo designed the system that way,
# but we still have to live with it 30 years later.
#
# Most of this code is adapted from the perl cookbook.
#
sub process_reaper {
  my @procs;
  undef $child_exited;		# Clear the flag
  while( (my $pid = waitpid -1, WNOHANG) > 0 ) {
# Collect as many defunct processes as early as possible.
#
# Note again because of another bozo design decision in unix, waitpid may
# actually return the PID of a process which was stopped, not exited.  So we
# have to check for this.  TODO: Is this true?  The POSIX manpage states
# otherwise at WUNTRACED.
#
#    next unless WIFEXITED($?); # Make sure it really has exited.
# The above causes a hang, when the process did not exit normally.
    push @procs, delete $running_processes{$pid} || die;
				# Returned a process not started by new MakeEvent::Process?
    $procs[-1]{STATUS} ||= $? > 255 ? $? >> 8 : "signal " . ($? & 127)
      if $?;
  }

  $MakeEvent::n_external_processes -= @procs;

#
# If there were other processes waiting to be run, start some of them now that
# job slots are free.
#
  start shift @pending_processes
    while @pending_processes && $MakeEvent::n_external_processes < $MakeEvent::max_proc;

  MakeEvent::process_finished $_ for @procs;
}

#
# This terminates all child processes and prevents any others from being
# spawned (at least until adjust_max_processes is called), leaving the
# subroutines in place to be executed as the processes on which they
# depend finish.
#
sub terminate_all_processes {
  $MakeEvent::max_proc = 0;
  kill TERM => keys %running_processes;
  &process_reaper while %running_processes;
}


###############################################################################
#
# Structure used to keep track of subroutines that are waiting to be called.
# This structure also keeps track of ancilliary variables like the log
# file indentation level so when the subroutine is called, the indentation
# level is correct.
#

package MakeEvent::WaitingSubroutine;

sub new {
  #my ($classname, $perl_subr, $args) = @_;

  bless { CODE => $_[1],
	  ARGS => $_[2],
	  INDENT => $::indent_level,
	 }, $_[0];
				# Make the structure for the process.
}

*status = \&MakeEvent::ProcessHandle::status;

#
# Start the process running.  This is pretty simple--we just execute the code.
# The return value can be one of several things:
# o 0 (for success).  In this case, we activate whoever is waiting for this
#    subroutine to finish.
# o A non-zero value, which is an error code.  We find an error subroutine
#   and call it with this value.
# o One or more handles.  In this case, we only activate whoever is waiting for
#   this subroutine when all of the other handles have also finished.
#
sub start {
  my $this_subr = $_[0];	# Get a reference to the process.
  MakeEvent::process_finished( $this_subr ), return if @_ == 1 && $this_subr->{STATUS};

#
# Look at the return values and figure out what to do:
#
  my $status;

  if( $this_subr->{CODE} != \&TextSubs::CONST0 ) {
    local $::indent_level = $this_subr->{INDENT};
				# Set the indentation level properly.
    for( $this_subr->{CODE}( @{$this_subr->{ARGS}} )) {
      if( $_ ) {		# Not 0 or undef?
	if( ref =~ /^MakeEvent::/s ) { # Something else to wait for?
	  if( exists $_->{STATUS} ) { # Did that thing already finish?
	    $status ||= $_->{STATUS}; # Pick up its status.
	  } else {		# Hasn't finished yet, we need to wait for it.
	    $this_subr->{CODE} = \&TextSubs::CONST0;
				# Convert this subroutine into a dummy which
				# isn't harmful to call again.
	    push @{$_->{WAITING_FOR}}, $this_subr;
				# Mark this handle as waiting for the specified
				# other handle.
	    ++$this_subr->{WAIT_COUNT}; # Remember that we're waiting for one
				# more thing.
	  }
	} else {
	  $status = $_;		# Must be a non-zero status.
	}
      }
    }
  }

  MakeEvent::process_finished $this_subr, $status
    unless $this_subr->{WAIT_COUNT}; # Quit now if we're waiting for something else.
				# Activate anyone who's waiting for this
				# process.
}

1;

=head1 AUTHOR

Gary Holt (holt@LNC.usc.edu)
