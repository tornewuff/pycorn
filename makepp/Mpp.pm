# $Id: Mpp.pm,v 1.5 2009/03/19 22:40:11 pfeiffer Exp $

=head1 NAME

Mpp - Common subs for makepp and makeppreplay

=head1 DESCRIPTION

This package contains basic stuff for makepp.

=cut

package Mpp;

use strict;

use Mpp::Text;

#
# Signal handling and exiting
#
# Do this early, because the END block defined below shall be the first seen
# by perl, such that it is the last executed.  Unless we need to propagate a
# signal, it leaves the process via POSIX::_exit, so that no expensive garbage
# collection of Mpp::File objects occurs.  All other places can use die or normal
# exit.  If you define additional END blocks in any module, you must take care
# to not reset $?.
#
{
  my (%signo, @signame);
  if(defined(my $sig_name = $Config{sig_name})) {
    my $i=0;
    for my $name (split(' ', $sig_name)) {
      $signo{$name} ||= $i;
      $signame[$i] = $name;
      $i++;
    }
  }
  sub signo {
    $signo{$_[0]} || $_[0];
  }
  sub signame {
    $signame[$_[0]] || $_[0];
  }
}

our $int_signo = signo 'INT';
our $quit_signo = signo 'QUIT';

my $logfh;
our @close_fhs = \(*STDOUT, *STDERR);
our $critical_sections = 0;

our $n_files_changed = 0;	# Keep track of the number of files that
				# we actually changed.
our $n_phony_targets_built = 0;	# Keep track of the number of phony targets
                                # built, too.
our $error_found = 0;		# Non-zero if we found an error.  This is used
				# to stop cleanly when we are doing a parallel
				# build (unless -k is specified).
our $failed_count = 0;		# How many targets failed.  TODO: my when moving build et al here
our $build_cache_hits = 0;	# Number of the files changed that were
				# imported from a build cache.
our $rep_hits = 0;		# Number of the files changed that were
				# imported from a rep.
our $log_level = 2;		# Default to logging. 1: STDOUT, 2: $logfile
sub log($@);

my @signals_to_handle = is_windows > 0 && is_perl_5_6 ?
  () :				# ActiveState 5.6 doesn't define signals.
  qw/INT QUIT HUP TERM/;
{
  my %pending_signals;
  sub suicide {
    my ($sig) = @_;
    $SIG{$sig} = 'DEFAULT';

    # If we're propagating a signal from a subprocess that produced a core
    # dump, then we want to propagate the same signal without overwriting
    # the core file.  This is true even if the subprocess didn't produce a
    # core dump, because it could be propagating a signal from its child
    # that did produce a core dump.
    $sig = 'INT' if $sig eq 'QUIT' &&
      !(require BSD::Resource && BSD::Resource::setrlimit( &BSD::Resource::RLIMIT_CORE, 0, 0 ));

    kill $sig, $$;
    POSIX::_exit(0x80 | signo($sig)); # just in case;
  }
  sub handle_signal {
    my $sig = $_[0];

    # Do nothing on SIGINT or SIGQUIT if there are external processes.  These
    # signals are sent to all child processes as well, and if any of those
    # processes propagates them, then we will too.  Otherwise we ignore them
    # completely (which is the UNIX convention).
    return if $Mpp::Event::n_external_processes &&
      ($sig eq 'INT' || $sig eq 'QUIT');

    $pending_signals{$sig} = 1;
    # If there's nothing that we absolutely have to do before terminating, then
    # just terminate.
    exit unless $critical_sections;

    &Mpp::Event::Process::terminate_all_processes;
  }
  # This gets called after a critical section is completed:
  sub propagate_pending_signals {
    die if $critical_sections<0;
    return if $critical_sections;
    exit 2 if grep $_, values %pending_signals;
  }
  sub reset_signal_handlers {
    @SIG{@signals_to_handle} = ( 'DEFAULT' ) x @signals_to_handle;
  }
  # Autovivification of hash elements probably isn't reentrant, so initialize
  # them here in case we're using Perl 5.6.
  @pending_signals{@signals_to_handle} = ( 0 ) x @signals_to_handle;

  END {
    Mpp::log N_REP_HITS => $rep_hits
      if $log_level && $rep_hits;
    Mpp::log N_CACHE_HITS => $build_cache_hits
      if $log_level && $build_cache_hits;
    Mpp::log N_FILES => $n_files_changed, $n_phony_targets_built, $failed_count
      if defined $logfh;		    # Don't create log for --help or --version.
    if( exists $Devel::DProf::{VERSION} ) { # Running with profiler?
      warn "Doing slow exit, needed for profiler.\n";
    } else {
      close $_ for @close_fhs;
      $pending_signals{$_} && suicide $_ for keys %pending_signals;
      POSIX::_exit $?;
    }
  }

}
@SIG{@signals_to_handle} = ( \&handle_signal ) x @signals_to_handle;

my $invocation = join_with_protection($0, @ARGV);

our $progname;

for( qw(/usr/xpg4/bin/sh /sbin/xpg4/sh /bin/sh) ) {
  if( -x ) {
    $ENV{SHELL} = $_;		# Always use a hopefully Posix shell.
    last;
  }
}
delete $ENV{PWD};		# This is dangerous.

our $indent_level = 0;		# Indentation level in the log output.
our $keep_going = 0;		# -k specified.
my $logfile;			# Other than default log file.
our $parallel_make = 0;		# True if we're in parallel make mode.
our $profile = 0;		# Log messages about execution times.
our $verbose;
our $silent_execution;

$SIG{__WARN__} = sub {
  my $level = ($_[0] =~ /^(?:error|info): /s) ? '' : 'warning: ';
				# TODO: should we return if !$warn_level && $1 ne 'error'?
  print STDERR "$progname: $level$_[0]";
  if( $log_level == 2 ) {
    &Mpp::log() unless defined $logfh; # Only open the file.
    print $logfh "*** $level$_[0]";
  }
};



=head2 Mpp::log

  Mpp::log KEY => {object | array of objects | string} ...
    if $log_level;

The list of available KEYs is present in makepplog.  If you pass an non-key
str if will later be output verbatim.  The objects must have a method `name'.

This log overrides logarithm (which is not needed by makepp).  Because of
this, and because it is not exported, it must always be invoked as Mpp::log.

The log format contains a few control chars, denoted here as ^A, ^B ...

The first line is special, the invocation preceded by "logversion^A" as
explained at @obsolete_msg in makepplog.

A leading ^B is stripped, but tells makepplog to outdent, and a leading ^C to
indent.  After that a line with no ^A is considered plain text.  Else it is
split on the ^A`s.  There must be a ^A at the line end, which allows having
multine fields between ^A`s.  If the resulting fields contain ^B`s they are
lists of simple fields, else just one simple field.

The first field is a message I<key>.  The others work as follows.  If the
simple fields contain ^C`s they are ref definitions of up to 4 fields:

    ref[^Cname[^Cdir-ref[^Cdir-name]]]

The I<refs> are numbers (hex on HP/UX with 64bit pointers) and the I<names>
are to be remembered by makepplog for these numbers.  If a I<dir-name> is
present, that is remembered for I<dir-ref>, else it has been given earlier.
If a I<dir-ref> is given, that is prependended to I<name> with a slash and
remembered for I<ref>.  Else if only I<name> is given that is remembered for
I<ref> as is.  If I<ref> is alone, it has been given earlier.  Makepplog will
output the remembered name for refs between quotes.

The fields may also be plain strings if they contain no known I<ref>.  Due to
the required terminator, the strings may contain newlines, which will get
displayed as \n.  For keys that start with N_, all fields are treated as plain
numbers, even if they happen to coincide with a I<ref>.

=cut

use Mpp::File;

my $last_indent_level = 0;
sub log($@) {

  # Open the log file if we haven't yet.  Must do it dynamically, because
  # there can be messages (e.g. from -R) before handling all options.
  unless( defined $logfh ) {
    if( $log_level == 1 ) {
      (my $mppl = $0) =~ s/\w+$/makepplog/;
      -f $mppl or
	substr $mppl, 0, 0, absolute_filename( $Mpp::original_cwd ) . '/';
      open $logfh, '|' . PERL . " $mppl -pl-" or # Pass the mesages to makepplog for formatting.
	die "$progname: can't pipe to `makepplog' for verbose option--$!\n";
    } else {
      if( !$logfile ) {
	mkdir '.makepp';	# Just in case
	$logfile = '.makepp/log';
      }
      unless( open $logfh, '>', $logfile ) {
	warn "$progname: can't create log file ./$logfile--$!\n";
	$log_level = 0;
	return;
      }
    }
    push @close_fhs, $logfh;
    print $logfh "2\01$invocation\n";

    # If we're running with --traditional-recursive-make, then print the directory
    # when we're entering and exiting the program, because we may be running as
    # a make subprocess.

    Mpp::Rule::print_build_cwd( $CWD_INFO )
      if defined $Mpp::Recursive::traditional;

    return unless @_;		# From __WARN__
  }

  print $logfh
    join "\01",
      $indent_level == $last_indent_level ? shift() : # Cheaper than passing a slice to map
	($indent_level < $last_indent_level ? "\02" : "\03") . shift,
      map( {
	if( !ref ) {
	  $_;
	} elsif( 'ARRAY' eq ref ) {
	  join "\02", map {		# Array shall only contain objects.
	    if( exists $_->{LOGGED} ) {	# Already defined
	      int;			# The cheapest external representation of a ref.
	    } elsif( !exists $_->{'..'} ) { # not a Mpp::File
	      # TODO: These two liness are a reminder for when we store RULE_SOURCE per ref.
	      #undef $_->{LOGGED};
	      #int() . "\03" . $_->name;
	      $_->name;
	    } elsif( exists $_->{'..'}{LOGGED} ) { # Dir already defined
	      undef $_->{LOGGED};
	      int() . "\03$_->{NAME}\03" . int $_->{'..'};
	    } else {
	      undef $_->{LOGGED};
	      undef $_->{'..'}{LOGGED};
	      int() . "\03$_->{NAME}\03" .
		int( $_->{'..'} ) . "\03" . ($_->{'..'}{FULLNAME} || absolute_filename $_->{'..'});
	    }
	  } @$_;
# The rest is a verbatim copy of the map block above.  This function is heavy
# duty, and repeating code is 6% faster than calling it as a function, even
# with &reuse_stack semantics.
	} elsif( exists $_->{LOGGED} ) {	# Already defined
	  int;			# The cheapest external representation of a ref.
	} elsif( !exists $_->{'..'} ) { # not a Mpp::File
	  # TODO: These two liness are a reminder for when we store RULE_SOURCE per ref.
	  #undef $_->{LOGGED};
	  #int() . "\03" . $_->name;
	  $_->name;
	} elsif( exists $_->{'..'}{LOGGED} ) { # Dir already defined
	  undef $_->{LOGGED};
	  int() . "\03$_->{NAME}\03" . int $_->{'..'};
	} else {
	  undef $_->{LOGGED};
	  undef $_->{'..'}{LOGGED};
	  int() . "\03$_->{NAME}\03" .
	    int( $_->{'..'} ) . "\03" . ($_->{'..'}{FULLNAME} || absolute_filename $_->{'..'});
	}
      } @_ ),
      "\n";
  $last_indent_level = $indent_level;
}

=head2 flush_log

Flush the log file and standard file handles.  This is useful for making sure
that output of a perl action is not lost before the action's process
terminates with POSIX::_exit.

=cut

sub flush_log {
  my $fh = select STDOUT; local $| = 1;
  select STDERR; $| = 1;
  if( defined $logfh ) { select $logfh; $| = 1 }
  select $fh;
}

my $hires_time;
sub print_profile {
  print $profile ? "$progname: Begin \@" . &$hires_time() . " $_[0]\n" : "$_[0]\n";
}

sub print_profile_end {
  print "$progname: End \@" . &$hires_time() . " $_[0]\n";
}

=head2 print_error

  print_error "message", ...;

Prints an error message, with the program name prefixed.  For any arg which is
a reference, $arg->name is printed.

If any filename is printed, it should have be quoted as in `filename' or
`filename:lineno:' at BOL, so that IDEs can parse it.

=cut

sub print_error {
  my( $log, $stderr ) = $log_level && '*** ';
  my $str = '';
  if( $_[0] =~ /^error()/ || $_[0] !~ /^\S+?:/ ) { # No name?
    $stderr = "$progname: ";
    $str = 'error: ' unless defined $1;
  }
  $str .= ref() ? $_->name : $_
    for @_;
  $str .= "\n" if $str !~ /\n\z/;
  print STDERR $stderr ? $stderr . $str : $str;
  if( $log_level == 2 ) {
    &Mpp::log() unless defined $logfh; # Only open the file.
    print $logfh $log . $str;
  }
  &flush_log;
}


sub perform(@) {
  #my @handles = @_;		# Arguments passed to wait_for.
  my $status;
  my $start_pid = $$;
  my $error_message = $@ || '';
  $error_message or eval { $status = &wait_for }; # Wait for args to be built.
				# Wait for all the children to complete.
  $error_message .= $@ if $@;	# Record any new error messages.
  {
    my $orig = '';
    if($error_message) {
        chomp($orig = $error_message);
        $orig = " (Original error was $orig)";
    }

    if( $$ != $start_pid ) {
      print STDERR qq{makepp internal error: sub-process died or returned to outer scope.
If we had not caught this, it would cause exit blocks to run multiple times.
Use POSIX::_exit instead.$orig Stopped};
      close $_ for @close_fhs;
      POSIX::_exit 3;
    }

    # Wait for our last jobs to finish.  This can be the case when running
    # with -kj<n>, a job failed and we ran out of queued jobs.
    &Mpp::Event::event_loop
      while $Mpp::Event::n_external_processes;

    die qq{makepp internal error: dangling critical section.
This means that there was a live process in the background when makepp
died, so makepp did not have a chance to create build info files for
targets generated by that process.  It also means that makepp can't
propagate signals.  This could instead mean that you need an extra 'eval'
somewhere to prevent an exception from bypassing process accounting and
signal propagation.$orig Stopped}
      if $critical_sections;

    if( $error_found && $error_found =~ /^signal ($int_signo|$quit_signo)$/ ) {
      my $sig = $1;
      handle_signal signame($sig);
    }
  }

  if( $n_files_changed || $rep_hits || $build_cache_hits || $n_phony_targets_built || $failed_count ) {
    print "$progname: $n_files_changed file" . ($n_files_changed == 1 ? '' : 's') . ' updated' .
      ($rep_hits ? ", $rep_hits repository import" . ($rep_hits == 1 ? '' : 's') : '') .
      ($build_cache_hits ? ", $build_cache_hits build cache import" . ($build_cache_hits == 1 ? '' : 's') : '') .
      ($error_found ? ',' : ' and') .
      " $n_phony_targets_built phony target" . ($n_phony_targets_built == 1 ? '' : 's') . ' built' .
      ($failed_count ? " and $failed_count target" . ($failed_count == 1 ? '' : 's') . " failed\n" : "\n");
  } elsif( !$error_message ) {
    print "$progname: no update necessary\n";
  }

  print "$progname: Ending \@" . &$hires_time() . "\n" if $profile;
  print_error $error_message if $error_message;
  exit 1 if $error_message || $status || !MAKEPP && $failed_count;
				# 2004_12_06_scancache has a use case for not failing despite $failed_count
  exit 0;
}

our @common_options =
  (
    ['n', qr/(?:just[-_]?print|dry[-_]?run|recon)/, \$Mpp::dry_run],

    ['k', qr/keep[-_]?going/, \$keep_going],

    [undef, qr/log(?:[-_]?file)?/, \$logfile, 1],

    [undef, qr/no[-_]?log/, \$log_level, undef, 0], # Turn off logging.

    [undef, 'profile', \$profile, undef, sub {
       if( !$hires_time ) {
	 eval { require Time::HiRes };
	 $hires_time = $@ ? sub { time } : \&Time::HiRes::time;
	 print "$progname: Beginning \@" . &$hires_time() . "\n";
       }
     }],

    [qw(s silent), \$silent_execution],

    [qw(v verbose), undef, undef, sub {
       $verbose = 2;		# Value 2 queried only by Mpp/Cmds.
       $log_level = 1;		# Send the log to stdout instead.  Don't make
				# this the option variable, as it must be
				# exactly 1, not just true.
     }],
    [qr/[h?]/, 'help', undef, undef, sub { local $/; print <DATA>; exit 0 }],
    [undef, 'version', undef, undef, \&Mpp::File::version]
  );

1;
