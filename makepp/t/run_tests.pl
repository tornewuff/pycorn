#!/usr/bin/perl -w
#
# See bottom of file for documentation.
#
use Config;
use Cwd;
use File::Path;
use File::Copy 'cp';


#
# See if this architecture defines the INT signal.
#
my $sigint;
if(defined $Config{sig_name}) {
  my $i=0;
  for(split(' ', $Config{sig_name})) {
    $sigint=$i if $_ eq 'INT';
    ++$i;
  }
}

my $archive = $Config{perlpath}; # Temp assignment is work-around for a nasty perl5.8.0 bug
my $source_path;
my $old_cwd;
my $dot;
my $verbose;
my $test;
my $dotted;
our $makepp_path;

# Global constants for compile time check.
BEGIN {

  open OSTDOUT, '>&STDOUT' or die $!;
  open OSTDERR, '>&STDERR' or die $!;

  $old_cwd = cwd;		# Remember where we were so we can cd back here.

  if( $0 =~ m@/@ ) {		# Path specified?
    ($source_path = $0) =~ s@/[^/]+$@@; # Get the path to our script.
  } elsif( $ENV{PATH} =~ /[;\\]/ ) { # Find it in Win $PATH:
    foreach (split(/;/, $ENV{PATH}), '.') {
      my $dir = $_ || '.';	# Blank path element is .
      if( -e "$dir\\$0" ) {
	$source_path = $dir;
	last;
      }
    }
  } else {				# Find it in $PATH:
    foreach (split(/:/, $ENV{PATH}), '.') {
      my $dir = $_ || '.';	# Blank path element is .
      if( -x "$dir/$0" ) {
	$source_path = $dir;
	last;
      }
    }
  }
  $source_path or die "$0: something's wrong, can't find path to executable\n";
  $source_path =~ m@^/@ or $source_path = "$old_cwd/$source_path";
				# Make path absolute.
  $source_path =~ s@/(?:\./)+@/@;
  $source_path =~ s@/\.$@@;
  1 while
    ($source_path =~ s@/\.(?=/|$)@@) || # Convert x/./y into x/y.
    ($source_path =~ s@/[^/]+/\.\.(?=/|$)@@); # Convert x/../y into y.

  if( $ARGV[0] =~ s/\bm(?:ake)?pp$/makepp/ ) {
    $makepp_path = shift;
    $makepp_path =~ m@^/@ or $makepp_path = "$old_cwd/$makepp_path";
  } else {
    $makepp_path = $source_path;
    $makepp_path =~ s@/([^/]+)$@/makepp@; # Get the path to the makepp
				# executable, which should always be in the
				# directory above us unless given as arg.
  }

  push @INC, substr $makepp_path, 0, rindex $makepp_path, '/';
  unless( eval { require TextSubs } ) {
    open my $fh, '<', $makepp_path;
    while( <$fh> ) {
      if( /^\$datadir = / ) {
	eval;
	require TextSubs;
	last;
      }
      die "Can't locate path to makepp libraries." if $. == 99;
    }
  }

  TextSubs::getopts(
    [qw(d dots), \$dot],
    [qw(t test), \$test],
    [qw(v verbose), \$verbose],
    [qr/[h?]/, 'help', undef, 0, sub { print <<EOF; exit }] );
run_tests.pl[ path/to/makepp][ options][ tests]
    -d, --dots
	Output only a dot for every successful test.
    -t, --test
	Output in format expected by Test::Harness.
    -v, --verbose
	Give some initial info and final statistics.

    If no path to makepp is given, looks for it one directory higher.
    If no tests are given, runs all in the current directory.
EOF

  printf "%sPerl V%vd %dbits - %s %s\n",
    $Config{cf_email} =~ /(Active)(?:Perl|State)/ ? $1 : '',
    $^V, $Config{ptrsize} * 8, $^O, $Config{archname}
    if $verbose;

  mkdir 'd';
  my $symlink = (stat 'd')[1] &&	# Do we have inums?
    eval { symlink 'd', 'e' } &&	# Dies on MSWin32.
    (stat _)[1] == (stat 'e')[1];	# MinGW emulates symlink by recursive copy, useless for repository.
  rmdir 'd';
  unlink 'e' or rmdir 'e';
  eval 'sub no_symlink() {' . ($symlink ? '' : 1) . '}';
  open my $fh, '>f';		# Use different filename because rmdir may fail on Win
  close $fh;
  my $link = eval { link 'f', 'g' } &&	# might die somewhere
    ((stat 'f')[1] ?	# Do we have inums?
      (stat _)[1] == (stat 'g')[1] : # vfat emulates link by copy, useless for build_cache.

      (stat _)[3] == 2 && (stat 'g')[3] == 2); # Link count right?
  unlink 'f', 'g';
  eval 'sub no_link() {' . ($link ? '' : 1) . '}';
  # Under 5.6.2 (at least on linux) we cannot repeatedly test for require :-(
  eval 'sub no_md5() {' . ($] > 5.007 || eval { require Digest::MD5; 1 } ? 0 : 1) . '}';
}

$ENV{PERL} ||= PERL;
#delete $ENV{'MAKEPPFLAGS'};     # These environment variables can possibly
#delete $ENV{'MAKEFLAGS'};       # mess up makepp tests.
# For some reason, with perl 5.8.4, deleting the environment variables doesn't
# actually remove them from the environment.
$ENV{"${_}FLAGS"} = ''
  for qw(MAKEPP MAKE MAKEPPBUILTIN MAKEPPCLEAN MAKEPPLOG MAKEPPGRAPH);


for( $ENV{PATH} ) {
  my $sep = is_windows > 0 ? ';' : ':';
  s/^\.?$sep+//;			# Make sure we don't rely on current dir in PATH.
  s/$sep+\.?$//;
  s/$sep+\.?$sep+/$sep/;
  $_ = "$source_path$sep$_";
}


#
# Equivalent of system() except that it handles INT signals correctly.
#
# If the first argument is a reference to a string, that is the command to report as failing, if it did fail.
#
sub system_intabort {
  my $cmd = ref( $_[0] ) && shift;
  system @_;
  kill 'INT', $$ if $sigint && $? == $sigint;
  if( $? && $cmd ) {
    if( $? == -1 ) {
      die "failed to execute $$cmd: $!\n"
    } elsif( $? & 127 ) {
      die sprintf "$$cmd died with signal %d%s coredump\n",
	($? & 127),  ($? & 128) ? ' and' : ', no';
    } else {
      die sprintf "$$cmd exited with value %d\n", $? >> 8;
    }
  }
  return $?;
}

sub makepp(@) {
  my $suffix = '';
  $suffix = ${shift()} if ref $_[0];
  print "makepp$suffix @_\n";
  system_intabort \"makepp$suffix", PERL, -f 'makeppextra.pm' ? '-Mmakeppextra' : (), $makepp_path.$suffix, @_;
  1;				# Command succeeded.
}

@ARGV or @ARGV = <*.test *.tar *.tar.gz>;
				# Get a list of arguments.

$n_failures = 0;

(my $wts = $0) =~ s/run_tests/wait_timestamp/;
do $wts;			# Preload the function.
eval { require Time::HiRes };	# Preload the library.

# spar <http://www.cpan.org/scripts/> extraction function
# assumes DATA to be opened to the spar
sub un_spar() {
    my( $lines, $kind, $mode, %mode, $atime, $mtime, $name, $nl ) = (-1, 0);
    while( <DATA> ) {
	s/\r?\n$//;		# cross-plattform chomp
	if( $lines >= 0 ) {
	    print F $_, $lines ? "\n" : $nl;
	} elsif( $kind eq 'L' ) {
	    if( $mode eq 'S' ) {
		symlink $_, $name;
	    } else {
		link $_, $name;
	    }
	    $kind = 0;
	} elsif( /^###\t(?!SPAR)/ ) {
	    (undef, $kind, $mode, $atime, $mtime, $name) = split /\t/, $_, 6;
	    if( !$name ) {
	    } elsif( $kind eq 'D' ) {
		$name =~ s!/+$!!;
		-d $name or mkdir $name, 0700 or warn "spar: can't mkdir `$name': $!\n";
		$mode{$name} = [$atime, $mtime, oct $mode];
	    } elsif( $kind ne 'L' ) {
		open F, ">$name" or warn "spar: can't open >`$name': $!\n";
		$lines = abs $kind;
		$nl = ($kind < 0) ? '' : "\n";
	    }
	} elsif( defined $mode ) {
	    warn "spar: $archive:$.: trailing garbage ignored\n";
	}			# else before beginning of spar
    } continue {
	if( !$lines-- ) {
	    close F;
	    chmod oct( $mode ), $name and
		utime $atime, $mtime, $name or
		warn "spar: $archive:$name: Failed to set file attributes: $!\n";
	}
    }

    for( keys %mode ) {
	chmod pop @{$mode{$_}}, $_ and
	    utime @{$mode{$_}}, $_ or
	    warn "spar: $archive:$_: Failed to set directory attributes: $!\n";
    }
}


# With -d report '.' for success, 's' for skipped because of symlink failure,
# 'w' for not applicable on windows, '-' for otherwise skipped.
sub dot($$) {
  if( defined $_[0] ) {
    if( $test ) {
      for( "$_[1]" ) {
	s/^passed // || s/^skipped/# skip/;
	print "ok $test $_";
      }
      $test++;
    } else {
      print $_[$dot ? 0 : 1];
      $dotted = 1 if $dot;
    }
  } elsif( $test ) {
    print "not ok $test $_[1]";
    $test++;
  } else {
    print "\n" if defined $dotted;
    print "FAILED $_[1]";
    undef $dotted;
  }
}


sub execute($$;$) {
  open STDOUT, ">$_[1]" or die "$_[1]: $!";
  open STDERR, '>&STDOUT' or die "$!";
  my $ret =
    -r( "$_[0].pl" ) ? !do "$_[0].pl" :
    -x( $_[0] ) ? system_intabort "./$_[0]", $makepp_path :
    $_[2] ? system_intabort PERL, $makepp_path :
    0;
  warn $@ if $@;		# This goes to log file
  open STDOUT, '>&OSTDOUT' or die "$!";
  open STDERR, '>&OSTDERR' or die "$!";
  $ret || $@;
}

# on some (Windowsish) filesystems rmtree may temporarily fail
sub slow_rmtree(@) {
  for my $tree ( grep -d, @_ ) {
    eval { rmtree $tree } && last
      or $_ < 9 && select undef, undef, undef, .1
      for 0..9;
    die $@ if $@;
  }
}

sub n_files(;$$) {
  my( $outf, $code ) = @_;
  open my $logfh, '.makepp/log' or die ".makepp/log--$!\n";
  seek $logfh, -20, 2 if !$code; # More than enough to find last message.
  open my $outfh, '>', $outf if $outf;
  while( <$logfh> ) {
    &$code if $code;
    if( /^[\02\03]?N_FILES\01(\d+)\01(\d+)\01(\d+)\01$/ ) {
      if( $outf ) {
	print $outfh "$1 $2 $3\n";
      } else {
	return "$1 $2 $3\n";
      }
    }
  }
  close $logfh;			# Might happen too late for Windows.
}

my $have_shell = -x '/bin/sh';

print OSTDOUT '1..'.@ARGV."\n" if $test;
test_loop:
foreach $archive (@ARGV) {
  my $testname = $archive;
  my( $tarcmd, $dirtest, $warned );
  $SIG{__WARN__} = sub {
    warn defined $dotted ? "\n" : '',
      $warned ? '' : "$testname: warning: ",
      $_[0];
    undef $dotted if -t STDERR;	# -t workaround for MSWin
    $warned = 1;
  };
  if( -d $archive ) {
    chdir $archive;
    $dirtest = 1;
  } else {
    $testname =~ s/\..*$//; # Test name is tar file name w/o extension.
    if( is_windows && $testname =~ /_unix/ ) {
				# Skip things that will cause errors on cygwin.
				# E.g., the test for file names with special
				# characters doesn't work under NT!
      dot w => "skipped $testname on Windows\n";
      next;
    }
    if( no_symlink && $testname =~ /repository/ ) {
      dot s => "skipped $testname because symbolic links do not work\n";
      next;
    }
    if( (no_link || no_md5) && $testname =~ /build_cache/ ) {
      if( no_link ) {
	dot l => "skipped $testname because links do not work\n";
      } else {
	dot m => "skipped $testname because MD5 is not available\n";
      }
      next;
    }
    if( no_md5 && $testname =~ /md5/ ) {
      dot m => "skipped $testname because MD5 is not available\n";
      next;
    }
    if ($archive !~ /^\//) {	# Not an absolute path to tar file?
      $archive = "$old_cwd/$archive"; # Make it absolute, because we're going
    }				# to cd below.

    if ($testname =~ /\.gz$/) { # Compressed tar file?
      $tarcmd = "gzip -dc $archive | tar xf -";
    }
    elsif ($testname =~ /\.bz2$/) { # Tar file compressed harder?
      $tarcmd = "bzip2 -dc $archive | tar xf -";
    }
    slow_rmtree 'tdir', "$testname.failed";
    mkdir "tdir", 0755 or die "$0: can't make directory tdir--$!\n";
				# Make a directory.
    chdir "tdir" or die "$0: can't cd into tdir--$!\n";
  }

  my $log = "$testname.log";
  $log="../$log" if $log !~ /^\//;
  eval {
    local $SIG{ALRM} = sub { die "timed out\n" };
    eval { alarm( $ENV{MAKEPP_TEST_TIMEOUT} || 600 ) }; # Dies in Win Active State 5.6
    if( $tarcmd ) {
      system_intabort $tarcmd and # Extract the tar file.
	die "$0: can't extract testfile $archive\n";
    } elsif( !$dirtest ) {
      open DATA, $archive or die "$0: can't open $archive--$!\n";
      eval { local $SIG{__WARN__} = sub { die @_ if $_[0] !~ /Failed to set/ }; un_spar };
				# Alas happens a lot on native Windows.
      die +(is_windows && $@ =~ /symlink .* unimplemented/) ? "skipped s\n" :
	$@ =~ /: can't open >`/ ? "skipped\n" : $@
      	if $@;
    }
    open STDOUT, '>', $log or die "write $log: $!";
    open STDERR, '>&STDOUT' or die $!;
    open my $fh, '>>.makepprc';	# Don't let tests be confused by a user's file further up.
    close $fh;
    eval {
      unless( $have_shell ) {
	die "skipped x\n" if
	  -f 'is_relevant' or
	  -f 'makepp_test_script' or
	  -f 'cleanup_script';
      }
      -r( 'is_relevant.pl' ) ? do 'is_relevant.pl' :
	-x( 'is_relevant' ) ? !system_intabort './is_relevant', $makepp_path :
	1
	or die "skipped r\n";
      if( -r 'makepp_test_script.pl' ) {
	do 'makepp_test_script.pl' or
	  die 'makepp_test_script.pl ' . ($@ ? "died: $@" : "returned false\n");
      } elsif( -x 'makepp_test_script' ) {
	system_intabort \'makepp_test_script', './makepp_test_script', $makepp_path;
      } else {
	makepp;
      }
    };
    open STDOUT, '>&OSTDOUT' or die $!;
    open STDERR, '>&OSTDERR' or die $!;
    die $@ if $@;

#
# Now look at all the final targets:
#
    my @errors;
    use File::Find;
    find sub {
	return if $_ eq 'n_files'; # Skip the special file.
	return if -d;		   # Skip subdirectories, find recurses.
	local $/ = undef;		# Slurp in the whole file at once.
	open TFILE, $_ or die "$0: can't open tdir/$File::Find::name--$!\n";
	$tfile_contents = <TFILE>; # Read in the whole thing.
	$tfile_contents =~ s/\r//g; # For cygwin, strip out the extra CRs.

	# Get the name of the actual file, older find can't do no_chdir.
	($mtfile = $File::Find::name) =~ s!answers/!!;
	open MTFILE, "$old_cwd/" . ($dirtest ? $archive : 'tdir') . "/$mtfile"
	  or die "$mtfile\n";
	my $mtfile_contents = <MTFILE>; # Read in the whole file.
	$mtfile_contents =~ s/\r//g; # For cygwin, strip out the extra CRs.
	$mtfile_contents eq $tfile_contents or push @errors, $mtfile;
    }, 'answers' if -d 'answers';
    close TFILE;
    close MTFILE;

#
# See if the correct number of files were built:
#
    my $n_files_updated = n_files;
    rename '.makepp/log' => '.makepp/log.failed';
				# Get rid of the log file so we don't
				# get confused if the next test doesn't make
				# a log file for some reason.
    defined($n_files_updated) or push @errors, '.makepp/log';

    if (open my $n_files, 'answers/n_files' ) { # Count of # of files updated?
      $_ = <$n_files>;
      $_ eq $n_files_updated or push @errors, 'n_files';
    }

#
# Also search through the log file to make sure there are no perl messages
# like "uninitialized value" or something like that.
#
    if( open my $logfile, $log ) {
      while( <$logfile> ) {
	# Have to control a few warnings before we can unleash this:
	#/makepp: warning/
	if( /at (\S+) line \d+/ && $1 !~ /[Mm]akep*file$|\.mk$/ || /(?:internal|generated) error/ ) {
	  push @errors, $log;
	  last;
	}
      }
    }
    eval { alarm 0 };
    die 'wrong file' . (@errors > 1 ? 's' : '') . ': ' . join( ', ', @errors) . "\n" if @errors;
  };

  if ($@) {
    if ($@ =~ /skipped(?: (.))?/) {	# Skip this test?
      chop( my $loc = $@ );
      dot $1 || '-', "$loc $testname\n";
      if( !$dirtest ) {
	$@ = '' if ::is_perl_5_6;
	execute 'cleanup_script', ">$log";
	slow_rmtree 'tdir';		# Get rid of the test directory.
      }
      chdir $old_cwd;		# Get back to the old directory.
      next;
    } elsif ($@ =~ /^\S+$/) {	# Just one word?
      my $loc = $@;
      $loc =~ s/\n//;		# Strip off the trailing newline.
      dot undef, "$testname (at $loc)\n";
    } else {
      dot undef, "$testname: $@";
    }
    ++$n_failures;
    close TFILE; close MTFILE;	# or cygwin will hang
    chdir $old_cwd;		# Get back to the old directory.
    rename tdir => "$testname.failed";
    last if $testname eq 'aaasimple'; # If this one fails something is very wrong
  } else {
    dot '.', "passed $testname\n";
    if( !$dirtest ) {
      execute 'cleanup_script', ">$log";
      chdir $old_cwd;		# Get back to the old directory.
      slow_rmtree 'tdir';		# Get rid of the test directory.
    } else {
      chdir $old_cwd;		# Get back to the old directory.
    }
  }
}
print "\n" if defined $dotted;

printf "%ds real  %.2fs user  %.2fs system  children: %.2fs user  %.2fs system\n", time - $^T, times
  if $verbose;
close OSTDOUT;			# shutup warnings.
close OSTDERR;
exit $n_failures;


=head1 NAME

run_tests.pl -- Run makepp regression tests

=head1 SYNOPSYS

  run_tests.pl [-d] [-v] test1.test test2.test

If no arguments are specified, defaults to *.test.

=head1 DESCRIPTION

This script runs the specified tests and reports their result.  With the -d
option it only prints a dot for each successful test.  A test that is skipped
for a standard reason outputs a letter instead of a dot.  The letters are B<l>
or B<m> for build cache tests that were skipped because links don't work or
MD5 is not available, B<s> for a repository test skipped because symbolic
links don't work or B<w> for a Unix test skipped because you are on Windows.
An B<x> means the test can't be executed because that would require a Shell.
If the test declares itself to not be relevant, that gives an B<r>.
Other reasons may be output as B<->.

With the -v option it also gives info about the used Perl version and system,
handy when parallely running this on many setups, and the used time for the
runner (and perl scripts it runs directly) on the one hand and for the makepp
(and shell) child processes on the other hand.

A test is stored as a file with an extension of F<.test> (very economic and --
with some care -- editable spar format), or F<.tar>, F<.tar.bz2> or
F<.tar.gz>.

First a directory is created called F<tdir> (called the test directory
below).	 Then we cd to the directory, then extract the contents of the
tar file.  This means that the tar file ought to contain top-level
files, i.e., it should contain F<./Makeppfile>, not F<tdir/Makeppfile>.

A test may also be the name of an existing directory.  In that case, no
archive is unpacked and no cleanup is performed after the test.

The following files within this directory are important:

=over 4

=item is_relevant.pl / is_relevant

If this file exists, it should be a perl script which return true (1) or shell
script which returns true (0) if this test is relevant on this platform, and
dies or false if the test is not relevant.

The first argument to this script is the full path of the makepp executable we
are testing.  The second argument is the current platform as seen by Perl.
The environment variable C<PERL> is the path to the perl executable we are
supposed to use (which is not necessarily the one in the path).

=item makepp_test_script.pl / makepp_test_script

If this file exists, it should be a perl script or shell script which runs
makepp after setting up whatever is necessary.  If this script returns false
(a non-zero value), then the test fails.

In a Perl script you can use the predefined function makepp() to run it with
the correct path and wanted interpreter.  It will die if makepp fails.  You
can also use the function wait_timestamp( file ... ), which will wait for both
the local clock and the timestamp of newly created files to be at least a
second later than the newest given file.  You also have the function n_files,
the first optional argument being a file name, where to write the count of
built files, the second a sub that gets called for each log line so you can
scan for messages.  File::Copy's cp is also provided.

The first argument to this shell script is the full path of the makepp
executable we are testing.  The environment variable C<PERL> is the path
to the perl executable we are supposed to use (which is not necessarily
the one in the path).

This script must be sufficiently generic to work in all test
environments.  For example:

=over 4

=item *

It must not assume that perl is in the path.  Always use $PERL instead.

=item *

It must work with the Bourne shell, i.e., it may contain no bash
extensions.

=item *

It must not use "echo -n" because that doesn't work on HP machines.  But you
should use &echo and other builtins for efficiency anyway.

=back

If this file does not exist, then we simply execute the command
S<C<$PERL makepp>>, so makepp builds all the default targets in the makefile.

=item makeppextra.pm

If present this module is loaded into perl before the script by the makepp
function.  See F<additional_tests/2003_11_14_timestamp_md5.test> for an
example of output redirection.

=item F<Makefile> or F<Makeppfile>

Obviously this is kind of important.

=item answers

This directory says what the result should be after running the test.
Each file in the answers directory, or any of its subdirectories, is
compared to a file of the same name in the test directory (or its
corresponding subdirectory).  The files must be exactly identical or the
test fails.

Files in the main test directory do not have to exist in the F<answers>
subdirectory; if not, their contents are not compared.

There is one special file in the F<answers> subdirectory: the file
F<answers/n_files> should contain two integers in ASCII format which are the
number of files that makepp ought to build and that are expected to have
failed.  This is compared to the corresponding number of files that it
actually built, extracted from the logfile F<.makepp/log>.

=item cleanup_script.pl / cleanup_script

If this file exists, it should be a perl script or shell script that is
executed when the test is done.  This script is executed just before the test
directory is deleted.  No cleanup script is necessary if the test directory
and all the byproducts of the test can be deleted with just S<C<rm -rf tdir>>.
(This is usually the case, so most tests don't include a cleanup script.)

=back

=cut
