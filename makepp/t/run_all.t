#! /usr/bin/perl -w

chdir 't';			# failure ignored, when called from here

my %c;				# CC=gehtnich CXX=gehtnich ./run_all.pl
@c{qw(
c_compilation.test
log_graph.test
md5.test
additional_tests/2003_10_11_idash.test
additional_tests/2003_11_25_wild.test
additional_tests/2004_02_19_repository_change.test
additional_tests/2004_03_12_condscan.test
additional_tests/2004_03_24_scanner_c_lib.test
additional_tests/2004_11_02_repository_rmstale.test
additional_tests/2004_12_06_scancache.test
additional_tests/2004_12_17_idl.test
additional_tests/2005_03_31_scanfail.test
additional_tests/2005_07_12_build_cache_cp.test
additional_tests/2006_12_07_scan_order.test
)} = ();

BEGIN {
  if( $^O =~ /^MSWin/ ) {
    require Win32API::File;
    Win32API::File::SetErrorMode( &Win32API::File::SEM_FAILCRITICALERRORS | &Win32API::File::SEM_NOOPENFILEERRORBOX );
  }
}

use Config;

my $bits = $Config{ptrsize} == 8 ? '-64bit' : '';

$0 =~ s!.*/!!;
my $makepp = @ARGV && $ARGV[0] =~/\bm(?:ake)?pp$/ && shift;
if( @ARGV && $ARGV[0] eq '-?' ) { print <<EOF; exit }
$0\[ path/to/makepp][ options in following order][ tests]
    -T  run_tests.pl -dv rather than default -t
    -n<name>  Give this run a name which becomes part of the result dir.
    -b  Add all build_cache tests to list.
    -c  Select only those which use the C compiler.
    -C  Select none of those which use the C compiler.
    -R  Add all repository tests to list.
    -S  None of the stress_tests.

    If no path to makepp is given, looks for it one directory higher.
    If no tests are given, runs all in and below the current directory.
EOF
$0 =~ s!all\.t!tests.pl!;
my $T = @ARGV && $ARGV[0] eq '-T' and shift;
my $n = @ARGV && $ARGV[0] =~ s/^-n// && shift;
my $b = @ARGV && $ARGV[0] eq '-b' and shift;
my $c = @ARGV && $ARGV[0] eq '-c' and shift;
my $C = @ARGV && $ARGV[0] eq '-C' and shift;
my $R = @ARGV && $ARGV[0] eq '-R' and shift;
my $S = @ARGV && $ARGV[0] eq '-S' and shift;

push @ARGV, <*build_cache*.test */*build_cache*.test> if $b;
push @ARGV, <*repository*.test */*repository*.test> if $R;

@ARGV = @ARGV ?
  map { /\.test$/ ? $_ : "$_.test" } @ARGV :
  <*.test */*.test>;
@ARGV = grep exists $c{$_}, @ARGV if $c;
@ARGV = grep !exists $c{$_}, @ARGV if $C;
@ARGV = grep !/stress_tests/, @ARGV if $S;

unshift @ARGV, $T ? '-dv' : '-t';
unshift @ARGV, $makepp if $makepp;
print "$0 @ARGV\n" if $ENV{DEBUG};

print "$n " if $n && $T;

if( $^O =~ /^MSWin/ ) {
  system $^X, $0, @ARGV;
} elsif( !fork ) {
  do $0;
  die "run_tests didn't exit--$@\n" if $@ and $@ !~ /Invalid argument at run_tests.pl line 169/;
  exit 1;
}

my $v = sprintf $Config{ptrsize} == 4 ? 'V%vd' : 'V%vd-%dbits', $^V, $Config{ptrsize} * 8;
$v .= "-$n" if $n;

wait;
my $ret = $? ? 1 : 0;

sub mail {
  my $a = 'occitan@esperanto.org';
  if( open MAIL, "| exec 2>/dev/null; mailx -s$_[0] $a || mail -s$_[0] $a || /usr/lib/sendmail $a || mail $a" ) {
    print MAIL "$_[0]\n";
    my %acc;
    $Config{$_} && push @{$acc{$Config{$_}}}, $_ for sort keys %Config;
    print MAIL "@{$acc{$_}} => $_\n" for sort keys %acc;
    1;
  }
}
if( $T ) {
  if( -d $v ) {
    require File::Path;
    eval { File::Path::rmtree $v } && last
      or $_ < 9 && select undef, undef, undef, .1
	for 0..9;
    die $@ if $@;
  }
  mkdir $v or warn $!;

  for( map { s/\.test$// ? grep -e, "$_.log", "$_.failed" : () } @ARGV ) {
    rename "$1/$_", $_ if s!^(.*/)!!; # Some systems don't mv from one dir to another
    rename $_, "$v/$_";
  }
  rename 'tdir', "$v/tdir" if -d 'tdir';
  exit $ret;
} elsif( $ENV{AUTOMATED_TESTING} ) { # CPAN testers don't send success or error details
  my @failed = <*.failed */*.failed>;
  push @failed, map substr( $_, 0, -6 ) . 'log', @failed;
  if( -d 'tdir' ) {
    push @failed, 'tdir';
    my @logs = <*.log */*.log>;
    push @failed, $logs[-1] if @logs;
  }
  ($v = "$Config{myarchname}-$v") =~ tr/ ;&|\\'"()[]*\//-/d;
  if( !@failed ) {
    mail "SUCCESS-$v";
  } elsif( mail "FAIL-$v" ) {
    print MAIL "<@failed>";
    open SPAR, '-|', $^X, 'spar', '-d', '-', @failed;
    undef $/;
    print MAIL "\nbegin 755 $v.spar\n" . pack( 'u*', <SPAR> ) . "\nend\n";
  }
}
