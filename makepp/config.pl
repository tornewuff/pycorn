#!/usr/bin/perl -w
# $Id: config.pl,v 1.28 2010/10/18 21:40:21 pfeiffer Exp $
#
# Configure this package.
#

package Mpp;

use strict;
use Mpp::Text ();
use Mpp::File ();		# ensure HOME is set

#
# First make sure this version of perl is recent enough:
#
eval { require 5.006 };
if ($@) {			# Not recent enough?
  die "I need perl version 5.6 or newer.  If you have it installed somewhere
already, run this installation procedure with that perl binary, e.g.,

	perl5.10.0 config.pl

If you don't have a recent version of perl installed (what kind of system are
you on?), get the latest from www.perl.com and install it.
";
}

if ($] == 5.006) {
  print "**************** You're running perl 5.6.0.  *************************

perl 5.6.0 has a bug which can cause makepp to behave in a bizarre fashion.
This bug is not encountered in every makefile, so you might be ok.  But it
would be safer to upgrade to another version of perl (e.g., 5.8.0 does
not seem to have the problem).\n";
} elsif ($] == 5.006001) {
  print "**************** You're running perl 5.6.1.  *************************

perl 5.6.1 fails on some architectures and works just fine on others.
If you encounter weird problems with makepp, or if the tests fail,
consider upgrading your version of perl.\n";
}

#
# Parse the arguments:
#
my $prefix = "/usr/local";
my $findbin = "none";
my $makefile = '.';
my( $bindir, $datadir, $mandir, $htmldir );

Mpp::Text::getopts
  [qw(p prefix), \$prefix, 1],
  [qw(b bindir), \$bindir, 1],
  [qw(h htmldir), \$htmldir, 1],
  [qw(m mandir), \$mandir, 1],
  [qw(d datadir), \$datadir, 1],
  [qw(f findbin), \$findbin, 1],
  [undef, 'makefile', \$makefile, 1],
  splice @Mpp::Text::common_opts;


$makefile .= '/Makefile' if -d $makefile;
$bindir ||= "$prefix/bin";
$datadir ||= "$prefix/share/makepp";
$htmldir ||= "$prefix/share/makepp/html";
$mandir ||= "$prefix/man";

foreach ($bindir, $datadir, $htmldir) {
  s@~/@$ENV{HOME}/@;
}

our $VERSION;

#
# Write out a makefile for this.  This makefile ought to work with any version
# of bozo make, so it has to be extremely generic.
#
open MAKEFILE, '>', $makefile or die "$0: can't write $makefile--$!\n";
my $perl = PERL;

s/\$/\$\$/g for $perl, $bindir, $datadir, $findbin, $mandir, $htmldir, $VERSION;

print MAKEFILE "PERL = $perl
BINDIR = $bindir
DATADIR = $datadir
FINDBIN = $findbin
MANDIR = $mandir
HTMLDIR = $htmldir
VERSION = makepp-$VERSION\n";

print MAKEFILE q[

all: test

test: .test_done

.test_done: *.pm Mpp/*.pm Mpp/Signature/*.pm Mpp/Scanner/*.pm Mpp/CommandParser/*.pm Mpp/Lexer/*.pm makepp \
	t/*.test t/run_tests.pl
	cd t && PERL=$(PERL) $(PERL) run_tests.pl --hint
	touch $@

testall: .testall_done

.testall_done: *.pm Mpp/*.pm Mpp/Signature/*.pm Mpp/Scanner/*.pm Mpp/CommandParser/*.pm Mpp/Lexer/*.pm makepp \
	t/*.test t/*/*.test t/run_tests.pl
	cd t && PERL=$(PERL) $(PERL) run_tests.pl --hint *.test */*.test
	touch $@

distribution: $(VERSION).tar.gz

$(VERSION).tar.gz: README INSTALL LICENSE VERSION makepp.lsm ChangeLog \
	makepp recursive_makepp makeppclean \
	Mpp/*.pm Mpp/Signature/*.pm Mpp/Scanner/*.pm \
	Mpp/BuildCheck/*.pm Mpp/CommandParser/*.pm Mpp/Lexer/*.pm *.mk *.pm \
	pod/*.pod \
	t/*.test t/*/*.test t/run_tests.pl \
	config.pl configure install.pl makepp_build_cache_control
	rm -rf $(VERSION)
	./configure         # Reset Makefile.
	mkdir $(VERSION) \
	   $(VERSION)/pod $(VERSION)/t \
	   $(VERSION)/Mpp $(VERSION)/Mpp/Signature $(VERSION)/Mpp/Scanner \
	   $(VERSION)/Mpp/CommandParser $(VERSION)/Mpp/Lexer
	for file in $^; do cp $$file $(VERSION)/$$file; done
	GZIP=-9 tar --create --gzip --file $@ $(VERSION)
	cd $(VERSION) && make test    # Make sure it all runs.
	rm -rf $(VERSION)

install: all
	$(PERL) install.pl $(BINDIR) $(DATADIR) $(MANDIR) $(HTMLDIR) $(FINDBIN) $(DESTDIR)

.PHONY: all distribution install test testall
];

__DATA__
Usage: configure [option]

Valid options are:

-b, --bindir=/path/to/installation/bin
    Where the binaries go.  Makepp's binaries are just perl scripts so they
    are architecture independent.
-d, --datadir=/path/to/installation/share/makepp
    Where to install makepp's library files.
-f, --findbin=relative/path/to/datadir/from/bindir
    Where to find libraries relative to executables. Specify 'none' (the
    default) to find them in datadir.
-h, --htmldir=/path/to/installation/share/html
    Where the HTML documentation goes.  Specify 'none' if you do not want the
    documentation installed.  (You can always read it online at
    http://makepp.sourceforge.net.)
-m, --mandir=/path/to/man
    Where the manual pages should reside.  Specify 'none' if you do not want
    the documentation installed.
--makefile=/path/to/Makefile (default: .)
    Specify location where you can write the Makefile.
-p, --prefix=/path/to/installation
    Specify location where you want to install everything.
-?, --help
    This help message.
