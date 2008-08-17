# This "module" is a haphazard assortment of stuff needed by various utils.
require 5.006;
use strict;

# $Id: Utils.pm,v 1.7 2008/05/17 14:08:38 pfeiffer Exp $

# This is syntactically needed by many modules but not called in utils.
sub log;

($::progname = $0) =~ s@.*/@@; # Get the program name w/o directories.
my %progname =
 (b => 'builtin',
  bcc => '_build_cache_control',
  c => 'clean',
  g => 'graph',
  i => 'info',
  l => 'log');
$::progname =~ s/^mpp([bcgil]|bcc)$/makepp$progname{$1}/;

use TextSubs ();

# Replace a function which depends on Makefile context to rewrite messages in
# a way we don't need here.  Hope that didn't get inlined.  Maybe we should
# build a switch into Makecmds::eval_or_die.
sub maybe_die() {
  if( $@ ) {
    (my $msg = $@) =~ s!\(eval \d+\)!expression!g;
    chomp $msg;
    die "$msg\n";
  }
}
# Can't put eval_or_die here, or variables end up in the wrong scope :-(

sub find_logfiles(\@) {
  @{$_[0]} = '.' if !@{$_[0]};
  for( @{$_[0]} ) {
    next if $_ eq '-' || -f;
    $_ .= '/.makepp/log';
    s!/.makepp/!/! if !-r;
    die "$::progname: can't read `$_'--$!\n" if !-r;
  }
}

my( $max_up, $cwd_re );
# Strip cwd from front, and optionally any level of parent directories.
sub Rewrite::cwd(;$$$) {
  if( defined and $_ ne '' ) {
    my( $up, $name, $sep ) = @_;
    if( !$cwd_re ) {
      my @path = split /(?=\/)/, FileInfo::absolute_filename( FileInfo::dereference( $FileInfo::CWD_INFO ));
      if( @path > 1 && $path[0] eq '/' ) { # //server/share
	shift @path;
	substr $path[0], 0, 0, '/';
      }
      $max_up = @path - 1;
      $cwd_re = join( '(?:', map quotemeta, @path ) . ('())?' x $max_up);
    }
    s!(.)/$!$1!;		# Other rewrite might have left trailing '/'.
    if( my @match = /^($cwd_re(\/|$))/o ) {
      $up = !$up ? 0 : $up > $max_up ? $max_up : $up;
				# undef -> 0, else max( $max_up : $up )
      my $up_found = $max_up + 2 - grep defined, @match;
      if( $up_found <= $up ) {
	$name = '..' if !defined $name;
	$sep = '/' if !defined $sep;
	substr $_, 0, length( $match[0] ),
	  (join( $sep, ($name) x $up_found, $match[-1] ? '' : () ) || ($match[-1] ? '' : '.'));
	1;			# Return true
      }
    }
  }
}

my $flags = "\U$::progname\EFLAGS";
unshift @ARGV, TextSubs::unquote_split_on_whitespace $ENV{$flags}
  if exists $ENV{$flags};

1;
