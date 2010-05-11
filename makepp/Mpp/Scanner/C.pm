# $Id: C.pm,v 1.25 2009/02/11 23:22:37 pfeiffer Exp $

=head1 NAME

Mpp::Scanner::C - makepp scanner for C-like files

=head1 DESCRIPTION

Scans a C file for C<#include>'s.

Tags are:

=over 6

=item user

File scanned due to an #include "filename" directive.

=item sys

File scanned due to an #include E<lt>filenameE<gt> directive.

=back

=cut

use strict;
package Mpp::Scanner::C;

use Mpp::Scanner;
our @ISA = qw/Mpp::Scanner/;

use Mpp::File;
use Mpp::Text ();

# The base class uses a scalar to describe a scope; we use arrayrefs.
# These are the indices into the arrayref that describes a scope:
BEGIN {
  *ACT = \&Mpp::Text::CONST0;	# Are we outside of a disabled '#if' block
  *PREV_ACT = \&Mpp::Text::CONST1; # Has the current or a previous block of the
				# current '#if'...'#elif' chain been active?
  *EXPR = \&Mpp::Text::CONST2;	# The expression that caused the state to be
				# unknown, if any.
  *UNKNOWN = \&Mpp::Text::CONST3; #any number except 0 or 1
}

our $dont_scan_hook;

sub dont_scan {
  #my ($self, $finfo, $absname) = @_;
  my( $guard, $dont_scan ) =
    Mpp::File::build_info_string $_[1], qw(GUARD DONT_SCAN);
  return 1 if defined $guard && exists $_[0]{VARS}{$guard};
  if( $dont_scan ) {
    $_[0]{VARS}{$guard} = '' if defined $guard; # This is the only thing that would happen if we did scan.
    return 1;
  }
  if( $dont_scan_hook && &$dont_scan_hook( $_[2] )) {
    Mpp::log SCAN_C_NOT => $_[1]
      if $Mpp::log_level;
    return 1;
  }
  &Mpp::Scanner::dont_scan;
}

sub push_scope {
  die "@{$_[1]}" unless $_[1][ACT]==0 || $_[1][PREV_ACT]==1 || $_[1][ACT]==$_[1][PREV_ACT];
  my $act = $_[0]{ACTIVE};
  if($act == 1) {
    $act = $_[1][ACT];
  } elsif($_[1][ACT] == 0) {
    $act = 0;
  }
  $_[0]{ACTIVE} = $_[1][ACT] = $act;
  $_[1][EXPR] ||= $_[0]{SCOPES}[-1][EXPR] if $act == UNKNOWN;
  die "@{$_[1]}" if ($_[1][ACT] == UNKNOWN || $_[1][PREV_ACT] == UNKNOWN) && !defined($_[1][EXPR]);
  push(@{$_[0]{SCOPES}}, $_[1]);
}

sub pop_scope {
  my $self=$_[0];
  my $result = $self->SUPER::pop_scope();
  $self->{ACTIVE}=$self->{SCOPES}[-1][ACT];
  $result;
}

sub reset {
  my $self= shift;
  $self->SUPER::reset(@_);
  unless(@_ > 0) {
    $self->{SCOPES}=[[1, 0]];
  }
  $self->{ACTIVE}=$self->{SCOPES}[-1][ACT];
}

sub new {
  my $class = shift;
  my $self = $class->SUPER::new(@_);
  $self->reset;
  $self;
}

sub get_bad_expr {
  $_[0]{SCOPES}[-1][EXPR];
}

#update scanner state and warning message
sub update_scope {
  my ($self, $else, $line, $expr)=@_;
  my @state = @{$self->{SCOPES}[-1]};
  if($else) { # Continue the current chain
    @state = @{$self->pop_scope};
    $state[ACT] = $state[PREV_ACT];
    $state[ACT] = !$state[ACT] unless $state[ACT] == UNKNOWN;
  } else { # Start a new chain
    die unless defined($expr);
    $state[PREV_ACT] = 0;
  }
  @state = (0,0) unless $self->{ACTIVE}; # The scope enclosing the chain is off
  if( $state[ACT] ) {
    my $go = defined($expr) ?
      &eval_condition($self->expand_defines($expr)) : 1; # '#else' == '#elif 1'
    $state[ACT] = $go unless $go==1 && $state[PREV_ACT];
    $state[EXPR] = $line."\"$expr\"" if $go == UNKNOWN;
    die if $state[PREV_ACT] == 1;
    $state[PREV_ACT] = $go unless $go == 0;
  }
  $self->push_scope(\@state);
}

sub get_macro {
  if(/\G(\`*)([a-z_]\w*)/igc) {
    return ($1, "", $2);
  }
  return;
}

sub expand_defines {
  my ($self,$expr,$vis) = @_;

  $expr =~ s/defined\s*(\(?)\s*(\w+)/$1 . (defined( $self->get_var( $2 )) ? 1 : 0)/eg;
  return $self->expand_macros($expr,$vis);
}

#since we already expanded defined and macros we should expect
#arithmetic expression on input and number on output.
#Any remaining words are undefined macros and should be eval to 0.
sub eval_condition {
  my $cond = $_[0];
  $cond =~ tr/`//d;		# TODO: What's this good for?
  $cond =~ s/\b([_a-z]\w*)/ 0 /ig;
  my $funny;
  local $SIG{__WARN__} = sub { $funny = 1 };
  $cond = eval $cond;
  return UNKNOWN if $@ || $funny;
  $cond ? 1 : 0;
}

sub expand_macros {
  my ($self, $expr, $visited) = @_;
  $visited ||= {};
  local $_ = $expr;
  $expr = '';
  pos($_) = 0;

  # TBD: Deal with macros that have parameters.

  while(1) {
    last if length($_) <= pos($_);
    if(/\G(\\.)/sgc) {
      $expr .= $1;
    }
    elsif(/\G(\"(?:[^"]*\\\")*[^"]*\")/gc) {
      $expr .= $1;
    }
    elsif(/\G'((\\)(?:\d+|.)|.)'/gc) {
      $expr .= ord( $2 ? eval "\"$1\"" : $1 );
    }
    elsif(/\G(\d+(?:\.\d+)?)[luf]?/gic) {
      $expr .= $1;
    }
    elsif(my ($prefix, $key_prefix, $key) = $self->get_macro) {
      $expr .= $prefix;
      my $x=$self->get_var($key) unless $visited->{$key};
      if(defined $x) {
	my %v=%$visited;
	$v{$key}=1;
				# NOTE: pos() isn't preserved by local:
	my $pos = pos($_);
	$expr .= $self->expand_defines($x, \%v);
	pos($_) = $pos;
      }
      else {
	$expr .= $key_prefix . $key;
      }
    }
    else {
      /\G([^\w\\"`']+)/igc or die "$_\n";
      $expr .= $1;
    }
  }
  $expr;
}

sub get_directive {
  if(s/^\s*\#\s*(\w+)\s*//) {
    return $1;
  }
  return;
}

#
# Override this in subclasses for picking up additional directives.
# Return 0 if not interested, or undef to abort.
#
*other_directive = \&Mpp::Text::CONST0;

sub xscan_file {
  my ($self, $cp, undef, $finfo, $conditional, $fh)=@_;
  my $absname = absolute_filename( $finfo );
  my ($go, $pending_comment, $continued_comment);
  my $guard_scope;		# So we can check that #define and #endif match #ifndef.
  my $guarded;			# True between #define to #endif, 0 afterwards.
  my $guard;			# The found macro or '' if non empty lines occur outside of #define to #endif.
  my $scanworthy;		# Is there anything, other than a guard, that warrants a future rescan?
  my $line_so_far = '';

  LINE: while(<$fh>) {
    s/\r*$//; # Ignore Windoze cruft
    my $continuation = s/\\\s*$//; # gcc allows spaces after \ with a warning
    # Need to handle "//\\\nfoo" correctly.
    if($continued_comment) {
      undef $continued_comment unless $continuation;
      next LINE;
    }
    # Need to handle /*\n/**/ and /*\n//*/ correctly.
    if($pending_comment) {
      if(s!^.*?\*/!!) {
	undef $pending_comment;
      }
      else {
	next LINE;
      }
    }
    # Get rid of single line comments.
    s!/(/.*|\*.*?\*/)!$continued_comment=1 if $continuation && ord $1 == ord '/'; ' '!eg;
    # Parse multiline instructions and comments.
    if( s!/\*.*! ! and $pending_comment = 1 or $continuation ) {
      chomp;
      $line_so_far .= $_;
      next LINE;
    }
    else {
      $_ = $line_so_far.$_;
      $line_so_far = '';
    }
    $go = $self->{ACTIVE};
    if( my $directive = $self->get_directive ) {
      s/\s*$//;
      my $ret = $self->other_directive( $cp, $finfo, $conditional, $directive, \$scanworthy );
      defined $ret or return undef;
      if( $ret ) {
	warn "$absname:$.: Ignoring trailing cruft \"$_\"\n" if $_;
      }
      elsif($directive eq 'include' ) {
	$_ = $self->expand_macros($_) if $conditional;
        my $userinc = s/^\"([^"]*)\"//;
	if($userinc || s/^\<([^>]*)\>//) {
	  local $_;		# Preserve $_ for later
          warn "$absname:$.: File $1 included because condition ", $self->get_bad_expr," cannot be evaluated\n"
	    if ($go == UNKNOWN);
	  $self->include($cp, $userinc ? 'user' : 'sys', $1, $finfo)
	    or return undef;
	}
	$scanworthy = 1;
        warn "$absname:$.: Ignoring trailing cruft \"$_\"\n" if /\S/;
      }
      elsif($conditional) {
	if( $go && $directive eq 'define' && /^(\w+)\s*(.*)/ ) {
	  $self->set_var($1, $2);
	  if( defined $guard_scope && $guard_scope == $self->{SCOPES}[-1] && $guard eq $1 ) {
	    $guarded = 1;	# Looks even more like an include guard.
	    next LINE;
	  }
	  $scanworthy = 1;
	}
	elsif( $go && $directive eq 'undef' && /^\w+$/ ) {
	  $self->set_var($_, undef);
	  $scanworthy = 1;
	}
	elsif( (my $no=$directive eq 'ifndef') || $directive eq 'ifdef' and /^\w+$/ ) {
	  my $def = defined $self->get_var( $_ );
          $go = $no ? !$def : $def;
          $go = $go ? 1 : 0;
	  $self->push_scope([$go, $go]);
	  if( $no && !defined $guarded && !defined $guard ) {
	    $guard_scope = $self->{SCOPES}[-1];	# Might be beginning of an include guard.
	    $guard = $_;
	    next LINE;
	  }
	}
	elsif($directive eq 'else') {
	  $self->update_scope(1);
	  warn "$absname:$.: Ignoring trailing cruft \"$_\"\n" if $_;
	}
	elsif($directive eq 'endif') {
	  $guarded = $guard_scope = 0 # Include guard is ok so far.
	    if $guarded && $guard_scope == $self->{SCOPES}[-1];
	  $self->pop_scope();
	  warn "$absname:$.: Ignoring trailing cruft \"$_\"\n" if $_;
	  next LINE;
	}
	elsif($directive eq 'if' ) {
	  my $maybe_guard = $1
	    if !defined $guarded && !defined $guard && /^!\s*defined\s*\(?\s*(\w+)\s*\)?$/;
          $self->update_scope(undef, "$absname:$.:", $_);
	  if( $maybe_guard ) {
	    $guard_scope = $self->{SCOPES}[-1];	# Might be beginning of an include guard.
	    $guard = $maybe_guard;
	    next LINE;
	  }
	}
        elsif($directive eq 'elif' ) {
          $self->update_scope(1, "$absname:$.:", $_);
	} elsif( !$scanworthy && !$go && ($directive eq 'define' || $directive eq 'undef') ) {
	  $scanworthy = 1;	# Not doing it this time, but maybe from another command.
	}
      }
    }
    $scanworthy ||= (defined $guard && $guard ne ''),
      $guard = ''
      if defined && !$guarded && (!defined $guard || $guard ne '') && /\S/;
				# Non empty line before or after means it is no guard.
  }
  my @build_info;
  @build_info = (GUARD => $guard) # Sane include guard structure for whole file.
    if !$guarded && defined $guard && $guard ne '';
  push @build_info, DONT_SCAN => 1 if $conditional && !$scanworthy;
				# Nothing interesting in file to warrant scanning it again?
  Mpp::File::set_build_info_string $finfo, @build_info
    if @build_info;
  # Don't update_build_infos as this is just optimization info that can be written later.
  1;
}

1;
