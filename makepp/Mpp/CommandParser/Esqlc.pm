# $Id: Esqlc.pm,v 1.16 2009/02/11 23:22:37 pfeiffer Exp $

=head1 NAME

Mpp::CommandParser::Esqlc - makepp command parser for various Esql preprocessors

=head1 DESCRIPTION

Scans an Embedded SQL preprocessor or compile command for implicit
dependencies.

This is essentially identical to Gcc.pm, but for some extra args, because some
precompilers can call the compiler and produce an executable.

=cut

use strict;
package Mpp::CommandParser::Esqlc;

use Mpp::CommandParser::Gcc;
our @ISA = 'Mpp::CommandParser::Gcc';

use Mpp::Text;
use Mpp::File;

sub new {
  my $self = &Mpp::CommandParser::Gcc::new_no_gcc;
  require Mpp::Scanner::Esqlc;
  $self->{SCANNER} = new Mpp::Scanner::Esqlc($self->rule, $self->dir);
  $self;
}

sub parse_arg {
  my( undef, undef, $words, $files ) = @_;
  my $scanner = $_[0]{SCANNER};
  for( $_[1] ) {
    if( s/^(sys_?)?include=//i ) { # Oracle Pro*C
      my $sys = $1;
      for( /^\((.+)\)$/ ? split( ',', $1 ) : $_ ) {
	$scanner->add_include_dir( user => $_ );
	$scanner->add_include_dir( sys => $_ ) if $sys;
      }
    } elsif( s/^iname=//i ) { # Oracle Pro*C
      push @$files,
	Mpp::File::exists_or_can_be_built( file_info $_ ) ? $1 : "$1.pc";
    } elsif( s/^define=//i ) { # Oracle Pro*C
      unshift @$words, "-D$_";
    } elsif( Mpp::is_windows ? s/^config=(?=(\/|[a-z]:)?)//i : s/^config=(?=(\/)?)//i ) { # Oracle Pro*C
      $_[0]->add_simple_dependency( $_ );
      substr $_, 0, 0, &Mpp::CommandParser::dirinfo->{FULLNAME} . '/' unless defined $1;
      Mpp::Subs::prebuild file_info( $_ ), $_[0]{RULE}{MAKEFILE}, $_[0]{RULE}{RULE_SOURCE};
				# Might be a generated file.
      if( open my $fh, $_ ) {
	while( <$fh> ) {
	  chomp;
	  parse_arg( $_[0], $_, $words, $files );
	}
      } else {
	warn "config file `$_' not found";
      }
    } elsif( /\.(?:[eps]|pg)c$/ ) {
      push @$files, $_;
    }
  }
}
sub parse_opt {
  my $words = $_[2];
  my $scanner = $_[0]{SCANNER};
  for( $_[1] ) {
    if( s/^E(?=[DU].+)// || s/^Y// ) {	# Informix or Yard
      unshift @$words, "-$_";
    }				# else ignore unknown option.
  }
}

my @suffix_list = ('', '.h');
sub xparse_command {
  $_[0]{SCANNER}->add_include_suffix_list( user => \@suffix_list );
  goto &Mpp::CommandParser::Gcc::xparse_command;
}

1;
