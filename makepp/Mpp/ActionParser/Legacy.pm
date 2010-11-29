# $Id: Legacy.pm,v 1.10 2010/10/18 21:34:13 pfeiffer Exp $

=head1 NAME

Mpp::ActionParser::Legacy - Makepp scanner class for legacy interface

=head1 SYNOPSIS

	use Mpp::ActionParser::Legacy;
	my $scanner=Mpp::ActionParser::Legacy->new(\&scanner_c_compilation);

=head1 DESCRIPTION

C<Mpp::ActionParser::Legacy> is an adapter class of type C<Mpp::Lexer> that can
talk to the legacy scanner interface.  Its use is deprecated.

=cut

use strict;
package Mpp::ActionParser::Legacy;
use Mpp::Lexer;
our @ISA = qw/Mpp::Lexer/;


=head1 METHODS

=head2 new

	my $rp = Mpp::ActionParser::Legacy->new($coderef);

Returns a new Mpp::Lexer object that always uses $coderef as its command
parser.

=cut

sub new {
  warn "Use of Mpp::ActionParser::Legacy is deprecated\n";
  my ($self, $coderef)=@_;
  my $class=ref($self)||$self;
  bless { COMMAND_SCANNER => $coderef }, $class;
}

=head2 find_command_parser

	my $cp = $rp->find_command_parser($command, $rule, $dir, \$found);

Call the predefined coderef with $command, $rule and $dir, and return 0.

=cut

sub find_command_parser {
  my ($self, $command, $rule, $dir)=@_;
  my $scanner=$self->{COMMAND_SCANNER};
  my $parser = &$scanner($command, $rule, $dir) || 0;
  unless(UNIVERSAL::isa($parser, 'Mpp::CommandParser')) {
    # This is assumed to mean that calling the $scanner already
    # did the scanning.
    Mpp::log SCAN_UNCACHEABLE => $rule, UNIVERSAL::can( $parser, 'name' ) ? $parser : 'anonymous'
      if $Mpp::log_level;
    $parser=0;
    $rule->mark_scaninfo_uncacheable;
  }
  return $parser;
}

1;

