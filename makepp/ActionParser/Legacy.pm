# $Id: Legacy.pm,v 1.7 2006/12/22 02:18:36 topnerd Exp $

=head1 NAME

ActionParser::Legacy - Makepp scanner class for legacy interface

=head1 SYNOPSIS

	use ActionParser::Legacy;
	my $scanner=ActionParser::Legacy->new(\&scanner_c_compilation);

=head1 DESCRIPTION

C<ActionParser::Legacy> is an adapter class of type C<ActionParser> that can
talk to the legacy scanner interface.
Its use is deprecated.

=cut

use strict;
package ActionParser::Legacy;
use ActionParser;
our @ISA = qw/ActionParser/;


=head1 METHODS

=head2 new

	my $rp = ActionParser::Legacy->new($coderef);

Returns a new ActionParser object that always uses $coderef as its command
parser.

=cut

sub new {
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
  unless(UNIVERSAL::isa($parser, 'CommandParser')) {
    # This is assumed to mean that calling the $scanner already
    # did the scanning.
    ::log SCAN_UNCACHEABLE => $rule, UNIVERSAL::can( $parser, 'name' ) ? $parser : 'anonymous'
      if $::log_level;
    $parser=0;
    $rule->mark_scaninfo_uncacheable;
  }
  return $parser;
}

1;

