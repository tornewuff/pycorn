# $Id: Specific.pm,v 1.6 2009/02/09 22:45:23 pfeiffer Exp $

=head1 NAME

Mpp::ActionParser::Specific - Makepp scanner class for a specified command scanner

=head1 SYNOPSIS

	use Mpp::ActionParser::Specific;
	my $scanner=Mpp::ActionParser::Specific->new("Mpp::CommandParser::Vcs");

=head1 DESCRIPTION

C<Mpp::ActionParser::Specific> is a class of type C<Mpp::ActionParser> that always
chooses the specified command parser.

=cut

use strict;
package Mpp::ActionParser::Specific;
use Mpp::ActionParser;
our @ISA = qw/Mpp::ActionParser/;


=head1 METHODS

=head2 new

	my $rp = Mpp::ActionParser::Specific->new($command_parser_class_name);

Returns a new Mpp::ActionParser object that always uses a default-constructed
object of class $command_parser_class_name as its command parser.

=cut

sub new {
  my ($self, $cp_class)=@_;
  my $class=ref($self)||$self;
  $cp_class = "Mpp::CommandParser::$cp_class" unless $cp_class =~ /^Mpp::CommandParser::/;
  eval "require $cp_class";
  unless($cp_class->can("new")) {
    warn $@ if $@;
    die "Unable to find `new' method via class $cp_class\n";
  }
  bless { COMMAND_PARSER_CLASS => $cp_class }, $class;
}

=head2 find_command_parser

	my $cp = $rp->find_command_parser($command, $rule, $dir, \$found);

Construct the predefined Mpp::CommandParser class object with $rule and $dir.

=cut

sub find_command_parser {
  ${$_[4]}++;
  $_[0]->{COMMAND_PARSER_CLASS}->new(@_[2..3]);
				# skip command text $_[0]
}

1;

