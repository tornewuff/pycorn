=head1 NAME

ActionParser::Specific - Makepp scanner class for a specified command scanner

=head1 SYNOPSIS

	use ActionParser::Specific;
	my $scanner=ActionParser::Specific->new("CommandParser::Vcs");

=head1 DESCRIPTION

C<ActionParser::Specific> is a class of type C<ActionParser> that always
chooses the specified command parser.

=cut

use strict;
package ActionParser::Specific;
use ActionParser;
our @ISA = qw/ActionParser/;


=head1 METHODS

=head2 new

	my $rp = ActionParser::Specific->new($command_parser_class_name);

Returns a new ActionParser object that always uses a default-constructed
object of class $command_parser_class_name as its command parser.

=cut

sub new {
  my ($self, $cp_class)=@_;
  my $class=ref($self)||$self;
  $cp_class = "CommandParser::$cp_class" unless $cp_class =~ /^CommandParser::/;
  eval "require $cp_class";
  unless($cp_class->can("new")) {
    warn $@ if $@;
    die "Unable to find `new' method via class $cp_class\n";
  }
  bless { COMMAND_PARSER_CLASS => $cp_class }, $class;
}

=head2 find_command_parser

	my $cp = $rp->find_command_parser($command, $rule, $dir, \$found);

Construct the predefined CommandParser class object with $rule and $dir.

=cut

sub find_command_parser {
  ${$_[4]}++;
  $_[0]->{COMMAND_PARSER_CLASS}->new(@_[2..3]);
				# skip command text $_[0]
}

1;

