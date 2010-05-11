# $Id: ActionParser.pm,v 1.42 2010/02/09 22:39:51 pfeiffer Exp $

=head1 NAME

Mpp::ActionParser - Makepp action parser base class

=head1 SYNOPSIS

	perl_begin
	 { package Mpp::ActionParser::MyParser;
		our @ISA = qw/Mpp::ActionParser/;
	 }
	 sub parser_myscan{
	 	return new Mpp::ActionParser::MyParser;
	 };
	perl_end

	target: dependency : scanner myscan
		action

=head1 DESCRIPTION

C<Mpp::ActionParser> is a base class for makepp(1) action parsers.
It is responsible for dealing with generic shell command parsing.

Ideally, rule options should indicate whether back-quotes should be expanded
and/or parsed.
Currently, they are neither.

=head1 THE SCANNER INTERFACE

When a scanner $parser is specified for a rule, first a subroutine named
"parser_$parser" is sought.
If it is found, then the parser object returned by that subroutine is used
for the rule.
Second, a subroutine named "scanner_$parser" is sought.
If it is found, then a action parser that always uses that routine as the
command parser is used.
The second subroutine search is for legacy compatibility, and it is deprecated.

Note that this is implemented in the Mpp::Makefile package, but we explain it here
because you only need to know about it when you're messing with custom
scanners.

=cut

use strict;
package Mpp::ActionParser;

use Mpp::Text;
use Mpp::Rule;
use Mpp::CommandParser;
use Mpp::File;
use Mpp::FileOpt;

=head1 METHODS

=head2 new

	my $parser=new Mpp::ActionParser;

Returns a new Mpp::ActionParser object.

=cut

my $parser;

sub new {
  my $self = $_[0];
  my $class=ref($self)||$self;

  # Optimize the default parser constructor, because we know it's stateless.
  if($class eq __PACKAGE__) {
    $parser ||= bless \$parser;
    return $parser;
  }

  bless {}, $class;
}

=head2 parse_rule

	$parser->parse_rule($actions, $rule);

For each action in $actions, first deal with shell stuff, for example:

=over 3

=item 1.

Break the actions into commands (split on C<;>, C<&>, C<&&>, C<|> and C<||>)

=item 2.

Deal with I/O redirectors

=item 3.

Determine which environment variables are set by each action [partially done]

=item 4.

Expand and/or parse back-quoted expressions if $rule's options so indicate [TBD]

=item 5.

For "cd" commands, determine the directory into which it changes [TBD]

=back

For each remaining command, call parse_command().

$actions represents $rule's newline-separated actions, which are not derived
from the rule itself so as to avoid duplicated expansion work.

Return value is TRUE on success.

=cut

my %scanner_warnings;

sub parse_rule {
  my( $self, undef, $rule ) = @_; # $_[1] gets used via &Mpp::Rule::split_actions
  my $parsers_found = 0;

  # TBD: There are 3 known problems with adding dependencies here:
  # 1. Since the non-shell actions might call f_target, tacking on
  #    dependencies here can mess things up.
  # 2. Adding a dependency on the makefile is often the wrong thing to do,
  #    because the routine could have been defined in Mpp/Subs.pm (in which
  #    case a dependency probably isn't needed), or in an include file (in
  #    which case the dependency will be on the wrong file).  This is
  #    probably the common case.
  # 3. Calling $rule->add_dependency bypasses the scaninfo caching, so the
  #    list of dependencies after replaying cached scanner info will be
  #    different from the list of dependencies on the last build, and the
  #    cached scanner info will be discarded.  Even if we're willing to
  #    live with that, it would be more efficient to call
  #    $rule->mark_scaninfo_uncacheable, to save it the effort of trying.

  my( undef, undef, $command, $action, @actions ) = &Mpp::Rule::split_actions; # Get the commands to execute.
  $#actions -= 4;		# Eliminate bogus undefs.
  while( 1 ) {
    next unless defined $action;

    # The perl block might have come from an include file (VAR = perl {...})
    # instead of the current makefile, but how could we know?  We might keep
    # track of the package's symbol table across includes.  This still doesn't
    # catch a variable built up over different files with +=, so ignore it.
    if( defined $command ) {	# &cmd
      my $cmd = unquote +(split_on_whitespace $action)[0];
      my $makefile_cmd = $rule->{MAKEFILE}{PACKAGE} . "::c_$cmd";
      if( defined &$makefile_cmd ) { # Function directly or indirectly from makefile?
	require B if !Mpp::is_perl_5_6;
	my $cwd = $rule->build_cwd;
	add_simple_dependency( '.', $cwd, $rule,
			       relative_filename Mpp::is_perl_5_6 ?
				 $rule->makefile->{MAKEFILE} :
				 # For '.' this returns a relative name to where use was performed, but we might be somewhere else now :-(
				 path_file_info( B::svref_2object( \&$makefile_cmd )->FILE, $cwd ),
				 $cwd );
      } elsif( defined &{"Mpp::Cmds::c_$cmd"} ) { # Builtin Function?
	# TODO: Should we use our knowledge of the builtins to find out exactly what files
	# they handle?  That would mean redoing half of what they'll really do, like option
	# processing.  E.g. -fo is a -o option, but if it's -o '|...' then there's no file.
      } else {
	my $finfo = file_info $cmd, $rule->makefile->{CWD}; # Relative path.
	$finfo = file_info Mpp::Subs::f_find_program( $cmd, $rule->makefile, $rule->{RULE_SOURCE} ),
	  $rule->makefile->{CWD}	# Find in $PATH.
	  if ($cmd !~ /\// || Mpp::is_windows > 1 && $cmd !~ /\\/) && !Mpp::File::exists_or_can_be_built $finfo;
	$rule->add_dependency( $finfo );
      }
      next;
    }

    my $dir = '.';
    # Split action into commands
    # TBD: This isn't quite right, because env and cwd settings don't propagate
    # to the next command if they are separated by '&' or '|'.
    for my $command ( split_commands $action ) {
      while( $command =~ /[<>]/ ) {
	my $max = max_index_ignoring_quotes $command, '>';
	my( $expr, $is_in );
	if( $max > -1 ) {	# have >
	  $expr = substr $command, $max + 1;
	  my $lt = max_index_ignoring_quotes $expr, '<';
	  if( $lt > -1 ) {	# < after >
	    $max += $lt;
	    substr $expr, 0, $lt + 1, '';
	    $is_in = 1;
	  } else {
	    --$max if substr( $command, $max-1, 1 ) eq '>';
				# Handle '>>' redirectors
	  }
	} else {
	  $max = max_index_ignoring_quotes $command, '<';
	  if( $max > -1 ) {	# have <
	    $expr = substr $command, $max + 1;
	    $is_in = 1;
	  } else {
	    last;
	  }
	}
	substr( $command, $max ) = '';
	$command =~ s/\d$//;	# strip off "2" in "2> file"
	next if $expr =~ /^\&/;	# Ignore ">&2" etc.
	$expr =~ s/^\s*//;
	my $file = unquote +(split_on_whitespace $expr)[0] or
	  die "Undefined redirection in rule " . $rule->source . "\n";
	next if $file eq '/dev/null' or $file =~ m|[^-+=@%^\w:,./]|;
	if( $is_in ) {
	  add_simple_dependency( $dir, file_info( $dir, $rule->makefile->{CWD} ), $rule, $file );
	} else {
	  add_target( $dir, $rule, $file );
	}
      }
      $rule->{MAKEFILE}->setup_environment;
      my %env = %ENV; # copy the set env to isolate command settings
      while ($command =~ s/^\s*(\w+)=//) {
        my $var=$1;
                                # Is there an environment variable assignment?
        my $ix = index_ignoring_quotes $command, ' ';
                                # Look for the next whitespace.
        $ix >= 0 or $ix = index_ignoring_quotes $command, "\t";
                                # Oops, it must be a tab.
        $ix >= 0 or last;       # Can't find the end--something's wrong.
        $env{$var} = substr $command, 0, $ix, ''; # Chop off the environment variable's value.
        $command =~ s/^\s+//;
      }
      # TBD: expand and/or parse back-quotes
      # Deal with cd commands and set $dir
      if( my( $reldir ) = $command =~ m|^\s*cd\s+([-+=@%^\w:,./]+)\s*|) {
        $dir = $reldir =~ m|^/| ? $reldir : $dir . '/' . $reldir;
      }

      unless($command =~ /^\s*$/) {
	my $result = $_[0]->parse_command( $command, $rule, $dir, \%env );
	return undef unless defined $result;
	$result and ++$parsers_found;
      }
    }
  } continue {
    last unless @actions;
    (undef, undef, $command, $action) = splice @actions, 0, 4;
  }

#
# We want to try to avoid the case where because of some quirk in the command,
# we failed to find a scanner.  Scanners are especially important for
# C/C++ compilation.  See if this rule looks like it's some sort of a
# compile:
#
  if( $parsers_found == 0 &&	# No scanners found at all?
      $Mpp::warn_level &&
      !$rule->{SCANNER_NONE} ) { # From scanner none or skip_word
    my $tinfo = $rule->{EXPLICIT_TARGETS}[0];
    if( ref($tinfo) eq 'Mpp::File' && # Target is a file?
        $tinfo->{NAME} =~ /\.l?o$/ ) { # And it's an object file?
      my $deps = $rule->{ALL_DEPENDENCIES};
      if( grep { 'Mpp::File' eq ref && $_->{NAME} =~ /\.c(?:|xx|\+\+|c|pp)$/i } values %$deps ) {
                                # Looks like C source code?
        warn 'action scanner not found for rule at `',
	  $rule->source,
	  "'\nalthough it seems to be a compilation command.  This means that makepp will
not detect any include files.  To specify a scanner for the whole rule,
add a :scanner modifier line to the rule actions, like this:
    : scanner gcc-compilation	# Scans for include files.
or
    : scanner none		# Does not scan, but suppresses warning.
Or to do it on a per command basis, if you have such obscure commands:
register-scanner my_obscure_cc_wrapper	skip-word
register-scanner my_obscure_cc		c-compilation
"
	    unless $scanner_warnings{$rule->source}++;
      }
    }
  }

  1;
}

=head2 parse_command

	$parser->parse_command($command, $rule, $dir, $setenv_hash);

Parse $command as if it is executed in directory $dir (relative to
$rule->build_cwd), and update $rule accordingly.
$setenv_hash is the environmental settings that are set by the rule itself,
with shell variables I<not> expanded.

In this base class, calls find_command_parser to determine which command
parser to use.
If the result is FALSE, then do nothing more.  (This can't
happen for an object of the base class, since we always use a
plain Mpp/CommandParser.)
Otherwise, the resulting object's parse_command method is called, and
we return that method's return value.

Return value is TRUE on success, zero if no meaningful command parsing was
performed, and undefined if a metadependency failed to build or if scanning
failed.

=cut

sub parse_command {
  #my ($self,$command, $rule, $dir, $setenv)=@_;
  my $result = 0;
  if( my $command_parser = $_[0]->find_command_parser($_[1], $_[2], $_[3] || '.', \$result) ) {
    return undef unless defined $command_parser->parse_command($_[1], $_[4] || {});
  }
  $result;
}

=head2 find_command_parser

	my $command_parser=$parser->find_command_parser(
	  $command, $rule, $dir, \$found
	);

The first word of $command is looked up in %scanners from the
Makeppfile's namespace.
If it isn't found, then undef is returned (after issuing a warning if it
looks like the rule really ought to have a scanner).
Otherwise, the resulting coderef is called with the command, rule
and directory.
If the return value is a reference to an object of type Mpp::CommandParser,
then it is returned.
Otherwise, 0 is returned.
Returning 0 is for backwards compatibility, and it might turn into an error
in the future.
There is no way to indicate a scanning failure if 0 is returned, for
backward compatibility.

=cut

sub find_command_parser {
  my( undef, $command, $rule, $dir, $found ) = @_;
  my $firstword;
  if( ($firstword) = $command =~ /^\s*(\S+)/ ) {
    if( Mpp::is_windows < 2 && $firstword =~ /['"\\]/ ) {	# Cheap way was not good enough.
      ($firstword) = unquote +(split_on_whitespace $command)[0];
    } elsif( Mpp::is_windows > 1 && $firstword =~ /"/ ) {
      ($firstword) = split_on_whitespace $command;
      $firstword =~ tr/"//d;	# Don't unquote \, which is Win dir separator
    }
  }
  my $parser;
  if( defined $firstword ) {
    no strict 'refs';
    my $scanner_hash = \%{$rule->{MAKEFILE}{PACKAGE} . '::scanners'};
    $parser = $scanner_hash->{$firstword};
				# First try it unmodified.
    unless( $parser ) {		# If that fails, strip out the
				# directory path and try again.
      $firstword =~ s@^.*/@@ || Mpp::is_windows > 1 && $firstword =~ s@^.*\\@@ and  # Is there a directory path?
        $parser = $scanner_hash->{$firstword};
      $parser ||= $Mpp::Subs::scanners{$firstword} ||
	$firstword =~ /gcc|g\+\+/ && \&Mpp::Subs::scanner_gcc_compilation;
    }
  }
  if ($parser) {               # Did we get one?
    $$found++;
    $parser=&$parser($command, $rule, $dir) || 0;
                                # Call the routine.
    unless(UNIVERSAL::isa($parser, 'Mpp::CommandParser')) {
      # This is assumed to mean that calling the %scanners value already
      # did the scanning.
      $parser=0;
      Mpp::log SCAN_UNCACHEABLE => $rule, $firstword
	if $Mpp::log_level;
      $rule->mark_scaninfo_uncacheable;
    }
  } else {   # No parser:
    $parser = new Mpp::CommandParser($rule, $dir);
  }
  $parser;
}

=head2 add_dependency

=head2 add_optional_dependency

=head2 add_simple_dependency

Like the corresponding methods in C<Mpp::CommandParser>, except that it's an
ordinary function, and the parameter list is prepended with the command's
directory name, directory Mpp::File object, and Rule object.

=head2 add_target

Like the corresponding methods in C<Mpp::CommandParser>, except that it's an
ordinary function, and the parameter list is prepended with the command's
directory name and Rule object.

=head2 add_env_dependency

Like the corresponding methods in C<Mpp::CommandParser>, except that it's an
ordinary function, and the parameter list is prepended with the Rule object.

=cut

sub relative_path {
#  my ($dir, $name)=@_;
  die if ref $_[1];
  die unless defined $_[0] && $_[0] ne '';
  return $_[1] if $_[1] =~ m@^/@ || $_[0] eq '.';
  "$_[0]/$_[1]";
}

# NOTE: $finfo can be either a filename of a Mpp::File object. If it's undefined,
# then that means that there *would be* a dependency on the file if it existed
# (i.e. rebuild if it comes into existence), so $incname must be defined in
# order to cache the nonexistent dependency.
sub add_any_dependency_ {
  my( $dir, $dirinfo, $rule, $meta, $finfo, $tag, $src, $incname ) = @_;
  if( defined $finfo ) {
    unless( ref $finfo ) {
      $incname ||= relative_path $dir, $finfo;
      $finfo = file_info $finfo, $dirinfo;
    }
  } else {
    die unless defined $incname;
  }
  return 1 if $incname eq "";	# This is an error, so don't add a dep
  my $method = $meta ? 'add_meta_dependency' : 'add_implicit_dependency';
  $rule->$method(
    $tag,
    (defined $src and
     relative_filename is_or_will_be_dir( $src ) || $src->{'..'}, $rule->build_cwd),
    $incname,
    $finfo
  ) and $meta and do {
    $rule->{SCAN_FAILED} = $finfo if $finfo;
    die "SCAN_FAILED\n";
  };
  $finfo || 1;
}

sub add_dependency {
  splice @_, 3, 0, 1;
  &add_any_dependency_;
}

sub add_optional_dependency {
  my ($dir, $dirinfo, $rule, $name, $simple) = @_;
  die if ref $name;
  my $finfo = file_info($name, $dirinfo);
  undef $finfo unless Mpp::File::exists_or_can_be_built $finfo;
  add_any_dependency_(
    $dir, $dirinfo, $rule,
    !$simple,
    $finfo, undef, undef, relative_path($dir, $name)
  );
}

sub add_simple_dependency {
  splice @_, 3, 0, 0;
  &add_any_dependency_;
}

sub add_target {
  $_[1]->add_implicit_target( relative_path $_[0], $_[2] );
}

sub add_env_dependency {
  $_[0]->add_implicit_env_dependency( $_[1] );
}

1;
