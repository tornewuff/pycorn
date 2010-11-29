# $Id: Lexer.pm,v 1.45 2010/11/17 21:35:52 pfeiffer Exp $

=head1 NAME

Mpp::Lexer - Makepp lexer for finding commands and redirections in a rule

=head1 DESCRIPTION

C<Mpp::Lexer> is responsible for lexical analysis of shell commands.  It finds
file redirections as prerequisites or targets.  It also splits pipelines or
series of commands checking each one, whether there is a command parser for it.

Ideally, rule options should indicate whether back-quotes should be expanded
and/or parsed.  Currently, they are neither.

This class should handle all POSIX-like shells (bash, ksh, zsh) well enough,
but not rather different ones like csh.

=cut

use strict;
package Mpp::Lexer;

use Mpp::Text;
use Mpp::Rule;
use Mpp::CommandParser;
use Mpp::File;
use Mpp::FileOpt;

=head1 METHODS

=head2 new

	my $lexer=new Mpp::Lexer;

Returns a new Mpp::Lexer object.

=cut

my $lexer;
sub new {
  my $self = $_[0];
  my $class=ref($self)||$self;

  # Optimize the default lexer constructor, because we know it's stateless.
  if($class eq __PACKAGE__) {
    $lexer ||= bless \$lexer;
    return $lexer;
  }

  bless {}, $class;
}

=head2 lex_rule

	$lexer->lex_rule($actions, $rule);

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
OTOH, you can always use "SOMEOPT ;= $(shell someconfig --opt)" instead.

=item 5.

For "cd" commands, determine the directory into which it changes

=back

For each remaining command, call parse_command().

$actions represents $rule's newline-separated actions, which are not derived
from the rule itself so as to avoid duplicated expansion work.

Return value is TRUE on success.

=cut

my %parser_warnings;
our @actions;			# private, except find_command_parser (p_shell) alters it
sub lex_rule {
  my $rule = $_[2];		# $_[1] gets used via &Mpp::Rule::split_actions
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

  (undef, undef, my( $command, $action ), local @actions) = &Mpp::Rule::split_actions; # Get the commands to execute.
  $#actions -= 4;		# Eliminate bogus undefs.
  my $p_shell;
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
				 # In case of chdir since '.'-based use was performed:
				 path_file_info( B::svref_2object( \&$makefile_cmd )->FILE, $cwd ),
				 $cwd );
      } elsif( defined &{"Mpp::Cmds::c_$cmd"} ) { # Builtin function?
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
    for $command ( split_commands $action ) {
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
      $command =~ s/^\s*(?:(?:[.!]|do|e(?:lif|lse|xec)|if|source|then|while|until)\s+)*//;
				# Skip "if gcc main.o -labc -o my_program; ..." or even "then if ! . myfile"
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
	local $rule->{PARSER} if $p_shell; # Special recursion brake set by find_command_parser's p_shell handling.
	my $found = $_[0]->parse_command( $command, $rule, $dir, \%env );
	return undef unless defined $found;
	$found == 1 and ++$parsers_found;
      }
    }
  } continue {
    last unless @actions;
    ($p_shell, undef, $command, $action) = splice @actions, 0, 4;
    $p_shell = ref $p_shell;
  }

#
# We want to try to avoid the case where because of some quirk in the command,
# we failed to find a scanner.  Scanners are especially important for
# C/C++ compilation.  See if this rule looks like it's some sort of a
# compile:
#
  if( $parsers_found == 0 &&	# No parsers found at all?
      !$rule->{SCANNER_NONE} ) { # From deprecated form of scanner none or skip_word
    my $tinfo = $rule->{EXPLICIT_TARGETS}[0];
    if( ref($tinfo) eq 'Mpp::File' && # Target is a file?
        $tinfo->{NAME} =~ /\.l?o$/ ) { # And it's an object file?
      my $deps = $rule->{ALL_DEPENDENCIES};
      if( grep { 'Mpp::File' eq ref && $_->{NAME} =~ /\.c(?:|xx|\+\+|c|pp)$/i } values %$deps ) {
                                # Looks like C source code?
        warn 'command parser not found for rule at `',
	  $rule->source,
	  "'\nalthough it seems to be a compilation command.  This means that makepp will
not detect any include files.  To specify a parser for the whole rule,
add a :parser modifier line to the rule actions, like this:
    : parser gcc-compilation	# Scans source for include files.
or
    : parser none		# Does not scan, but suppresses warning.
Or to do it on a per command basis, if you have such obscure commands:
register-parser my_obscure_cc_wrapper	skip-word
register-parser my_obscure_cc		c-compilation
"	    unless $parser_warnings{$rule->source}++;
      }
    }
  }

  1;
}

=head2 parse_command

	$lexer->parse_command($command, $rule, $dir, $setenv_hash, \$found);

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
  my $command = $_[1];		# modified by skip-word
  my $found = 0;
  if( my $command_parser = $_[0]->find_command_parser($command, $_[2], $_[3] || '.', \$found) ) {
    return unless
      defined( ref $command_parser ?
	       $command_parser->parse_command($command, $_[4] || {}) :
	       $command_parser );
  }
  $found;
}

=head2 find_command_parser

	my $command_parser=$lexer->find_command_parser($command, $rule, $dir, \$found);

The first word of $command is looked up in %parsers from the Makeppfile's namespace.
If it isn't found, then undef is returned.
Otherwise, the resulting coderef is called with the command, rule and directory.
If the return value is a reference to an object of type Mpp::CommandParser,
then it is returned.
Otherwise, 0 is returned.
There is no way to indicate a scanning failure if 0 is returned, for
backward compatibility.

=cut

sub find_command_parser {
  my( undef, $command, $rule, $dir, $found ) = @_;
  my $parser;
  my $from_rule = $rule->{PARSER};
  my( $firstword, @rest ) = split_on_whitespace $command;
  until( defined $parser ) {
    if( $from_rule ) {
      $parser = $from_rule;
      $from_rule = 0;		# Only 1st word, not again after skip-word
    } else {
      if( Mpp::is_windows < 2 && $firstword =~ /['"\\]/ ) {
	$firstword = unquote $firstword;
      } elsif( Mpp::is_windows > 1 && $firstword =~ /"/ ) {
	$firstword =~ tr/"//d;	# Don't unquote \, which is Win dir separator
      }
      if( defined $firstword ) {
	no strict 'refs';
	my $parsers = \%{$rule->{MAKEFILE}{PACKAGE} . '::parsers'};
	$parser = $parsers->{$firstword};
				# First try it unmodified.
	unless( $parser ) {	# If that fails, strip out the directory path and try again.
	  $firstword =~ s@^.*/@@ || Mpp::is_windows > 1 && $firstword =~ s@^.*\\@@ and  # Is there a directory path?
	    $parser = $parsers->{$firstword};
	  $parser ||= $Mpp::Subs::parsers{$firstword} ||
	    $firstword =~ /gcc|g\+\+/ && \&Mpp::Subs::p_gcc_compilation;
	}
      }
    }
    if( $parser ) {		# Did we get one?
      $parser = &$parser( $command, $rule, $dir ); # Call the routine.
      if( ref $parser ) {
	$$found = 1;
	$parser = 0 unless UNIVERSAL::isa $parser, 'Mpp::CommandParser';
				# This is assumed to mean that calling the parser already did the scanning.
      } elsif( $parser ) {	# p_skip_word or p_shell
	shift @rest while @rest && $rest[0] =~ /^["'\\]?-/; # skip options
	if( $parser == &Mpp::Subs::p_shell ) {
	  unshift @actions, defined $from_rule ? \1 : undef, # special marker to not propagate parser into subcommand
	      undef, undef, join ' ', map unquote, @rest;
				# just 1 arg for sh -c cmd, but handle eval cmd1\; cmd2
	  Mpp::log PARSE_SHELL => $firstword, $actions[3], $rule
	    if $Mpp::log_level;
	  $$found = 2;		# Not 1, but continue
	  return;
	}
	# Must be p_skip_word; doesn't count as found
	Mpp::log PARSE_SKIP_WORD => $firstword, $rest[0], $rule
	  if $Mpp::log_level;
	$_[1] = $command = join ' ', @rest; # parse only rest
	$firstword = shift @rest;
	undef $parser;
      } elsif(  defined $parser ) { # p_none
	Mpp::log PARSE_NONE => $firstword, $rule
	  if $Mpp::log_level;
	$$found = 1;
      } else {			# failed
	Mpp::log SCAN_UNCACHEABLE => $rule, $firstword
	  if $Mpp::log_level;
	$rule->mark_scaninfo_uncacheable;
	return;
      }
    } else {
      $parser = 0;
    }
  }
  $parser || new Mpp::CommandParser($rule, $dir); # none, fall back to trivial parser
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
