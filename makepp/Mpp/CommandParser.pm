# $Id: CommandParser.pm,v 1.32 2009/02/11 23:22:37 pfeiffer Exp $

=head1 NAME

Mpp::CommandParser - Base class for makepp command parsers

=head1 SYNOPSIS

  use Mpp::CommandParser;
  our @ISA = qw/Mpp::CommandParser/;

=head1 DESCRIPTION

C<Mpp::CommandParser> is a base class for makepp(1) command parsers.  When used
directly, it does nothing except add a dependency on the executable.

The parser for a particular command should derive from this class.
The parser should instantiate objects of type C<Mpp::Scanner> for
the languageZ<>(s) of the files that it scans for include directives.

=cut

use strict;
package Mpp::CommandParser;
use Mpp::Text ();
use Mpp::Subs ();
use Mpp::File;

=head1 METHODS

=head2 new

  my $parser=new Mpp::CommandParser($rule, $dir);

Returns a new C<Mpp::CommandParser> object for rule $rule.
$dir is the directory (a string) in which the command runs.
The derived class may override this method and its parameters.

=cut

sub new {
  my( $self, $rule, $dir ) = @_;
  my $class=ref($self)||$self;
  $rule && $dir or die;
  UNIVERSAL::isa($rule, 'Mpp::Rule') or die;
  UNIVERSAL::isa($dir, 'Mpp::File') and die;
  bless {
    RULE => $rule,
    DIR => $dir,
  }, $class;
}

=head2 parse_command

  $parser->parse_command($command, $setenv_hash);

Splits the command into words, prints a log message, and calls xparse_command.

=cut

our $ignore_exe;		# Emergency override.
my %is_builtin;
@is_builtin{qw(
  source break cd continue echo exit export kill let pwd return
  set test ulimit umask unset wait
  case esac fi for done
)} = ();			# Make these exist
sub parse_command {
  my( $self, $command, $setenv_hash ) = @_;

  Mpp::log PARSE => $command, $self->dirinfo, $self->rule
    if $Mpp::log_level;

  my @cmd_words = Mpp::is_windows < 2 ?
    Mpp::Text::unquote_split_on_whitespace $command :
    map { tr/"//d; $_ } Mpp::Text::split_on_whitespace $command; # Don't unquote \, which is Win dir separator, TODO: \" -> "
  unless( $ignore_exe || UNIVERSAL::isa($self->rule->build_check_method, 'Mpp::BuildCheck::ignore_action') ) {
    # TODO: This test is redundant with scanner_skip_word:
    if( $cmd_words[0] =~ /^(?:\.|do|e(?:val|xec)|if|source|t(?:hen|ime)|while|until)$/ ) {
      $self->add_executable_dependency( $cmd_words[1] ) if @cmd_words > 1
        && !exists $is_builtin{$cmd_words[1]};
    } elsif( !exists $is_builtin{$cmd_words[0]} ) {
      $self->add_executable_dependency( $cmd_words[0] );
    }
  }
  $self->xparse_command( \@cmd_words, $setenv_hash );
}

=head2 add_executable_dependency

Gets called before xparse_command with the first command word as the
argument.
By default, it adds a dependency on the file given by the word, but only if it
contains no shell metacharacters.  It then adds runtime dependencies, if any,
and calls add_more_executable_dependencies (which the subclass can override)
with the directory name of the executable.

=cut

sub add_executable_dependency {
  my( $self, $exe ) = @_;
  # Ignore the executable if it has shell metacharacters, because we
  # won't easily be able to figure out what it is.
  if( $exe !~ m|[^-+=@%^\w:,./]| || Mpp::is_windows > 1 && $exe !~ /[^-+=@%^\w:,.\\]/ ) {
    # TODO: This is not the best spot to do this, because the path doesn't get
    # rechecked if the command doesn't change.  It would be better to do this
    # like searching for includes.
    if( $exe !~ m@/@ || Mpp::is_windows > 1 && $exe !~ /\\/ ) {
      return if $Mpp::no_path_executable_dependencies;
      my $CWD_INFO = $CWD_INFO; # Might load a makefile and chdir there:
      $exe = Mpp::Subs::f_find_program( $exe, $self->{RULE}{MAKEFILE}, $self->{RULE}{RULE_SOURCE}, 1 );
      chdir $CWD_INFO;
      return if $exe eq 'not-found';
    }
    $self->add_simple_dependency($exe);
    my $dirinfo = $self->dirinfo;
    my $finfo = file_info($exe, $dirinfo);
    my @runtime_deps = values %{$finfo->{RUNTIME_DEPS} || {}};
    my %visited = map { int, 1 } @runtime_deps;
    while(@runtime_deps) {
      my $runtime_dep = shift @runtime_deps;
      $self->add_simple_dependency( relative_filename $runtime_dep, $dirinfo );
      $visited{int()}++ or push @runtime_deps, $_
	for values %{$runtime_dep->{RUNTIME_DEPS} || {}};
    }
    $self->add_more_executable_dependencies($1)
      if $exe =~ m@^(.+)/[^/]+$@ || Mpp::is_windows > 1 && $exe =~ /^(.+)\\[^\\]+$/;
  }
}

*add_more_executable_dependencies = \&Mpp::Text::CONST0;

=head2 input_filename_regexp

  $parser->input_filename_regexp($command[0], [qw(.c .C .cpp .cc .c++)]);

Returns a regular expression against which arguments will match if they
look like ordinary (positional) input files.
This is the common way to pick up custom file suffixes from
register_input_suffix statements.

=cut

sub input_filename_regexp {
  my ($self, $firstword, $predefs) = @_;
  my $list;
  {
    no strict 'refs';
    my $suffix_hash = \%{$self->rule->{MAKEFILE}{PACKAGE} . '::input_suffix_hash'};
    $list = $suffix_hash->{$firstword} || do {
      $firstword =~ s@^.*/@@ and $suffix_hash->{$firstword}
    };
  }
  my @list = ($list ? @$list : (), $predefs ? @$predefs : ());
  return unless @list;
  return '(' . join('|', map { quotemeta } @list) . ')$';
}

=head2 xparse_command

  $parser->xparse_command($command_array, $setenv_hash);

The derived class should override this to set the default signature method,
to parse the $command_array command
and to add the implicit targets and dependencies to $self->rule.
$setenv_hash is a hashref indicating the command-line environment settings.
(Whether its values have shell variables expanded is not yet guaranteed.)

The current plan is that a new object of this class is created for each
command, but that might change in the future.
If it does, then this method is responsible for clearing previous command
information, such as the include paths.

If the command line contains another command to be parsed, then that can
be handled by calling $self->rule->scan_action recursively.

A TRUE return value indicates that some meaningful scanner was successfully
employed.
An undefined return value is interpretted as a failure.
A failure forces the rule to propagate failure status, without attempting to
build the target.

=cut

*xparse_command = \&Mpp::Text::CONST1;

=head2 rule

  my $rule=$parser->rule;

Returns the rule.

=cut

sub rule { $_[0]->{RULE} }

=head2 dir

  my $dir=$parser->dir;

Returns the directory Mpp::File object.

=cut

sub dir { $_[0]->{DIR} }

=head2 add_dependency

=head2 add_optional_dependency

=head2 add_simple_dependency

=head2 add_target

=head2 add_env_dependency

  $parser->add_dependency($name);
  $parser->add_dependency($finfo, $tag, $src, $incname);
  $parser->add_optional_dependency($name, $simple);
  $parser->add_simple_dependency($name);
  $parser->add_simple_dependency($finfo, $tag, $src, $incname);
  $parser->add_target($name);
  $parser->add_env_dependency($name);

Add a dependency or target on file $name relative to $self->dir.
Returns the Mpp::File object.

The 5-arg versions add a dependency on $finfo, and record the dependency
based on $tag, $src, and $incname.
$finfo can be undefined, indicating that the file was sought but not found.
The 1-arg versions add a dependency on $name (relative to $parser->dirinfo),
and record the dependency based on undef, undef, and $name relative to
$parser->rule->build_cwd.
The 1-arg version of add_dependency is guaranteed either to return a
sucessfully built Mpp::File object (possibly nonexistent if it was phony or
marked for don't build, etc.) or to die.

add_dependency adds a "meta" dependency (a dependency on which the rule's
dependencies might depend), and causes the dependency to be built before
returning.
If the dependency file was found but couldn't be built, then it dies with
"SCAN_FAILED\n".
If the dependency wasn't found, then it returns 1.

add_optional_dependency is just like add_dependency, except that no error
occurs if the file does not exists and cannot be built.
This is useful for adding dependencies on optional initialization files,
because the target becomes outdated should the file come into existence.
If you pass it a second TRUE argument, then it adds a simple optional
dependency instead of a meta dependency.

add_simple_dependency adds a simple dependency, and does not attempt to
build the dependency if it doesn't exist.

Even if no actual file dependency is added, calls to add_dependency and
add_simple_dependency and recorded for the purposes of caching scanner info,
in case the file becomes buildable in subsequent makepp runs.

add_env_dependency adds a dependency on the value of the named environment
variable.
If $name is of the form /(.+) in (\S+)/, then $1 is a filename to seek in
directories given by the value of the environment variable named by $2,
split on colons.
The rule is then dependent on the name of the first directory in which the
file is found or can be built.
(This is useful for avoiding rebuilds when a search path changed without
affecting where files are found along the search path.)

=cut

sub dirinfo {
  $_[0]{DIRINFO} ||=
    file_info $_[0]{DIR}, $_[0]{RULE}->build_cwd;
}

sub add_dependency {
  my $self = shift;
  Mpp::ActionParser::add_dependency( $self->{DIR}, $self->dirinfo, $self->{RULE}, @_ );
}

sub add_optional_dependency {
  my $self = shift;
  Mpp::ActionParser::add_optional_dependency( $self->{DIR}, $self->dirinfo, $self->{RULE}, @_ );
}

sub add_simple_dependency {
  my $self = shift;
  Mpp::ActionParser::add_simple_dependency( $self->{DIR}, $self->dirinfo, $self->{RULE}, @_ );
}

sub add_target {
  my $self = shift;
  Mpp::ActionParser::add_target( $self->{DIR}, $self->{RULE}, @_ );
}

sub add_env_dependency {
  my $self = shift;
  Mpp::ActionParser::add_env_dependency( $self->{RULE}, @_ );
}

#=head1 FIELDS
#
#The following fields represent the current implementation of this class,
#but they may change without notice.
#Therefore, clients outside this class should using accessor methods instead.
#
#=head2 $self->{RULE}
#
#A reference to the rule.
#
#=head2 $self->{DIR}
#
#The directory under which the present command being parsed will execute.

=head1 BUGS

There is no variant of the add_dependency method that doesn't add a
dependency if the file doesn't exist and can't be built.
(For example, an optional configuration file.)
This would be easy to implement, but there aren't any scanners that
would use it right now.

=head1 FUTURE WORK

Dependencies on the environment should also be captured by xparse_command.
I don't know if there is a way to do this at all yet, but in principle it
could be added.
The $setenv_hash parameter can be used to avoid adding depedencies on
variables that are set earlier in the action.
For search paths picked up from the environment, it would be a lot more
efficient to capture dependencies in the form
"rebuild unless file X is picked up from directory Y,"
rather than "rebuild if the path changed."
Also, if we could avoid rebuilding if the file is picked up from a different
location but has the same MD5, then that would be cool.

Perhaps there should be a finfo method that just returns
file_info($_[0], $self->dirinfo).

=cut

1;
