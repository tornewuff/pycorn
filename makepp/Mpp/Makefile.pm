# $Id: Makefile.pm,v 1.141 2010/11/17 21:35:52 pfeiffer Exp $
package Mpp::Makefile;

use Mpp::Glob qw(wildcard_action needed_wildcard_action);
use Mpp::Event qw(wait_for);
use Mpp::Text qw(max_index_ignoring_quotes index_ignoring_quotes split_on_whitespace split_on_colon
		unquote unquote_split_on_whitespace requote hash_neq);
use Mpp::Subs;
use Mpp::Cmds ();
use Mpp::File;
use Mpp::FileOpt;

use strict qw(vars subs);

=head1 NAME

Mpp::Makefile -- an object that parses makefiles and stores their relevant info

=head1 USAGE

  $makefile = Mpp::Makefile::load('filename_or_dir_name');
  $makefile = Mpp::Makefile::load($fileinfo_or_dirinfo);

  $makefile->expand_text('$(STRING) $(WITH) $(MAKE) $(VARIABLES) $(OR FUNCTIONS)');

=head1 DESCRIPTION

The Mpp::Makefile package is responsible for parsing a makefile, and
subsequently for holding all relevant information from the makefile,
such as variable definitions.

A new Makefile class may be created at any time, whenever you discover
that it is available.  The Mpp::Makefile constructor parses the makefile, and
integrates all of its rules into the makepp's memory database.

=cut

my $package_seed = '000';
# These get localized, so they can't be my vars.
our( $makefile, $makefile_directory, $makefile_name, $makefile_contents,
     $makefile_lineno, @hold_lines );

our $global_command_line_vars;	# The command line variables that were
				# specified on the top level command line
				# (as opposed to additional ones that may
				# have been specified on the load_makefile
				# or recursive make lines).
our $c_preprocess = 0;		# Set to 1 (parse assignments) or 2 in &preprocess.

#
# Targets that we ignore:
#
my %ignored_targets;
@ignored_targets{qw(.DEFAULT .DELETE_ON_ERROR .EXPORT_ALL_VARIABLES .IGNORE
	.INTERMEDIATE .NOEXPORT .POSIX .PRECIOUS .SECONDARY .SILENT)} = ();
				# These targets should be ignored.  In fact,
				# they will be even if we didn't do this, but
				# if they happen to be the first target in the
				# file we don't want to make them the default
				# target.

=head2 expand_text($makefile, 'string', $makefile_line)

  my $expanded_string = $makefile->expand_text('string with make variables',
					       $makefile_line);

Expands all the make variables and expressions in the text and returns the
result.	 If an error occurs, die's with the error message prefixed by
C<$makefile_line>.

=cut

my $expand_bracket;
my $self_expanding_functions = qr/^(?:and|if|foreach|map|or|perl)\b/;
sub expand_text {
  -1 < index $_[1], '$' or return $_[1]; # No variables ==> no substitution,
				# so exit immediately to avoid consuming CPU
				# time.  Don't use regexp or a caller's $2 as
				# argument gets undefed on the stack.
  my $self = $_[0];
  my $makefile_line = $_[2];
#  local *_ = \$_[1];		# Get the string into $_.  This gets the
				# string without making a copy (by experiment,
				# local $_ = $_[1] makes a copy).
				# Note that this messes up @_, so we have
				# to do this after getting the other arguments.
  local $_ = $_[1];		# The above does not work because args like
				# $1, $2, are references which we overwrite in
				# our regexps.
  my $ret_str = '';
  pos = 0;			# Suppress a warning message.

  if( !expand_variable( $self, 'makepp_simple_concatenation', $makefile_line )) {
#
# Code for handling rc-style substitution (the default):
#
    my @cur_words = '';		# The word we are currently expanding.	There
				# may be more than one if we are in the middle
				# of RC expansion.  For example, if X = a b c
				# and we are expanding q$(X)r, then
				# @cur_words will contain just one element
				# when we see the q.  When we process the $(X)
				# @cur_words will be (qa, qb, qc).  Then when
				# we see the r, it turns into (qar, qbr, qcr).
				# When we see a space, it is reset.

    while (pos() < length) {
      if (/\G([\s,:;{[()\]}=#`"'@]+)/gc) {	 # Word separators?
	$ret_str .= "@cur_words$1";
				# Store the accumulated words.
				# Put in the original punctuation.
	@cur_words = '';
      } elsif (/\G([^\$\s,:;{[()\]}=#`"'@]+)/gc) {  # Text of a word?
	$_ .= $1 for @cur_words; # Append to each word fragment we are holding.
      } else {			# Must be a dollar sign.
	++pos;			# Skip it.
	if( $expand_bracket ) {
	  if( /\G([^[])/gc ) {
	    $_ .= "\$$1" for @cur_words;
				# Replace with a single one.
	    next;
	  }
	} elsif( /\G\$/gc ) {	# Double dollar sign?
	  $_ .= '$' for @cur_words;
				# Replace with a single one.
	  next;
	}
#
# Get the whole text of the expression to expand, and expand any nested
# parts of it.
#
	my $oldpos = pos;	# Remember where the expression starts.
	my $len = &Mpp::Text::skip_over_make_expression; # Find the end of it.
	defined $len or
	  die "$makefile_line: unterminated reference $_\n";
	my $newpos = pos;	# For some obscure reason, the following
				# messes up pos($_).
	my $expr = substr $_, $oldpos + $len, $newpos - $oldpos - 2*$len;
				# Get the expression to expand.
	$expr = expand_text($self, $expr, $makefile_line)
	  if $expr !~ $self_expanding_functions;
				# Expand any nested make expressions.

	my $space = ord( $expr ) == ord ' ';
	$expr = expand_expression($self, $expr, $makefile_line);
				# Evaluate the expression.

	if( $space && !length $expr ) {
				# Expression is also preceded by a separator?
				# E.g., this is an expression like
				# Optimize for special case.  Also, do not
				# destroy newlines in variables if there
				# is no reason to do so.  This is necessary
				# for support for the define statement.

	  @cur_words = ();
	} elsif( @cur_words == 1 && !length( $cur_words[0] ) &&
	    substr($_, $newpos, 1) =~ /^[\s,:;{[()\]}=#`"'@]?$/
				# Next char is separator?
	    ) {
				# Expression is also preceded by a separator?
				# E.g., this is an expression like
				# Optimize for special case.  Also, do not
				# destroy newlines in variables if there
				# is no reason to do so.  This is necessary
				# for support for the define statement.

	  $ret_str .= $expr;  # Just append it directly.
	} else {
	  my @exp_words = split_on_whitespace($expr);

	  if (@exp_words == 1) { # Optimize for the most common case.
				# Treat as a single word, and append to each
				# current word.
	    $_ .= $exp_words[0] for @cur_words;
	  } elsif (@exp_words > 1) { # We have to do a real cartesian product.
	    my (@old_words) = @cur_words; # Make a copy of the old things.
	    @cur_words = ();
	    foreach my $old_word (@old_words) {
	      push @cur_words, "$old_word$_" for @exp_words;
	    }
	  }
	}
	pos = $newpos;	       # Reset the position after the make expression.
      }
    }

    $ret_str .= "@cur_words"; # Store the last word(s), if any.
  } else {
#
# Code for handling the traditional substitution style (needed for some
# legacy makefiles, usually those that depend on leading/trailing whitespace).
#
    while (pos() < length) {
      if( /\G([^\$]+)/gc ) {	# Text w/o variables?
	$ret_str .= $1;		# Just append it.
      } else {			# Must be a dollar sign.
	++pos;			# Skip it.
	if( $expand_bracket ) {
	  if( /\G([^[])/gc ) {
	    $ret_str .= "\$$1";
	    next;
	  }
	} elsif( /\G\$/gc ) {	# Double dollar sign?
	  $ret_str .= '$';	# Replace with a single one.
	  next;
	}
#
# Get the whole text of the expression to expand, and expand any nested
# parts of it.
#
	my $oldpos = pos;	# Remember where the expression starts.
	my $len = &Mpp::Text::skip_over_make_expression; # Find the end of it.
	defined $len or
	  die "$makefile_line: unterminated reference $_\n";
	my $newpos = pos;	# For some obscure reason, the following
				# messes up pos($_).
	my $expr = substr $_, $oldpos + $len, $newpos - $oldpos - 2*$len;
				# Get the expression to expand.
	$expr = expand_text($self, $expr, $makefile_line)
	  if $expr !~ $self_expanding_functions;
				# Expand any nested make expressions.

	$ret_str .= expand_expression($self, $expr, $makefile_line);
				# Do the expansion.
	pos = $newpos;		# Move to after the make expression.
      }
    }

  }
  $ret_str;
}

#
# This is a helper routine which is used for expanding a complex variable expression.
# Arguments:
# a) The makefile.
# b) The expression to expand.	This should have no nested make expressions.
#    This expression should have had the surrounding parentheses removed.
#    For example, if expand_text() was called on the string
#    'x = $(patsubst %.o, %.c, stuff)' then the string that we actually
#    will see is 'patsubst %.o, %.c, stuff'.
# c) The makefile line number (for error messages only).
#
sub expand_expression {
  my ($self, $expr, $makefile_line) = @_; # Name the arguments.
  my $result;
  if( $expr =~ s/^\s+//) {	# It begins with a space.  This is just a
				# trigger for rc-style expansion, so we should
				# return the text verbatim.
    $result = $expr;
  } elsif( $expr =~ /^([-.\w]+)\s+(.*)/s ) {
				# Does it begin with a leading word,
				# so it must be a function?
    my( $rtn, $rest_of_line ) = ($1, $2);
    my $orig = $rtn;
    my $code = $rtn =~ tr/-/_/ && *{"$self->{PACKAGE}::f_$rtn"}{CODE} ||
				# Convert - into _ so it's more perl friendly.
      *{"$self->{PACKAGE}::f_$orig"}{CODE};
				# See if it's a known function.
    if( $code ) {
      $result = eval {		# Evaluate the function.
	local $_;		# Prevent really strange head-scratching errors.
	local $Mpp::makefile = $self; # Pass the function a reference to the makefile.
	&$code( $rest_of_line, $self, $makefile_line );
				# Call the function.
      };
      die $@ if $@;
    } else {
      die "$makefile_line: unknown function $rtn\n";
    }
  } elsif( $expr =~ s/^&(?=.)// ) { # & alone is a silly variable
    my( $cmd, @args ) = unquote_split_on_whitespace $expr;
    local $Mpp::Subs::rule = { MAKEFILE => $self, RULE_SOURCE => $makefile_line };
    local *OSTDOUT;		# TODO: convert to my $fh, when discontinuing 5.6.
    open OSTDOUT, ">&STDOUT" or die;
    my $temp = f_mktemp '', $self;
    open STDOUT, '>', $temp or die; # open '+>' screws up from 5.8.0 to 5.8.7 on some OS
    eval {
      local $_;
      if( defined &{$self->{PACKAGE} . "::c_$cmd"} ) { # Function from makefile?
	local $0 = $cmd;
	&{$self->{PACKAGE} . "::c_$0"}( @args );
      } elsif( defined &{"Mpp::Cmds::c_$cmd"} ) { # Builtin Function?
	local $0 = $cmd;
	&{"Mpp::Cmds::c_$0"}( @args );
      } else {
	run $cmd, @args;
      }
    };
    open STDOUT, ">&OSTDOUT";
    die $@ if $@;
    open my $fh, '<', $temp or die;
    local $/;
    $result = <$fh>;
    $result =~ s/\r?\n/ /g # Get rid of newlines.
      unless $Mpp::Subs::s_define;
    $result =~ s/\s+$//;	# Strip out trailing whitespace.
  } elsif( $expr =~ /^([^\s:#=]+):([^=]+)=([^=]+)$/ ) {
				# Substitution reference (e.g., 'x:%.o=%.c')?
    my $from = (0 <= index $2, '%') ? $2 : "%$2"; # Use the full GNU make style
    my $to = (0 <= index $3, '%') ? $3 : "%$3";
    $result = join ' ',
      Mpp::Text::pattern_substitution $from, $to,
	split_on_whitespace expand_variable( $self, $1, $makefile_line );
  } else {			# Must be a vanilla variable to expand.
    $result = &expand_variable;
  }

  if( !defined $result ) {
    warn "expression `$expr' expanded to an undefined value at `$makefile_line'.\n";
    '';
  } else {
    $result;
  }
}

#
# A variable allowing to intoduce special purpose Makefile variables that have
# a higher lookup priority than any other.  This is used by rules for target
# specific variables and by $(foreach ...) for the index variable.
# This must (typically temporarily) contain a hash reference with one or two
# elements:
# PRIVATE_VARS	A hash reference with the overriding values.
# VAR_REEXPAND	An optional hash reference with the reexpand flags for each
#		value.
#
our $private;

# Like $private, but contains (permanently) the reexpand flags (i.e. a hash
# ref with a VAR_REEXPAND field) for global variables.  The values themselves
# are however stored as normal variables in the Mpp::global:: package.
our $global;

our $rule_include; # undef unless reading an :include .d file. 1 before rule execution, 2 after.
#
# This is a helper routine which is used for expanding a simple variable expression.
# Arguments:
# a) The makefile.
# b) The variable name.
# c) The makefile line number (for error messages only).
# d) The mode is one of
#	false	simply expand the variable
#	1	return (expand_flag, unexpanded_value)
#	other	return whether the variable is defined
#
sub expand_variable {
  my( $self, $var, $makefile_line, $mode ) = @_; # Name the arguments.
  my $reexpand = 1;		# Assume it was an = variable, not a :=
				# variable.
				# Note that we actually do want to reexpand
				# variables gotten from the comand line (gcc
				# 2.95.2's build procedure depends on this) and
				# from the environment.

  my( $varref, $result );
  {				# This isn't a real loop; it merely defines
				# where "last" actually goes to.  Too bad
				# almost every variable storage level has a
				# different mechanism, so we can't just loop
				# through these:

# 1st attempt:
    if( exists $Mpp::Subs::perl_unfriendly_symbols{$var} ) { # Is it one of the 1-char
				# symbols like '$@' that conflict with perl
				# variables?  These can't be per target or global.
      if( ref $Mpp::Subs::perl_unfriendly_symbols{$var} ) {
	$result = eval { &{$Mpp::Subs::perl_unfriendly_symbols{$var}}() };
	$@ and die "$makefile_line: $@\n";
	if( 2 == length $var ) { # Variants like $(@D) or $(@F)
	  if( 'D' eq substr $var, 1 ) {
	    $result = f_dir_noslash $result;
	  } elsif( 'F' eq substr $var, 1 ) {
	    $result = f_notdir $result;
	  }
	}
      } else {
	$result = $Mpp::Subs::perl_unfriendly_symbols{$var};
      }
      $reexpand = 0;
      last;
    }

# 2nd attempt:
    if( $private ) {
      defined( $result = $private->{PRIVATE_VARS}{$var} ) and
				# Check for target-specific or foreach
				# variables.  This variable is set
				# up by the rule when it's expanding the
				# action and by f_foreach.
	$varref = \$private->{PRIVATE_VARS}{$var},
	$reexpand = $private,
	last;
    }

# 3rd attempt:
    defined( $result = ${"$self->{PACKAGE}::$var"} ) and
				# Get it from the makefile.
      $varref = \${"$self->{PACKAGE}::$var"},
      $reexpand = $self,
      last;

# 4th attempt:
    defined( $result = ${"Mpp::global::$var"} ) and
      $varref = \${"Mpp::global::$var"},
      $reexpand = $global,
      last;

# 5th attempt, no varref beyond here, because these things are not ;= assigned:
    defined( $result = $self->{COMMAND_LINE_VARS}{$var} ) ||
				# Try to get it from the command line.
      $Mpp::environment_override && defined( $result = $self->{ENVIRONMENT}{$var} )
				# Or from the environment·
      and last;

# 6th attempt:
    my $fn = "$self->{PACKAGE}::f_$var"; # Name of the function with no arguments?
				# Localizes $_; causes very weird errors if $_ is messed up.
    my $orig = $fn;
    $fn = $fn =~ tr/-/_/ && *{$fn}{CODE} || # Convert - to _ so it's more perl friendly.
      *{$orig}{CODE};
    if( defined $fn ) {	# Defined in the makefile?
      my $tmp = !$Mpp::environment_override && $self->{ENVIRONMENT}{$var};
      if( $tmp && $fn == *{"Mpp::Subs::f_$var"}{CODE} ) {
	$result = $tmp;
      } else {
	local $Mpp::makefile = $self; # Pass the function a reference to the makefile.
	$result = &$fn( '', $self, $makefile_line ) and
	  $reexpand = 0;	# It was a := variable.
      }
    }
    last if defined $result;	# Did we find it?

# 7th attempt if env not handled above:
    !$Mpp::environment_override && defined( $result = $self->{ENVIRONMENT}{$var} )
				# Or from the environment·
      and last;

    $reexpand = 0;		# Was not found.
  }				# "last" above breaks to here.

  ref $reexpand and
    my $reexpandref = $reexpand,
    $reexpand = $reexpand->{VAR_REEXPAND}{$var};

  if( !$mode ) {
    defined $result or $result = ''; # Variable not found--substitute blank.
    if( $reexpand ) {
      $result = expand_text $self, $result, $makefile_line;
				# Reexpand any variables inside.
      if( $reexpand == 2 ) {
	$$varref = $result;
	$reexpandref->{VAR_REEXPAND}{$var} = 0;
      }
    }
    $result;
  } elsif( $mode == 1 ) {
    ($reexpand, $result);	# Don't yet reexpand any variables inside.
  } else {
    defined $result and length $result; # GNU make regards variables set equal
				# to the empty string as undefined.
  }
}

my @root_makefiles = qw(RootMakeppfile RootMakeppfile.mk);

#
# Attempt to find a makefile in a directory.  Returns a fileinfo structure
# for the makefile if it found one, otherwise returns undef.
# Argument: the Mpp::File structure for the directory.
#
sub find_makefile_in {
  my $dir = dereference $_[0];	# Resolve a soft link on the directory.
				# This can be important if this is the first time
				# we've seen this directory.

  local $Mpp::implicitly_load_makefiles = 0;
				# Don't let this trigger a makefile load.

  for( @root_makefiles, qw(Makeppfile Makeppfile.mk), $_[1] ? () : qw(makefile Makefile) ) {
    my $trial_makefileinfo = file_info $_, $dir;
    return $trial_makefileinfo
      if Mpp::File::exists_or_can_be_built $trial_makefileinfo;
  }
}

#
# This subroutine is called when we have discovered a new directory and we
# want to load a makefile implicitly from it, if we haven't already.
#
# Argument: the Mpp::File structure for the relevant directory.
#
sub implicitly_load {
  $Mpp::implicitly_load_makefiles or return;
				# Don't do anything if we don't implicitly
				# load makefiles from directories.

  my $dirinfo = $_[0];

  exists($dirinfo->{MAKEINFO}) and return;
				# Already tried to load something.
  Mpp::File::is_writable( $dirinfo ) ||	# Directory already exists?
    !exists $dirinfo->{EXISTS} && is_or_will_be_dir( $dirinfo ) &&
    exists $dirinfo->{ALTERNATE_VERSIONS}
    or return;			# If the directory isn't writable, don't
				# try to load from it.	(Directories from
				# repositories will always be writable since
				# we're going to create them, except if there
				# is already an unwritable directory there.)
#
# See if this directory or any of its parents is marked for no implicit
# loading.
#
  for (my $pdirinfo = $dirinfo; $pdirinfo; $pdirinfo = $pdirinfo->{'..'} || '') {
    $pdirinfo->{NO_IMPLICIT_LOAD} and return;
  }

  eval { load($dirinfo, $dirinfo,
	      $global_command_line_vars,
	      '',
	      \@Mpp::makepp_include_path,
	      \%Mpp::global_ENV,
	      $Mpp::implicit_load_makeppfile_only) };
				# Try to load the makefile.
  $dirinfo->{MAKEINFO} ||= undef;
				# Remember that we tried to load something,
				# even if we failed.
  if ($@ &&			# Some error?
      $@ !~ /can\'t find a makefile in directory/) { # Unrecognized error?
    die $@;			# Resignal the error.
  }
}

=head2 cleanup_vars

Remove undefined variables from the environment and the command line.
Such variables come into existence when perl looks for them, but they need
to be ignored for comparing and/or setting them.

=cut

sub cleanup_vars {
  my $self = $_[0];
  for my $hash ( @$self{qw(ENVIRONMENT COMMAND_LINE_VARS)} ) {
    for my $key ( keys %$hash ) {
      delete $hash->{$key} unless defined $hash->{$key};
    }
  }
}

=head2 cd

Change to the makefile's directory.
A synonym of $self->{CWD}->chdir, but you don't have to know about the
existence of $self->{CWD}, which is supposedly a private implementation
detail.

=cut

sub cd {
  chdir( $_[0]{CWD} );
}

=head2 $makefile->setup_environment()

Sets %ENV. No mechanism for restoring the previous environment is provided.
This might do other similar things (I<e.g.> set the umask) in the future.

=cut
sub setup_environment {
  my $self = $_[0];

  # Make sure that the Mpp::Makefile is initialized in case it's not done loading
  # yet. This is allowed, but potentially dangerous because a rule could change
  # after it's executed.
  &initialize;

  &cleanup_vars;
  my ($exports, $environment) = @{$self}{qw(EXPORTS ENVIRONMENT)};

  %ENV = %$environment;		# Set the environment.
#
# Handle any exported variables.
#
  if ($exports) {		# Any exported variables?
    my ($var, $val);
    while (($var, $val) = each %$exports) {
      $ENV{$var} = $val;
    }
  }
}

#
# Look upwards for root makefile.  Return it only if it hasn't been returned
# or loaded before.  As a side effect caches the ROOT directory (or undef if
# not available).
#
sub find_root_makefile_upwards {
  my $cwd = $_[0];
  my( @path, $found ) = $cwd;
  push @path, $cwd = $cwd->{'..'} until file_exists $cwd;
				# Go up, as directory may not already exist.
  my $cwd_devid = (stat_array $cwd)->[Mpp::File::STAT_DEV];
				# Remember what device this is mounted on
				# so we can avoid crossing file system boundaries.
  until( exists $cwd->{ROOT} ) {
    for( @root_makefiles ) {
      my $finfo = file_info $_, $cwd;
      $found = $finfo, $cwd->{ROOT} = $cwd, last
	if exists $finfo->{ALTERNATE_VERSIONS} ||
	  file_exists $finfo;	# Found file in the path?  Don't
				# check can_be_built, since this is supposed
				# to be the first makefile we load, so no
				# other can give us the rule.  This avoids
				# going into load recursion.
    }
    if( $found && exists $found->{ALTERNATE_VERSIONS} && !file_exists $found ) {
				# RootMakeppfile to be imported first time.
				# Check downwards that this is no accident.
      my @subdirs = grep $_->{NAME} !~ /^\./, Mpp::Glob::find_real_subdirs $found->{'..'};
      while( @subdirs ) {
	for( @root_makefiles ) {
	  die "makepp: Must not have nested directories with a RootMakeppfile\n"
	    if file_exists file_info $_, $subdirs[-1];
	}
	push @subdirs, grep $_->{NAME} !~ /^\./, Mpp::Glob::find_real_subdirs pop @subdirs;
      }
    }
    last if $found or $cwd == $Mpp::File::root;
    push @path, $cwd = $cwd->{'..'}; # Look in all directories above us.

    undef( $cwd ), last unless
      $cwd && $cwd_devid && ((stat_array $cwd)->[Mpp::File::STAT_DEV] || 0) == $cwd_devid;
				# Remember what device this is mounted on.;
				# Don't cross device boundaries.  This is
				# intended to avoid trouble with automounters
				# or dead network file systems.
				# Win ActiveState 5.8.8 fails on stat '/'
  }
  $_->{ROOT} = $cwd && $cwd->{ROOT} for @path;
  $found;
}

=head2 load('makefile', $default_dir, $command_line_vars, $makecmdgoals, $include_path, $environment, $makeppfile_only)

Makes a new makefile object.  The argument is the makefile to load, or else
a directory that may contain the makefile.  Exits with die if no such
makefile exists, or if there is a fatal parse error.  Otherwise, returns
the Mpp::Makefile object.

If you do not specify the default directory, then directory containing the
makefile is assumed.

If the makefile has already been loaded, then this does not reload the
makefile; it returns the old makefile object.

$command_line_vars is a reference to a hash containing the names and values of
all variables which were specified on the command line.

$makecmdgoals is the value of $(MAKECMDGOALS) for this makefile.

include_path is an array of Mpp::File structures for directories that the
include statement should search.

$environment is a hash containing the environment for this particular
makefile.

$makeppfile_only is a flag that says only to look for a file called
F<Makeppfile> and not F<makefile> or F<Mpp::Makefile>.  This is used to avoid doing
implicit loads on subdirectories controlled by make (see the
--implicit-load-Makeppfile-only option).

If there is a target in the Makefile for the Makefile itself, the makefile is
remade and then reread.	 Mpp::Makefile::load does not return until the makefile
has been rebuilt.

=cut

my( $makepp_default_makefile, $makepp_builtin_rules );
sub load {
  my $minfo = ref( $_[0] ) ? $_[0] : &file_info;
				# Get the Mpp::File struct for the makefile.
  my( undef, $mdinfo, $command_line_vars, $makecmdgoals, $include_path, $env,
      $makeppfile_only, $autoload ) = @_; # Name the other arguments.
  my %this_ENV = %$env;		# Make a modifiable copy of the environment.
  delete @this_ENV{'MAKEPP_SOCKET', # Get rid of our special variables.
				# (This gets put back into the environment
				# later by Rule::execute, but we don't want
				# it here when we're making comparisons.)
		   'SHLVL',	# This variable gets incremented by the
				# shell and can cause unnecessary makefile
				# reloads.
		   'OLDPWD',	# Another variable that can cause unnecessary
				# reloads.
		   '_'};	# Last command executed by Shell, it too seems
				# to cause problems.
  local $Mpp::Subs::rule;	# Make sure that subroutine calls not
  				# associated with a rule do the right thing

  $global_command_line_vars ||= $command_line_vars;
				# If these are the top level variables,
				# remember them in case we have to load
				# other makefiles implicitly.

  my $is_dir = is_or_will_be_dir $minfo;
				# Is this a directory rather than a file?
  $mdinfo ||= $is_dir ?
    $minfo :			# Save pointer to the directory.
    $minfo->{'..'};		# Default directory is what contains the makefile.
  $mdinfo = dereference $mdinfo; # Resolve a soft link on the directory.

  $mdinfo->{MAKEINFO} ||= undef; # Indicate that we're trying to load a
				# makefile from this directory.
				# This prevents recursion with implicitly
				# loading a makefile.
  $minfo = Mpp::MAKEPP && find_makefile_in( $minfo, $makeppfile_only ) ||
				# Find a makefile.
#
# If there's no makefile, then load the default makefile as if it existed in
# that directory.
#
    ($makepp_default_makefile ||= path_file_info "$Mpp::datadir/makepp_default_makefile.mk")
    if $is_dir;
  if( grep { $_ eq $minfo->{NAME} } @root_makefiles ) {
    find_root_makefile_upwards $mdinfo->{'..'};
    die "makepp: Must not have nested directories with a RootMakeppfile\n" if $mdinfo->{'..'}{ROOT};
    $mdinfo->{ROOT} = $mdinfo;	# Nothing else to do as we're just loading it.
  } elsif( exists $mdinfo->{ROOT} ) { # Already checked for root makefile.  Else we
				# must be in a different tree, where we also
				# have a chance of finding a(nother) root
				# makefile.
  } else {			# Look upwards for root makefile.
    my $rootmf = find_root_makefile_upwards $mdinfo->{'..'};
    $mdinfo->{ROOT} = $mdinfo->{'..'}{ROOT};
    load( $rootmf, 0, @_[2..6] ) if Mpp::MAKEPP && $rootmf; # Load this one first.
  }

  my $mpackage;
  my $self = $mdinfo->{MAKEINFO};
  if( $self ) {			# Was there a previous makefile?
    my $var_changed;		# What actually changed to cause a reload.
    if ($self->{MAKEFILE} == $minfo) {
				# Attempt to reload the same makefile?
				# If the variables and include path are the
				# same, no need to reload.  Otherwise, we'll
				# have to reload.
      &cleanup_vars;
      $var_changed = hash_neq($command_line_vars, $self->{COMMAND_LINE_VARS}) ||
	hash_neq(\%this_ENV, $self->{ENVIRONMENT});
				# Did any variables change?
      unless ($var_changed) {
	"@$include_path" eq "@{$self->{INCLUDE_PATH}}" or
	  $var_changed = 'include path';
      }
      $var_changed or return $mdinfo->{MAKEINFO};
				# No need to reload the makefile--just reuse
				# what we've got.
    } elsif( ! $autoload ) {
#
# We're loading two makefiles for this directory.  This is disallowed because
# the phony targets of the two makefiles will get confused.
#
      die 'attempt to load two makefiles (' . absolute_filename( $mdinfo->{MAKEINFO}{MAKEFILE} ) .
	' and ' . absolute_filename( $minfo ) . ")
  with the same default directory.  This is not supported unless you add
  the --traditional-recursive-make option to the command line.\n";

    }
#
# We're reloading this makefile.  Clean out all the old definitions, and set
# up a few variables:
#
    delete $self->{INITIALIZED};
    unless($autoload) {
      $self->{ENVIRONMENT} = \%this_ENV; # Store the new environment.
      $self->{COMMAND_LINE_VARS} = $command_line_vars;
      $self->{INCLUDE_PATH} = [ @$include_path ];
      ++$self->{LOAD_IDX};	# Invalidate all the rules from the last time
				# we loaded this makefile.  (See code in
				# Mpp::File::set_rule.)
    }

    $mpackage = $self->{PACKAGE};
    if($autoload) {
      print "$Mpp::progname: Autoloading makefile `" . absolute_filename( $minfo ) . "'\n" unless $Mpp::quiet_flag;
      Mpp::log LOAD => $minfo, $mdinfo
	if $Mpp::log_level;
    } else {
      delete $Mpp::{$mpackage . '::'}; # Wipe the whole package.

      if( $Mpp::log_level || !$Mpp::quiet_flag ) {
	print "$Mpp::progname: Reloading makefile `" . absolute_filename( $minfo ) . "'\n" unless $Mpp::quiet_flag;
	Mpp::log LOAD_AGAIN => $minfo, $var_changed, $mdinfo
	  if $Mpp::log_level;
      }
    }
  } else {			# Loading a new makefile:
    if ($minfo->{NAME} eq 'makepp_default_makefile.mk') {
      Mpp::log LOAD_DEFAULT => $mdinfo
	if $Mpp::log_level;
    } else {
      print "$Mpp::progname: Loading makefile `" . absolute_filename( $minfo ) . "'\n" unless $Mpp::quiet_flag;
      Mpp::log LOAD => $minfo, $mdinfo
	if $Mpp::log_level;
    }

    $mpackage = 'makefile_' . $package_seed++;
				# Make a unique package to store variables and
				# functions from this makefile.

    $self = bless { MAKEFILE => $minfo,
		    PACKAGE => $mpackage,
		    CWD => $mdinfo,
		    COMMAND_LINE_VARS => $command_line_vars,
		    INCLUDE_PATH => [ @$include_path ],
		    ENVIRONMENT => \%this_ENV,
		    LOAD_IDX => 0 # First time this has been loaded.
		  };
				# Allocate our info structure.
  }
  $mdinfo->{MAKEINFO} = $self;	# Remember for later what the makefile is.

#
# Export all subroutines from the Mpp::Subs package into the given package, so
# the subroutines can be used directly.
#
  eval 'package ' . $mpackage . q{;
    use Mpp::Subs;
    *rule = \$Mpp::Subs::rule;	# Also pass in the $rule symbol.
    our $makefile = $self;	# Tell the makefile subroutines about it.
    our $MAKECMDGOALS = $makecmdgoals; # Set up this special variable.
    our $MAKEPP_VERSION = $Mpp::VERSION;
  };
  $mpackage .= '::';

#
# We used to fork here, load the makefile once, rebuild the makefile if
# necessary, and then finally load the makefile in the parent process.	This
# avoids polluting the Mpp::File hierarchy with old rules that don't exist in
# the up-to-date makefile.  It's a bit slow, however, and since we now allow
# makefiles to be reloaded and overwritten if the command line arguments or
# environment variables change, it seemed unnecessarily conservative to allow
# it to do it the old way.
#
  if ($minfo->{NAME} ne 'makepp_default_makefile.mk') {
    wait_for Mpp::build($minfo) and die "Failed to build ". absolute_filename( $minfo );
				# Build the makefile, using what rules we
				# know from outside the makefile.  This may
				# also load it from a repository.
    delete $minfo->{BUILD_HANDLE}; # Get rid of the build handle, so we avoid
				# the error message that we built the file
				# before we saw the rule.
  }

  chdir( $mdinfo );		# Get in the correct directory for wildcard
				# action routines.

  if( Mpp::MAKEPP ) {
#
# Build up the MAKEFLAGS variable:
#
    my $flags = Mpp::Text::join_with_protection
      map( '-I'.relative_filename( $_ ), @{$include_path}[1..@$include_path-1] ),
      map { /^makepp_/ ? () : "$_=$command_line_vars->{$_}" } keys %$command_line_vars;
    ${$mpackage . 'MAKEFLAGS'} = $Mpp::MAKEFLAGS .
      ($Mpp::MAKEFLAGS && $flags ? ' ' : '') .
      $flags;			# Set the variable.
    $self->{EXPORTS}{MAKEFLAGS} = 1; # Export it to the environment.
    if( defined $Mpp::MAKEPPFLAGS ) {
      ${$mpackage . 'MAKEPPFLAGS'} = ${$mpackage . 'MAKEFLAGS'};
      $self->{EXPORTS}{MAKEPPFLAGS} = 1; # Export it to the environment.
    }

#
# Read in the makefile, except in makeppreplay:
#
    if( $this_ENV{MAKEFILES} ) { # Supposed to pre-load some files?
      foreach( split ' ', $this_ENV{MAKEFILES} ) {
	my $finfo = file_info $_, $mdinfo;
	eval { read_makefile($self, $finfo) };
	warn "can't read ", absolute_filename( $finfo ), " (listed in \$MAKEFILES):\n$@"
	  if $@;
      }
    }
    read_makefile($self, $minfo); # Read this makefile (possibly again).
    unless( expand_variable $self, 'makepp_no_builtin' ) {
      $makepp_builtin_rules ||= path_file_info "$Mpp::datadir/makepp_builtin_rules.mk";
      read_makefile( $self, $makepp_builtin_rules );
      Mpp::log LOAD_INCL => $makepp_builtin_rules, $minfo
	if $Mpp::log_level;
    }
  }

#
# For variables which were assigned with =, we're supposed to reexpand them
# later.  However, if they don't have any $ in them, then they might as well
# have been assigned with :=, so pretend they were.  This should speed up
# expand_text() slightly.
#
  foreach my $varname (keys %{$self->{VAR_REEXPAND}}) {
    my $val = ${$mpackage . $varname};
    defined $val && $val =~ /\$/ or
      delete $self->{VAR_REEXPAND}{$varname};
  }

  initialize( $self );
  $self->{INITIALIZED} = 1;

#
# Now see if the makefile is up to date.  If it's not, we just wipe it out
# and reload.  This may leave some bogus rules lying around.  Oh well.
# This must be done after setting up the EXPORTS variables above, because
# makefile rebuilding might depend on that.
#
  if( Mpp::MAKEPP && $Mpp::remake_makefiles && # This often causes problems, so we provide
				# a way of turning it off.
      $minfo->{NAME} ne 'makepp_default_makefile.mk' ) {
    my $old_n_files = $Mpp::n_files_changed;
    # If there isn't a rule for the Makefile at this point, then it has already
    # been re-generated, or there isn't a rule to be found.  In the first case
    # we might fail to re-rebuild it, and either way we don't need to.
    if($minfo->{RULE}) {
      require Mpp::BuildCheck::target_newer; # Make sure the method is loaded.
      local $Mpp::default_build_check_method = $Mpp::BuildCheck::target_newer::target_newer;
				# Use the target_newer technique for rebuilding
				# makefiles, since makefiles are often modified
				# by programs like configure which aren't
				# under the control of make.
      wait_for Mpp::build($minfo) and # Try to rebuild the makefile.
	die "can't find or build " . absolute_filename( $minfo ) . "\n";
    }
    if ($old_n_files != $Mpp::n_files_changed) {
				# Did we change anything?
      $self->{ENVIRONMENT} = { I_rebuilt_it => 'FORCE RELOAD'};
				# Wipe out the environment, so we force a
				# reload.
      local $Mpp::remake_makefiles = 0; # Don't try to keep on remaking the
				# makefile.
      return &load;		# Call ourselves with the same arguments to
				# force rereading the makefile.
    }
  }

  if( my $fh = $self->{CWD}{DUMP_MAKEFILE} ) {
    # Dump the final variables too.
    print $fh "\n#Variables...\n#####\n";
    for my $var (keys %{$self->{PACKAGE} . '::'}) {
      my $val = ${$self->{PACKAGE} . "::$var"};
      if(defined($val) && !ref($val)) {
        $val =~ s/\n/\n /g;
        print $fh "$var=$val\n";
      }
    }
    print $fh "#####\n";
  }
  Mpp::log LOAD_END => $minfo
    if $Mpp::log_level;

  $minfo->{BUILD_HANDLE} ||= undef; # Remember not to rebuild it.
  $self;
}

#
# Initialize the EXPORTS, which could be needed before running any rules.
#
sub initialize {
  my $self = $_[0];
  unless($self->{INITIALIZED}) {
  #
  # Fetch the values of exported variables so we can quickly change the
  # environment when we have to execute a rule.  When the export statement was
  # seen, we put the names of the variables into a hash with a null value;
  # now replace that null value with the actual value.
  #
    if ($self->{EXPORTS}) {	# Are there any?
      foreach my $var (keys %{$self->{EXPORTS}}) {
        $self->{EXPORTS}{$var} = expand_variable $self, $var, absolute_filename $self->{MAKEFILE};
      }
    }

    # NOTE: Don't set INITIALIZED here, because even though we've done the
    # initialization, we'll have to do it again after the Makefile is
    # completely read in order to track any subsequent changes.
  }
}

#
# Predict the value of the specified environment variable.
#
sub get_env {
  my ($self, $var) = @_;
  $var or die;
  if ($self->{EXPORTS} && exists($self->{EXPORTS}{$var})) {
    unless($self->{INITIALIZED}) {
      # Even if it's already set, we still have to re-evaluate, because its
      # value could have changed since it was last set.
      $self->{EXPORTS}{$var} =
	expand_variable $self, $var, absolute_filename $self->{MAKEFILE};
    }
    return $self->{EXPORTS}{$var};
  }
  return $self->{ENVIRONMENT}{$var} if exists($self->{ENVIRONMENT}{$var});
  return;
}

sub assign {
  my( $self, $name, $type, $value, $override, $makefile_line, $sep, $private ) = @_;
  return $self if !$override and
    exists $self->{COMMAND_LINE_VARS}{$name} ||
    $Mpp::environment_override && exists $self->{ENVIRONMENT}{$name};
                                # Don't even evaluate variables whose
                                # definition is overridden on the command line.
                                # This allows a user to override buggy
                                # read-only makefiles.

  warn "MAKE redefined at `$makefile_line', recursive make won't work as expected\n"
    if $name eq 'MAKE';

  if( $type == ord '?' ) {
    if( expand_variable $self, $name, $makefile_line, 2 ) {
      return $self;		# Nothing to do for ?= if already defined
    } else {
      $type = 0;	# Else it's like a = assignment.
    }
  }

  my( $varref, $reexpandref, $reexpand );
  if( $private ) {		# Target specific?

    $varref = \$private->{PRIVATE_VARS}{$name};
    $reexpandref = $private;

  } else {

    my $var = "Mpp::global::$name";
    if( defined $$var ) {	# Newly or already global?
      $varref = \$$var;
      $reexpandref = $global;
    }

    substr $var, 0, 11, $self->{PACKAGE};
    if( !$varref || defined $$var ) { # Even if global, we might have a local.
      $varref = \$$var;
      $reexpandref = $self;
    }
  }

  if( !$type ) {	# Plain assignment?

    $$varref = $value;
    $reexpand = 1;		# Remember to expand this variable's contents
				# when it's invoked.

  } elsif( $type == ord ':' ) { # Immediate evaluation?

    $$varref = expand_text( $self, $value, $makefile_line );

  } elsif( $type == ord '+' ) { # Append?

    ($reexpand, $$varref) = expand_variable $self, $name, $makefile_line, 1;
    if( !defined $$varref ) {
      $$varref = '';
      $reexpand = 1;
    } elsif( length $$varref ) {
      $$varref .= defined $sep ? $sep : ' ';
    }
    $$varref .= $reexpand ?
      $value : # Was it a regular =?
      expand_text $self, $value, $makefile_line;
				# Expand the RHS if it was set with :=
				# previously.

  } elsif( $type == ord ';' ) { # Postponed evaluation once?

    $$varref = $value;
    $reexpand = 2;		# Remember to expand this variable's contents
				# when it's invoked, and then change to :=.

  } elsif( $type == ord '&' ) { # Prepend?

    ($reexpand, $$varref) = expand_variable $self, $name, $makefile_line, 1;
    if( !defined $$varref ) {
      $$varref = '';
      $reexpand = 1;
    } elsif( length $$varref ) {
      $$varref = (defined $sep ? $sep : ' ') . $$varref;
    }
    $$varref = ($reexpand ?
      $value : # Was it a regular =?
      expand_text $self, $value, $makefile_line) . $$varref;
				# Expand the RHS if it was set with :=
				# previously.

  } else {			# Must be a !=, run through shell to evaluate.

    $$varref = f_shell expand_text( $self, $value, $makefile_line ), $self, $makefile_line;

  }

  if( $reexpand ) {

    $reexpandref->{VAR_REEXPAND}{$name} = $reexpand;

  } elsif( ref $reexpandref && $reexpandref->{VAR_REEXPAND} ) {

    delete $reexpandref->{VAR_REEXPAND}{$name};
    delete $reexpandref->{VAR_REEXPAND} if !%{$reexpandref->{VAR_REEXPAND}};

  }
}

#
# Parse a potential assignment statement.  Arguments:
# a) The makefile.
# b) The makefile line number (for error messages).
#  ) $_ is the implicit argument containing the assignment.
# c) The index where the '=' was found in $_.
#
# Returns true if this is actually an assignment, false otherwise.
#
sub parse_assignment {
  my( $self, $makefile_line ) = @_; # Name the arguments.
  my $var_name = substr $_, 0, $_[2];
  my $var_value = substr $_, $_[2] + 1;

  $var_name =~ s/([+&;:?!])$//;
  my $type = !$1 ? 0 : ord $1;
				# Pull off the character before the equals
				# sign if it's part of the assignment token.

  $var_name = expand_text($self, $var_name, $makefile_line);
				# Make sure we can handle indirect assignments
				# like x$(var:y=z) = value.
  $var_name =~ s/^\s+//;	# Strip leading/trailing whitespace.
  $var_name =~ s/\s+$//;
  $var_value =~ s/^\s+//;
  $var_value =~ s/\s+$//;

  my @list;
  if( $var_name =~ /:/ and (@list = split_on_colon $var_name) > 1 ) {
				# If there's a : on the LHS, it's probably a
				# target-specific variable assignment.
    $c_preprocess || @list > 2 and return undef; # Not a valid target-specific assignment.
#
# It's a target-specific assignment, like this:
#   target1 target2: VAR = val
# or
#   target1 target2: VAR := val
# or
#   target1 target2: VAR += val
#
    $var_name = $list[1];
    $var_name =~ s/^\s+//;	# Strip leading whitespace (again).
  }

  my $override = $var_name =~ s/^override\s+//;

  if( @list ) {			# It's a target-specific assignment.
    my $targets = $list[0];	# Get the targets for which this variable
				# applies.o
    $targets =~ tr/%/*/;	# Convert % wildcard to normal filename wildcard.
    cd( $self );		# Make sure we're in the right directory
				# to expand the wildcard.
				# Can't just &cd; because for some reason Perl
				# up to 5.6.2 has clobbered @_.
    wildcard_action unquote_split_on_whitespace( $targets ),
      sub {			# This subroutine is called for every file
				# that matches the wildcard.
	local $private = $_[0];	# Prior PRIVATE_VARS for +=, and for storing new value.
	assign $self, $var_name, $type, $var_value, $override, $makefile_line, undef, $private;
      };
  } else {
#
# Not a target-specific assignment:
#
    if( $var_name =~ s/^export\s+// ) {
      s_export $var_name, $self, $makefile_line;
    } elsif( !$c_preprocess && $var_name =~ s/^global\s+// ) {
      s_global $var_name, $self, $makefile_line;
    } elsif( $var_name =~ s/^define\s+// ) {
      die "Trailing cruft after define statement at `$makefile_line'\n" if length $var_value;
      s_define $var_name, $self, $makefile_line, $type, $override;
      return $self;
    } elsif( $var_name =~ /[\s:#]/ ) { # More than one word on the LHS
				# implies it's not an assignment.
      return undef;
    }

    assign $self, $var_name, $type, $var_value, $override, $makefile_line;
  }

  $self;			# Return a true value.
}

#
# Parse a rule definition.  Arguments:
# a) The makefile.
# b) The line in the makefile (for error messages).
# c) Whether this is a double colon rule.
# d) The target string.
# e) The dependency string.
# f) Any other : modifiers that were present on the line after the
#    dependency string.
#
sub parse_rule {
  my ($self, $makefile_line, $makefile_line_dir, $is_double_colon, $target_string, @after_colon) = @_;
				# Name the arguments.

  local $Mpp::implicitly_load_makefiles = 0 if $self->{RECURSIVE_MAKE};
				# Turn off implicit makefile loading if there
				# is an invocation of recursive make in this
				# file.	 (This is not passed to the wildcard
				# action routine.)

  my $target_whitespace_len;
  $target_whitespace_len = Mpp::Text::strip_indentation for $target_string;
				# Strip out leading whitespace in the target.
  my $first_action_indent;
  my $last_line_was_blank;
  my $action = '';		# No actions seen yet.
#
# Unfortunately, due to some bozo design, the first line of a rule
# may be on the same line as the dependencies if it is separated by
# a semicolon, like this:
#
# x.o: x.c; @echo this is a stupid syntax
#	$(CC) $< -o $@
#
  my $idx = index_ignoring_quotes($after_colon[-1], ';');
  if ($idx >= 0) {		# Do we have this abhorrent syntax?
    $action = substr($after_colon[-1], $idx+1);
    substr( $after_colon[-1], $idx ) = '';
    $action =~ s/^\s+//;	# Strip out any leading space.	If the action
  }				# is entirely blank (as happens in some
				# makefiles), this will eliminate it.

#
# Get all the modifiers, and the actions for the rule (if any).
#
  local $_;

  while (defined($_ = read_makefile_line_stripped(1))) {
				# Get the next line.
    my $whitespace_len = Mpp::Text::strip_indentation;
    if( /^$/ or /^#/ ) {
      $last_line_was_blank = 1 if !$whitespace_len or /^$/;
				# Blank line or comment at right margin?
      next;			# Skip the blank or commented out lines.
    }
    $whitespace_len or last;	# If there wasn't any leading white-
				# space, then this is the first line of
				# the next rule or assignment.

#
# Note that we have to be able to handle weird indentation schemes.  Make
# requires that all rules begin with a tab character.  We don't do this
# since there's no way visually to tell a tab from 8 spaces, but we do have
# to properly parse things like this:
#
# ifneq ($X,y)
#   target: dependencies
#	actions
#
#   ifneq ($Y,y)
#     X = 3
#   endif
#   target: dependencies
# endif
#
# target1: dependencies
# ifneq ($Z,y)
#	action1
# else
#	action2
# endif
#
# Note that the ifneq/else/endif lines are never seen by this function since
# they are handled by read_makefile_line_stripped.
#
# So our rules are a bit complicated.  Here is the current set of rules:
# 1) If the line is of the same indentation as the target or less, it ends
#    the action lines.
# 2) If the line is more indented than the target line, but less indented
#    than any previous action lines, it ends the rule (unless it's indented
#    by more than 8 spaces--necessary for ugly backward compatibility).
# 3) If we have seen a blank line, or a comment line that begins at the
#    right margin, then an action line must be indented at least 8 spaces
#    (one hardware tab).  This is to solve most problems like this:
#
#	 all: xyz pdq
#
#	 ifdef something
#	  X = 3
#	 endif
#
#    Usually people put enough whitespace in their makefiles so this works.
#
    if ($whitespace_len < 8 &&
	($whitespace_len <= $target_whitespace_len ||
	 defined($first_action_indent) && $whitespace_len < $first_action_indent ||
	 $last_line_was_blank) ||
	($whitespace_len >= 8 &&
	 $whitespace_len <= $target_whitespace_len)) {
      substr $_, 0, 0, ' ' x $target_whitespace_len;
				# Put the whitespace back (in case it's the
				# next target).
      last;			# We've found the end of this rule.
    }
    if (/^:\s*((?:build[-_]?c(?:ache|heck)|dispatch|env(?:ironment)?|foreach|multiple[-_]?rules[-_]?ok|parser|s(?:ignature|canner|martscan)|quickscan|last[-_]?chance)\b.*)/ ) {
				# A colon modifier?
      push @after_colon, $1;
      if( (my $i = index_ignoring_quotes $after_colon[-1], '#') > 0 ) {
	substr( $after_colon[-1], $i ) = ''
      }
    } else {			# Not a colon modifier?
      $action .= $_;		# Must be an action for the rule.
    }

    $first_action_indent ||= $whitespace_len; # If this was the first line,
				# remember its indentation.
    $last_line_was_blank = 0;	# This line was not blank.
  }

  unshift @hold_lines, $_ if $_; # We read too far, so put this line back.

#
# Pull off the : modifiers.
#
  my( $foreach, $signature, $build_check, $build_cache, $have_build_cache );
  my( $lexer, $parser );
  my $conditional_scanning;
  my $multiple_rules_ok;
  my $last_chance_rule;
  my $dispatch;
  my $env_dep_str;
  my $include;

  while (@after_colon > 1) {	# Anything left?
    if ($after_colon[-1] =~ /^\s*foreach\s+(.*?)\s*$/) {
      $foreach and die "$makefile_line: multiple :foreach clauses\n";
      $foreach = expand_text $self, $1, $makefile_line;
    } elsif ($after_colon[-1] =~ /^\s*build[-_]?cache\s+(.+)/) {
                                # Specify a local build cache for this rule?
      $build_cache and die "$makefile_line: multiple :build_cache clauses\n";
      $build_cache = expand_text $self, $1, $makefile_line;
      if( $build_cache eq 'none' ) {
        $build_cache = undef;   # Turn off the build cache mechanism.
      } else {
        require Mpp::BuildCache;
        $build_cache = new Mpp::BuildCache( absolute_filename file_info $build_cache, $self->{CWD} );
      }
      $have_build_cache = 1;    # Remember that we have a build cache.
    } elsif ($after_colon[-1] =~ /^\s*build[-_]?check\s+(.*?)\s*$/) { # Build check class?
      $build_check and die "$makefile_line: multiple :build_check clauses\n";
      my $name = expand_text $self, $1, $makefile_line;
      $build_check = eval "use Mpp::BuildCheck::$name; \$Mpp::BuildCheck::${name}::$name" || # Try to load the method.
	eval "use BuildCheck::$name; \$BuildCheck::${name}::$name"; # TODO: provisional
      defined $build_check or
        die "$makefile_line: invalid build_check method $name\n";
    } elsif ($after_colon[-1] =~ /^\s*signature\s+(.*?)\s*$/) { # Specify signature class?
      $signature and die "$makefile_line: multiple :signature clauses\n";
      my $name = expand_text $self, $1, $makefile_line;
      $signature = eval "use Mpp::Signature::$name; \$Mpp::Signature::${name}::$name" ||
	eval "use Signature::$name; \$Signature::${name}::$name"; # TODO: provisional
      unless( defined $signature ) {
# For backward compatibility, accept build check methods as a name to
# :signature as well.
        $build_check = eval "use Mpp::BuildCheck::$name; \$Mpp::BuildCheck::${name}::$name" ||
	  eval "use BuildCheck::$name; \$BuildCheck::${name}::$name"; # TODO: provisional
        if( defined $build_check ) {
	  warn "$makefile_line: requesting build check method $name via :signature is deprecated.\n";
        } else {
          die "$makefile_line: invalid signature class $name\n";
        }
      }
    } elsif ($after_colon[-1] =~ /^\s*scanner\s+([-\w]+)/) { # Specify scanner class?
      warn "$makefile_line: rule clause :scanner deprecated, please use :parser\n";
      $lexer and die "$makefile_line: multiple :scanner clauses\n";
      my $scanner_name = expand_text $self, $1, $makefile_line;
      $scanner_name =~ tr/-/_/;
      $lexer = *{"$self->{PACKAGE}::parser_$scanner_name"}{CODE};
      unless(defined $lexer) {
        my $scanref = *{"$self->{PACKAGE}::scanner_$scanner_name"}{CODE};
        defined($scanref) or
          die "$makefile_line: invalid scanner $scanner_name\n";
        $lexer = sub {
          require Mpp::ActionParser::Legacy;
          Mpp::ActionParser::Legacy->new($scanref);
        };
      }
    } elsif ($after_colon[-1] =~ /\s*(?:smart()|quick)[-_]?scan/) {
      $conditional_scanning = defined $1;
    } elsif ($after_colon[-1] =~ /^\s*(?:command[-_]?)?parser\s+(.*?)\s*$/) {
      $parser and die "$makefile_line: multiple :command-parser clauses\n";
      $parser = unquote expand_text $self, $1, $makefile_line;
      $parser =~ tr/-/_/;
      $parser =
	*{"$self->{PACKAGE}::p_$parser"}{CODE} ||
	*{"$parser\::factory"}{CODE} ||
	*{"Mpp::CommandParser::$parser\::factory"}{CODE} ||
	die "$makefile_line: invalid command parser $parser\n";
    } elsif ($after_colon[-1] =~ /^\s*multiple[-_]?rules[-_]?ok/) {
      # This is an ugly hack to solve an unusual problem, and it shouldn't
      # be used by the general public.  The reason for it is that when you
      # have a directory with a makefile that needs to read lots of generated
      # files in order to compute the buildable targets, you sometimes would
      # rather save time by only computing the targets that are needed by the
      # local directory, and have the rules for building those targets defined
      # in the local makefile.  This will cause multiple rules to be defined
      # for the same target when multiple local directories need the same
      # file in the central area.  We want to supress warnings in that case.
      # A better long-term solution is to generalize Mpp::File::get_rule to
      # be able to obtain rules through means other than reading a makefile
      # (so that buildable targets can be computed lazily), but that would
      # require a significant re-design of makepp.
      $multiple_rules_ok = 1;
    } elsif ($after_colon[-1] =~ /^\s*env(?:ironment)?\s+(.*?)\s*$/) {
      if($env_dep_str) {
        $env_dep_str .= " $1";
      } else {
        $env_dep_str = $1;
      }
    } elsif ($after_colon[-1] =~ /^\s*dispatch\s+(.*?)\s*$/) {
      $dispatch = $1;
    } elsif ($after_colon[-1] =~ /^\s*last[-_]?chance/) {
      $last_chance_rule = 1;
    } elsif ($after_colon[-1] =~ /^\s*include\s+(.*?)\s*$/) {
      $include = $1;
    } else {			# Something we don't recognize?
      last;
    }
    pop @after_colon;
  }

#
# Now process the pieces of the rule.  We recognize several different kinds
# of rules:
#
# 1) .c.o:			# The old-style suffix rule.
# 2) %.o : %.c			# GNU make's pattern rules.
# 3) a.o b.o c.o : %.o : %.c	# GNU make's static pattern rules.
# 4) %.o : %.c :foreach abc.c def.c # Our static pattern rule.
# 5) $(patsubst %.c, %.o, $(foreach)) : $(foreach) :foreach *.c
#
# The first four forms are often more convenient to type, but they all
# get converted into the fifth form for internal use because it is the
# most powerful.  (Note that additional dependencies, possibly depending on
# $<, may be added to the fourth form.)
#
  my $expanded_target_string = eval { expand_text($self, $target_string, $makefile_line) };
  $expanded_target_string = $target_string if $@; # In case $(foreach) is there
				# Expand the target string now.	 We reexpand
				# it later so that it works properly if it
				# contains a $(foreach).
#
# First check for an old-style suffix rule and convert this into a GNU make
# pattern rule (type 2).
#
  if ($expanded_target_string =~ /^\s*\.([-+\w]+)\.([-+\w]+)\s*$/) {
				# One of the old suffix rules?
    $expanded_target_string = $target_string = "%.$2";
				# Convert it to a new-style pattern rule.
    $after_colon[0] = "%.$1 $after_colon[0]";
  }

#
# Convert GNU make's static pattern rules into something we like better.
# If the rule was
#    a.o b.o c.o : %.o : %.c
# then we treat it as if it were written:
#    $(foreach) : $(patsubst %.o, %.c, $(foreach)) : foreach a.o b.o c.o
#
  if (@after_colon == 2) {
    $foreach and die "$makefile_line: :foreach and GNU static pattern rule are incompatible\n";
    $foreach = $target_string;
    $after_colon[1] =~ /%/ && $after_colon[0] =~ /%/ or
      die "$makefile_line: no pattern in static pattern rule\n";
    (@after_colon) = "\$(filesubst $after_colon[0], $after_colon[1], \$(foreach))";
    $target_string = '$(foreach)';
  }

  @after_colon == 1 or die "$makefile_line: extra `:'\n";
				# At this point, the only thing we haven't
				# interpreted after the colon should be the
				# dependency string.
  my @deps = split_on_whitespace($after_colon[0]); # Separate the dependencies.
#
# Handle GNU make's regular pattern rules.  We convert a rule like
#   %.o: %.c
# into this:
#   $(filesubst %.c, %.o, $(foreach)) : $(foreach) : foreach **/*.c
#
  my $pattern_dep = 0;
  my $target_pattern = index_ignoring_quotes( $expanded_target_string, '%' ) >= 0;
  if( $target_pattern ) { # Pattern rule?
    # find the first element of @deps that contains a pattern character, '%'
    for my $dep ( @deps ) {
      if( index_ignoring_quotes( $dep, '%' ) < 0 ) { ++$pattern_dep }
      else { last }
    }
    if ($pattern_dep < @deps) {  # does such an element exist?
      unless ($foreach) { # No foreach explicitly specified?
        $foreach = $deps[$pattern_dep]; # Add one, making wildcard from first pattern dep.
        if( expand_variable( $self, 'makepp_percent_subdirs', $makefile_line ) ) {
          # % searches subdirs?
          $foreach =~ s@^%@**/*@ or # Convert leading % to **/*.
            $foreach =~ tr/%/*/; # Convert nested % to just a *.
        } else {
          $foreach =~ tr/%/*/;	# Convert percent to a wildcard.
        }
      }
    } else {
      die "$makefile_line: target has % wildcard but no % dependencies. This is currently\nnot supported, unless '--last-chance-rules' or ':last_chance' is specified.\n" unless $Mpp::last_chance_rules || $last_chance_rule;
    }
  }

  my $handle_include = defined $include && sub {
    my $rule = $_[0];
    my $include = expand_text $self, $_[1], $makefile_line;
    $rule->{TARGET_STRING} .= " $include"; # So count of built files is correct and mppc cleans it.
    $rule->{INCLUDE} = $include = file_info $include, $self->{CWD};
    Mpp::File::set_rule $include, $rule; # I don't expect anything else to depend on this, just in case.
    unless( $Mpp::force_rescan ) {
      Mpp::Repository::get( $include, $include->{ALTERNATE_VERSIONS}[0] )
				# Get first one as we can't precalculate its signature
	if $include->{ALTERNATE_VERSIONS};
      if( Mpp::File::have_read_permission $include ) {
	if( Mpp::is_perl_5_6 ? $Mpp::has_md5_signatures : 1 ) {
	  require Mpp::Signature::md5;
	  $rule->{INCLUDE_MD5} = Mpp::Signature::md5::signature( undef, $include );
				# remember it for checking if content changed
	}
	$rule->{PARSER} = \&Mpp::Subs::p_none;
	Mpp::log LOAD_INCL => $include, $makefile_line
	  if $Mpp::log_level;
	local $rule_include = 1;
	read_makefile( $self, $include );
      }
    }
  };

  if ($foreach) {		# Is there a foreach clause?
    die "$makefile_line: Combining \":foreach\" and \":last_chance\" is not supported.\n" if $last_chance_rule;
###### TODO: This needs to handle rules with no actions here, as well as
###### below where there's no :foreach clause.

#
# Handle our static pattern rule, with the % modifiers:
#
    if( $target_pattern ) { # Pattern rule?

      $target_string = "\$(filesubst $deps[$pattern_dep], $target_string, \$(foreach))";
      for( @deps[$pattern_dep+1..$#deps] ) { # Handle any extra dependencies:
	index_ignoring_quotes( $_, '%' ) >= 0 and
	  $_ = "\$(filesubst $deps[$pattern_dep], $_, \$(foreach))";
      }
      $include = "\$(filesubst $deps[$pattern_dep], $include, \$(foreach))"
	if $include && index_ignoring_quotes( $include, '%' ) >= 0;
      $deps[$pattern_dep] = '$(foreach)';	# This had better match the wildcard specified
				# in the foreach clause.  I don't know of
				# any way to check that.
      $after_colon[0] = "@deps";
    }
    &cd;			# Make sure we're in the correct directory,
				# or everything will be all messed up.

    wildcard_action unquote_split_on_whitespace( expand_text( $self, $foreach, $makefile_line )),
    sub {
#
# This subroutine is called once for each file that matches the foreach clause.
#
      my ($finfo, $was_wildcard_flag) = @_;
				# Get the arguments.

      return if exists $finfo->{PATTERN_RULES} # Don't keep on applying same rule.
	and grep $_ eq $makefile_line_dir, @{$finfo->{PATTERN_RULES}};
      # Note, we could say 3 < grep ... to allow a certain depth of rules
      # applied to their own output, e.g. '%a: %' to produce *aaa, but doing
      # it here would only apply to files discovered after the rule.
      # Additionally set_rule via wildcard_action would have to push to
      # WILDCARD_ROUTINES before looping over existing files.  That can't
      # currently be done because then we'd have the same rule twice, giving a
      # warning.

      local $Mpp::implicitly_load_makefiles = 0
	if $Mpp::implicitly_load_makefiles && $self->{RECURSIVE_MAKE};
				# Turn off implicit makefile loading if there
				# is an invocation of recursive make in this
				# file.	 (This is not passed to the wildcard.)

      my $rule = new Mpp::Rule($target_string, $after_colon[0], $action, $self, $makefile_line_dir);
      $rule->{MULTIPLE_RULES_OK} = 1 if $multiple_rules_ok;
      $rule->{DISPATCH} = $dispatch if $dispatch;
      $rule->{ENV_DEPENDENCY_STRING} = $env_dep_str if $env_dep_str;
				# Make the rule.
      local $Mpp::Subs::rule = $rule; # Put it so $(foreach) can properly expand.
      $self->{DEFAULT_SIGNATURE_METHOD} and
	$rule->set_signature_method_default($self->{DEFAULT_SIGNATURE_METHOD});
				# Get the signature method from the signature
				# statement.
      $self->{DEFAULT_BUILD_CHECK_METHOD} and
        $rule->set_build_check_method_default($self->{DEFAULT_BUILD_CHECK_METHOD});
      $signature and $rule->set_signature_method($signature);
				# Override that with the method from the
				# :signature clause, if any.
      $build_check and $rule->set_build_check_method($build_check);
                                # Set the build check method too.
      $have_build_cache and $rule->set_build_cache($build_cache);
                                # If we have a build cache, set it too.
      $lexer and $rule->{LEXER} = $lexer;
      $parser and $rule->{PARSER} = $parser;
      defined($conditional_scanning) and
        $rule->{CONDITIONAL_SCANNING} = $conditional_scanning;
      $rule->{FOREACH} = $finfo; # Remember what to expand $(FOREACH) as.
      $rule->{PATTERN_RULES} = exists $finfo->{PATTERN_RULES} ?
	[$makefile_line_dir, @{$finfo->{PATTERN_RULES}}] : [$makefile_line_dir]
	if $was_wildcard_flag;	# Mark it as a pattern rule if it was actually
				# done with a wildcard.

      my @targets = split_on_whitespace(expand_text($self, $target_string, $makefile_line));
				# Get the targets for this rule.

      foreach (@targets) {
	my $tinfo = file_info unquote(), $self->{CWD}; # Access the target object.
	Mpp::File::set_rule $tinfo, $rule; # Update its rule.  This will be ignored if
				# it is overriding something we shouldn't
				# override.
	$was_wildcard_flag or	# If there was no wildcard involved, this is
				# a candidate for the first target in the file.
	  $self->{FIRST_TARGET} ||= $tinfo;
				# Remember what the first target is, in case
				# no target was specified on the command
				# line.
      }
      &$handle_include( $rule, $include ) if $include;
    };				# End subroutine called on every file that
				# matches the wildcard.
  } elsif(!defined $foreach) {	# it's not a foreach rule
#
# This rule is not a pattern rule.  If there is an action, then it's
# a non-pattern rule; otherwise, we're just adding extra dependencies to
# certain files.
#
    my $expanded_target = expand_text($self, $target_string, $makefile_line);
    # { balance brackets for the following RE:
    $expanded_target =~ /\$[({]foreach\b[^})]*[})]/ and
      return;                   # $(foreach) that couldn't expand.
    my @targets = split_on_whitespace($expanded_target);
				# Get the list of targets.

    if( length $action ) {	# Is this actually a rule?
      unless( case_sensitive_filenames ) {
	tr/A-Z/a-z/ for @targets;
      }
#
# If the action string mentions $@, then (for backward compatibility with
# bozo make) we assume that the command must be executed once for each
# target.  This is used frequently in makefiles (especially
# those generated by automake).	 For example,
#
# all-recursive install-data-recursive [other targets] :
#	for dir in $(SUBDIRS); do cd $dir; $(MAKE) $@; cd ..; done
#
# If there is no mention of $@, then we assume that the same rule makes
# all of the dependencies at once.
#
# y.tab.c y.tab.h: parser.y
#	yacc -d $<
#
      my @target_exprs = ($expanded_target); # Assume only one target.

      if (($is_double_colon ||	 # Obsolete syntax?
	   $action =~ /\$\@/) && # Does it include the old $@ target?
	  $action !~ /\$[({](?:outputs|targets)[)}]/) {
				# And it doesn't include something that refers
				# to all targets at once?
	@target_exprs = @targets; # Apply rule independently to each target.
      }

      my $generate_rule = sub {
	my ($tstring) = @_;
	my $rule = new Mpp::Rule($tstring, $after_colon[0], $action, $self, $makefile_line_dir);
        $rule->{MULTIPLE_RULES_OK} = 1 if $multiple_rules_ok;
        $rule->{DISPATCH} = $dispatch if $dispatch;
        $rule->{ENV_DEPENDENCY_STRING} = $env_dep_str if $env_dep_str;

	$self->{DEFAULT_SIGNATURE_METHOD} and
	  $rule->set_signature_method_default($self->{DEFAULT_SIGNATURE_METHOD});
				# Get the signature method from the signature
				# statement.
        $self->{DEFAULT_BUILD_CHECK_METHOD} and
          $rule->set_build_check_method_default($self->{DEFAULT_BUILD_CHECK_METHOD});
	$signature and $rule->set_signature_method($signature);
        $build_check and $rule->set_build_check_method($build_check);
        $have_build_cache and $rule->set_build_cache($build_cache);
	$lexer and $rule->{LEXER} = $lexer;
	$parser and $rule->{PARSER} = $parser;
        defined($conditional_scanning) and
          $rule->{CONDITIONAL_SCANNING} = $conditional_scanning;
	for( split_on_whitespace $tstring ) {
	  my $tinfo = file_info unquote(), $self->{CWD}; # Access the target object.
	  if( $is_double_colon && $tinfo->{RULE} && # Append to previous rule?
	      $tinfo->{RULE}{LOAD_IDX} == $rule->{LOAD_IDX} ) {
				# Other rule for same target is not just from
				# loading the same makefile twice?
	    $tinfo->{RULE}->append($rule); # Append the dependency list and the
				# build commands.
	  } else {
	    Mpp::File::set_rule($tinfo, $rule); # Update its rule.
	  }
	  $self->{FIRST_TARGET} ||= $tinfo;
				# Remember what the first target is, in case
				# no target was specified on the command
				# line.
	}
	$rule;
      };

      my $warned_non_wild;
      foreach my $tstring (@target_exprs) {
      	# If it is an open-ended ":last_chance" rule, then we need to
	# set a trigger to generate the rules on demand.  Otherwise, it's
	# just a single rule that we can generate now.
      	if(index_ignoring_quotes($tstring, '%') >= 0) {
	  die "Can't use :last_chance and :include together\n" if $include; # TODO: figure out how to handle %.d
	  my ($pct_re, $pct_glob) = expand_variable( $self, 'makepp_percent_subdirs', $makefile_line ) ?
	    ('.*', '**/*') :
	    ('[^/]*', '*');
	  my @wild_targets;
	  foreach (split_on_whitespace($tstring)) {
	    if( index_ignoring_quotes($_, '%') >= 0 ) {
	      push @wild_targets, unquote;
	    } else {
	      warn "$makefile_line: Because this is a :last_chance rule, it might not get found for non-pattern targets (e.g. \"$_\")." unless exists file_info($_, $self->{CWD})->{IS_TEMP} || $warned_non_wild++;
	    }
	  }
	  ++$Mpp::File::n_last_chance_rules; # Turn off an optimization
	  # Register the globs for making rules on demand.
	  needed_wildcard_action map {
	    my $x = $_; $x =~ s/%/$pct_glob/g; $x
	  } @wild_targets,
	  sub {
	    my ($finfo) = @_;
	    my $rel_fname = $finfo->relative_filename($self->{CWD});
	    # Look for the first target pattern that matches.
	    my $found;
	    TARGET: for my $pattern (@wild_targets) {
	      my $re = quotemeta($pattern);
	      $re =~ s|\\%|($pct_re)| or die "makepp internal error: Where did the percent go?";
	      if(my @matches = ($rel_fname =~ /^$re$/)) {
	      	# Use the matched subexpressions from the matching target
		# pattern to determine the actual target list for this instance
		# of the rule, and generate it.
		&$generate_rule(
		  Mpp::Text::join_with_protection(
		    map {
		      my @m = @matches;
		      $_ = unquote;
		      s/\%/@m ? shift(@m) : die "$makefile_line: Not enough wildcards in target `$pattern' (matching filename `$rel_fname') to resolve other target `$_'.\n"/eg;
		      $_
		    } split_on_whitespace($tstring)
		  )
		);
		++$found;
		last TARGET;
	      }
	    }
	    $found or die "makepp internal error: no matching target patterns";
	  };
	} elsif( $include ) {
	  &$handle_include( &$generate_rule( $tstring ), $include );
	} else {
	  &$generate_rule( $tstring );
	}
      }
    } else {
#
# We're just adding a dependency to this target, like this:
#   target : additional-dependency
#
      if( @targets == 1 && ord( $targets[0] ) == ord '.' ) {
				# Check for some special targets.
	return if exists $ignored_targets{$targets[0]};
	if( $targets[0] eq '.PHONY' ) {
				# Mark other targets as phony?
	  undef file_info( unquote(), $self->{CWD} )->{IS_PHONY} # Mark as phony.
	    for split_on_whitespace expand_text $self, $after_colon[0], $makefile_line;
	  return;
	}
	if( $targets[0] eq '.SUFFIXES' ) {
				# Control the default rules?
	  if( $after_colon[0] !~ /\S/ ) { # Turn off all suffixes?
	    ${$self->{PACKAGE} . '::makepp_no_builtin'} = 1;
				# Suppress loading of all builtin rules.
	  }
	  return;
	}
      }

      foreach (@targets) {
	my $tinfo = file_info unquote(), $self->{CWD};
	Mpp::File::set_additional_dependencies($tinfo, $after_colon[0], $self, $makefile_line);
	$self->{FIRST_TARGET} ||= $tinfo;
				# Remember what the first target is, in case
				# no target was specified on the command
				# line.

      }
    }
  }				# End if not a pattern rule.
}

#
# Actually read a makefile.
#
# Arguments:
# a) The makefile structure to fill out (usually set up by Mpp::Makefile::load).
# b) The Mpp::File structure for the makefile.
#
our $last_conditional_start;
sub read_makefile {
  my ($self, $minfo) = @_;

  local $_;			# Don't mess up caller's $_.

  local $makefile = $self;	# Pass this into the routines that
				# handle ifeq/ifneq.

  local $makefile_name = absolute_filename( $minfo );
				# Get the name of the file (and pass this
				# to all subroutines we call).
  local $makefile_directory =
    $minfo->{'..'} == $self->{CWD} ?
      '' :
      # If the makefile's parent directory isn't the directory to which the
      # makefile (which could be an include file) pertains, then print the
      # directory following the line number. This avoids confusing diagnostics
      # when rules from the same include file are picked up in different
      # directories.
      # TODO:
      '(' . relative_filename($self->{CWD},$minfo->{'..'}) . ')';

  local $makefile_contents;
  {
    local $/ = undef;		# Read in the whole file with one slurp.
    open my $fh, $makefile_name or
      die "can't read makefile $makefile_name--$!\n";
    $makefile_contents = <$fh>; # Read the whole makefile.
    $makefile_contents =~ tr/\r//d;
				# Strip out those annoying CR characters
				# which get put in sometimes on windows.
  }

  unless( $c_preprocess ) {
    if ($makefile_contents =~ /^# Makefile\.in generated by automake/) {
      require Mpp::AutomakeFixer;	# Load the automake fixing stuff.
      Mpp::log LOAD_AUTOMAKE => $minfo
	if $Mpp::log_level;
      $makefile_contents =
	Mpp::AutomakeFixer::remove_automake_junk($makefile_contents);
                                # Clean out the crap that automake puts in for
                                # dependency tracking and recursive make.
    }

    # TBD: Shouldn't we at least exclude *comments* from the grep?
    if( $makefile_contents =~ /\$[({]MAKE[})]/ ) {
      Mpp::log LOAD_REC => $minfo
	if $Mpp::log_level;
      $self->{RECURSIVE_MAKE} = 1;
				# If there's a recursive invocation of make,
				# remember this so we can turn off implicit
				# makefile loading.  We have to know this
				# before we process any rules or anything
				# else from the makefile.
    }
  }

  local $makefile_lineno = 0;	# We're on the first line.

  local @hold_lines;		# Nothing in the hold area yet.

  local $last_conditional_start;
				# Don't mess up error messages from parent
				# makefile.

 makefile_line:
  while (defined($_ = read_makefile_line_stripped())) { # Read a line at a time.
    next if /^\s*$/;		# Skip blank lines.

    my $makefile_line = $makefile_name . ":$makefile_lineno";
				# The line name to use for error messages.

    if( $c_preprocess < 2 ) {
      my $equals = /=/ ? index_ignoring_quotes($_, '=') : 0;
				# Search for the equals of an assignment.
				# We use index_ignoring_quotes to skip over
				# equals signs that happen to be in quotes or
				# inside other make expressions.

      next if
	$equals > 0 &&		# If it's a real assignment, then we're done.
	parse_assignment $self, $makefile_line, $equals;
    }

    if( $c_preprocess ) {
#
# Do the actual work of the &preprocess command.
#
      no strict 'refs';
      my $count = grep { /::s_/ && *{$_}{CODE} } values %{"$self->{PACKAGE}::"};
      if( $self->{RE_COUNT} != $count ) { # Statement count changed.
	$self->{RE_COUNT} = $count;
	$self->{RE} = join '|', grep {
	  if( s/^s_// ) {
	    s/_/[-_]/g;
	    1;
	  }
	} keys %{"$self->{PACKAGE}::"};
	$self->{RE} = qr/$self->{RE}/;
      }
      if( !/^\s*($self->{RE})\b\s*(.*)/ ) {
	$_ = expand_text( $self, $_, $makefile_line );
	local $ARGV = $makefile_name;
	local $. = $makefile_lineno;
	&Mpp::Cmds::print;
	next;
      } elsif( $1 eq 'include' or my $optional = ($1 eq '-include' || $1 eq '_include') ) {
				# Do our own, since standard statement tries to build.
	for( split_on_whitespace expand_text $self, $2, $makefile_line ) {
	  my $finfo = file_info unquote(), $self->{CWD};
	  next if $optional && !file_exists $finfo;
	  read_makefile( $self, $finfo );
	}
	next;
      }
    } else {
#
# It's not an assignment. Check for a rule of some sort.  Basically, we just
# look for a colon, but this is somewhat tricky because there may be extra
# colons inside quotes or variable expansions.
#
      my @pieces = split_on_colon($_);
      if( @pieces > 1 ) { # Was there a colon somewhere?
	parse_rule( $self, $makefile_line, $makefile_line . $makefile_directory,
		    substr($_, length($pieces[0]), 2) eq '::', # Double colon rule.
		    @pieces );
	next;
      }
    }

#
# It's not a rule, either.  Check for a command or a statement, like
# 'include xyz.mk'.
#
    if( /^\s*(-?)\s*&(\w+)\s*(.*)/ ) { # Command at beginning of line?
      my( $ignore_error, $cmd, @args ) = ($1, $2, $3);
      @args = unquote_split_on_whitespace expand_text( $self, $args[0], $makefile_line ) if @args;
      local $Mpp::Subs::rule = { MAKEFILE => $self, RULE_SOURCE => $makefile_line };
      eval {
	if( defined &{$self->{PACKAGE} . "::c_$cmd"} ) { # Function from makefile?
	  local $0 = $cmd;
	  &{$self->{PACKAGE} . "::c_$0"}( @args );
	} elsif( defined &{"Mpp::Cmds::c_$cmd"} ) { # Builtin Function?
	  local $0 = $cmd;
	  &{"Mpp::Cmds::c_$0"}( @args );
	} else {
	  run $cmd, @args;
	}
      };
      if( $@ ) {
	for( "$@" ) { # Make a modifiable copy.
	  s/\(eval \d+\)(?:\[.*?\])? line \d+/\`$makefile_line\'/g;
	  s/^$cmd: //;
	  if( $ignore_error ) {
	    print STDERR "makepp: &$cmd: $_";
	  } else {
	    die "makepp: &$cmd: $_";
	  }
	}
      }
      next;
    }

    if( /^\s*([-\w]+)(.*)/ ) {	# Statement at beginning of line?
      my ($rtn, $rest_of_line) = ($1, $2);
      $rtn =~ tr/-/_/;		# Make routine names more perl friendly.
      substr $rtn, 0, 0, "$self->{PACKAGE}::s_";
      if( defined &{$rtn} ) { # Function from makefile?
	eval { &$rtn( $rest_of_line, $self, $makefile_line ) };
				# Try to call it as a subroutine.
	$@ and die "$makefile_line: error handling $rtn statement\n$@\n";
      }	else {
	die "$makefile_line: unknown statement $rtn\n";
      }
      next;
    }

    if( /\$/ ) {
	$_ = expand_text $self, $_, $makefile_line;
	redo;
    }

    chomp;
    die "$makefile_line: syntax error in line '$_'\n";
  }
}

#
# Register a parser.  Arguments:
# a) The makefile.
# b) The word in the command to match.
# c) A reference to the subroutine.
#
sub register_parser {
  #my ($self, $word, $subr) = @_;

  ${"$_[0]{PACKAGE}::parsers"}{$_[1]} = $_[2];
}

#
# This subroutine reads a single line from the makefile fetched by
# read_makefile.  It works along with @hold_lines so you can temporarily put
# back lines if you've read too far.
#
sub read_makefile_line {
  if( exists $makefile->{CWD}{DUMP_MAKEFILE} ) {
    dump_line( &_read_makefile_line_1 );
  } else {
    goto &_read_makefile_line_1;
  }
}
sub _read_makefile_line_1 {
  return shift @hold_lines
    if @hold_lines;	# Was anything unread?

  ++$makefile_lineno;	# Keep the line counter accurate.
  length($makefile_contents) == 0 and return undef;
				# End of file.
  $makefile_contents =~ s/^(.*\n?)//;
				# Strip off the next line.  (Using pos() and
				# /\G/gc doesn't work, apparently because the
				# position gets lost when local() is executed.)
  $1;				# Return the next line.
}

{
  my $last_lineno=0;
  my $last_name='';
  sub dump_line {
    my $line = $_[0];
    my $fh = $makefile->{CWD}{DUMP_MAKEFILE};
    if($fh && defined($line) &&
      ($makefile_lineno != $last_lineno ||
       $makefile_name ne $last_name)
    ) {
      if($makefile_lineno != $last_lineno+1) {
        if( $makefile_lineno > $last_lineno && $makefile_name eq $last_name ) {
	  for(1..($makefile_lineno - $last_lineno - 1)) {
	    print $fh "\n";
	  }
        } else {
	  print $fh "# $makefile_lineno " .
	    "\"$makefile_name\"\n";
	}
      }
      print $fh $line;
      $last_lineno = $makefile_lineno;
      $last_name = $makefile_name;
    }
    return $line;
  }
}
#
# Read a line from this makefile, strip comments, and handle line
# continuations ('\' at end of line) and GNU make style conditionals.
#
# If you pass a true argument, then comments are not stripped but line
# continuations are handled.
#
# If the second argument is true, structures like ifdef are ignored.
#
sub read_makefile_line_stripped {
  if( exists $makefile->{CWD}{DUMP_MAKEFILE} ) {
    dump_line &_read_makefile_line_stripped_1;
  } else {
    goto &_read_makefile_line_stripped_1;
  }
}
my %sys;
sub _truthval($$) {
  my( $cond, $line ) = @_;
  my( $not, $def, $eq, $sys, $makeperl, $perl ) =
    $cond =~ /^(?:n())?(?:def()|eq()|sys()|true)|^(?:make())?perl()/;

  $last_conditional_start = $makefile_lineno;
				# Remember what line this was on so we can
				# give better error messages.
  my $file = $makefile_name . ':' . $makefile_lineno;
  $line = expand_text( $makefile, $line, $file )
    if defined $makeperl or !defined $perl; # not plain Perl
				# Expand away all the variables.
  $line =~ s/^\s+//;		# Strip leading whitespace.

  my $truthval;
  my $idx = index_ignoring_quotes $line, '#'; # Find comment.
  if( defined $def ) {			# See whether something is defined?
    substr( $line, $idx ) = '' if 0 <= $idx; # Strip comment.

    $truthval = expand_variable $makefile, $_, $file , 2
      and last			# Test for the existence of the variable.
      for split ' ', $line;	# Also strips trailing whitespace.
  } elsif( defined $eq ) {	# Check for string equality?
    substr( $line, $idx ) = '' if 0 <= $idx; # Strip comment.
    $line =~ s/\s+$//;	# Strip trailing whitespace.

    $idx = index_ignoring_quotes $line, ',';
    if ($line =~ /^\(/) {	# Parenthesized syntax? need to match make syntax to avoid
				# ambiguity if strings contain parentheses
      0 <= $idx or die "$file: Comma missing in 'if$cond$line'\n";
      $a = substr $line, 1, $idx - 1;
      $a =~ s/\s+$//;
      $b = substr $line, $idx + 1;
      $b =~ s/^\s+//;
      0 <= ($idx = max_index_ignoring_quotes $b, ')') or
	die "$file: Closing paren missing in 'if$cond$line'\n";
      $idx + 1 < length $b and
	die "$file: Trailing cruft after closing paren in `if$cond$line'\n";
      chop $b;			# Strip )
    } elsif( 0 <= $idx ) {	# Comma syntax
      $a = substr $line, 0, $idx;
      $a =~ s/\s+$//;
      $a =~ s/^\s+//;
      $b = substr $line, $idx + 1;
      $b =~ s/\s+$//;
      $b =~ s/^\s+//;
    } else {
      ($a, $b) = (split_on_whitespace($line), '', '');
				# Split on whitespace except whitespace inside
				# the quotes.
    }
				# Remove the quotes and compare.
    if( $Mpp::log_level ) {
      $a = unquote $a;
      $b = unquote $b;
      Mpp::log IFEQ => $a, $b, $file;
      $truthval = $a eq $b;
    } else {
      $truthval = unquote( $a ) eq unquote $b;
    }
  } elsif( defined $sys ) {	# See whether we're on the right system?
    substr( $line, $idx ) = '' if 0 <= $idx; # Strip comment.
    unless( %sys ) {		# First such, initialize.
      @sys{$^O, @Config::Config{qw(archname myarchname)}} = ();
      @sys{split " ", `uname -mps` || ''} = ()
	if Mpp::is_windows < 2;
    }
  REGEX:
    for( split_on_whitespace $line ) {
      my $regex = Mpp::Glob::wild_to_regex unquote;
      case_sensitive_filenames or $regex =~ s/\(\?i-/(?-i/; # Want this to be case_sensitive
      $truthval = /$regex/ and last REGEX
	for keys %sys;
    }
  } elsif( defined $perl ) {
    $makefile->cd;		# Evaluate in the correct directory.
    $truthval = Mpp::Subs::eval_or_die $line, $makefile, $file;
  } else {			# must be check for nonzero?
    substr( $line, $idx ) = '' if 0 <= $idx; # Strip comment.
    $line =~ s/\s+$//;	# Strip trailing whitespace.
    $truthval = $line ? 1 : undef;
  }
  defined $not ? !$truthval : $truthval; # Check for negated condition.
}

# ifdef, ifndef, ...
my $if_re = qr/n?(?:def|eq|sys|true)|(?:make)?perl/;
sub _read_makefile_line_stripped_1 {
  my $line;

  for (;;) {			# Loop until we get a whole line.
    my $next_line = &_read_makefile_line_1; # Get the next line.
    defined $next_line or last;

    $next_line =~ s/^\s+// if defined $line;
				# Strip out leading whitespace from line
				# continuations.

    next if !$_[0] && $next_line =~ /^#/; # Skip it if it begins with a comment.

    $line .= $next_line;	# Append it to the current line.
    my $closedgroup = ($line !~ s/((?:^|[^\$])\$(?:\(\((?!.*\)\))|\{\{(?!.*\}\})|\[\[(?!.*\]\])).*?)\s*$/$1 /s);
				# Allow $(( )) or ${{ }} to span lines.
    # TODO: this breaks on ' #' within quoted strings:
    $line =~ s/\s+#.*/ $closedgroup ? '' : ' ' /e # Strip out comments.
      unless $closedgroup && $_[0];

    last if $line !~ s/\s*\\\s*$/ / && # Quit unless there's a trailing \.
				# Note that the trailing backslash has to be
				# replaced by whitespace to conform with
				# some makefiles I have seen.
      $closedgroup;
  }

  defined $line or return undef; # No point checking at end of file.
  if( -1 < index $line, '$[' ) {
    eval {
      $expand_bracket++;
      unshift_makefile_lines( expand_text $makefile, $line, $makefile_name . ':' . $makefile_lineno );
      $line = shift @hold_lines;
    };
    $expand_bracket--;
    die $@ if $@;
  }
  return $line if $_[1];

#
# Handle GNU make's conditionals:
#

 ANY_LINE:
  # TODO: complain about unexpected else or endif:
  if( $line =~ s/^\s*if($if_re)\b//o ) {
				# Looks like an if statement?
  IF_STATEMENT:
    my( $truthval, $totaltruthval ) = _truthval $1, $line;
    while( 1 ) {
      $line = _read_makefile_line_stripped_1( 0, 1 );
				# Next joined line with ifdef intact.
      defined $line or
	die "$makefile_name:$last_conditional_start: end of makefile inside conditional\n";
      if( $line =~ s/^\s*(?:and|or())\s+if($if_re)\b//o ) {
	next if $totaltruthval ||= $truthval && defined $1;
				# Once we had a series of "1 and 1 ...", then an or, test no more.
	next unless $truthval || defined $1; # "0 and anything" stays 0
	$truthval = _truthval $2, $line;
				# Was either "0 or ..." or "1 and ..."
      } elsif ($line =~ s/^\s*else\s+if($if_re)\b//o ) {
				# Empty then branch and straight into another if.
	$totaltruthval = $totaltruthval ? 0 : !$truthval;
				# ifxxx, else is like a complex ifnxxx
	goto IF_STATEMENT if $totaltruthval;
				# Shortcut for pushback and reparsing.
	undef $line;		# line has already been processed
	last;
      } elsif( $line =~ /^\s*else\s*(?:#|$)/ ) { # Empty then branch.
	$totaltruthval = $totaltruthval ? 0 : !$truthval;
				# ifxxx, else is like a complex ifnxxx
	undef $line;		# line has already been processed
	last;
      } elsif( $line =~ /^\s*endif\s*(?:#|$)/ ) { # What the heck?
	warn "empty conditional at `$makefile_name:$last_conditional_start'\n";
	goto &_read_makefile_line_stripped_1; # Return a new line.
      } else {			# Reached then branch.
	$totaltruthval ||= $truthval;
	last;			# line still needs to be processed
      }
    }
    if (!$totaltruthval) {		# Was it true?
      unshift @hold_lines, $line if defined $line;
				# Put back the extra line
      skip_makefile_until_else_or_endif( 0 );
				# No.  Skip a big chunk.
      goto &_read_makefile_line_stripped_1; # Now return a line.
    }
    goto ANY_LINE if defined($line);
				# We read one too much.
    goto &_read_makefile_line_stripped_1;
  } elsif( $line =~ /^\s*else\s*(?:if$if_re\b|#|$)/o ) { # Else clause for an if?
    skip_makefile_until_else_or_endif( 1 );
				# If we're here, the condition must have been
				# true, so we know the else part must be false.
				# Skip until we see the endif.
    goto &_read_makefile_line_stripped_1; # Return the next line.
  } elsif ($line =~ /^\s*endif\s*(?:#|$)/) { # End of an if?
    goto &_read_makefile_line_stripped_1; # Return the next line.
  }

  $line;
}

#
# Skip until we find a line containing else or endif.  This is used to skip
# over the false part of an if statement.  With a true argument always skip
# upto endif.
#
sub skip_makefile_until_else_or_endif {
  my $was_true = $_[0];
  my $endif_expected = 1;	# We return on the first endif, unless we see
				# a nested if in the mean time.
  my $re;
  for (;;) {
    my $line = &_read_makefile_line_1; # Read another line.
    defined($line) or
      die "$makefile_name:$last_conditional_start: end of makefile inside conditional\n";
    if ($re) {
      next if $line !~ /$re/;
      $re = 0;
    }
    while( $line =~ s/\s*\\\s*$/ / ) {
      my $nextline = &_read_makefile_line_1; # Handle continuations, because
				# we don't want to find an else inside an
				# action.
      last unless defined $nextline;
      $line .= $nextline;
    }

    if( $line =~ /^\s*if$if_re\b/o ) {
      ++$endif_expected;	# Need another endif.
    } elsif( $endif_expected == 1 && !$was_true && $line =~ /^\s*else\s*(?:\s*if$if_re\b()|#|$)/o ) {
				# Found the matching else for the
				# current conditional.
      if( defined $1 ) {
	$line =~ s/^\s*else//;	# Turn it into a normal if, as though this were the beginning.
	unshift @hold_lines, $line;
      }
      return;
    } elsif ($line =~ /^\s*endif\s*(?:#|$)/) {
      return if --$endif_expected == 0;  # Found the last expected endif.
    } elsif ($line =~ /^\s*(?:make)?(?:perl\s*\{|sub\s)/) {
      # an else in one of the following is not a makepp statement
      Mpp::Subs::read_block $line;
    } elsif ($line =~ /^\s*define\b/) {
      $re = qr/^\s*endd?ef\s*$/;
    } elsif ($line =~ /^\s*perl_begin\b/) {
      $re = qr/^\s*perl_end\b/;
    }
  }
}

#
# The "official" interface for embedded-perl to use
#
sub unshift_makefile_lines {
  # split lines, but need to retain "\n" because it's a char after "split_on_colon", etc
  unshift @hold_lines, map split( /^/m ), @_;
}

1;
