# $Id: Subs.pm,v 1.173 2010/02/09 22:45:23 pfeiffer Exp $

=head1 NAME

Mpp::Subs - Functions and statements for makefiles

=head1 DESCRIPTION

This package contains subroutines which can be called from a makefile.
Subroutines in this package are called in two ways:

=over

=item 1)

Any line which isn't a rule or an assignment and has at the left margin a word
is interpreted as a subroutine call to a subroutine in the makefile package,
or if not in the makefile package, in this package.  "s_" is prefixed to the
name before the perl function is looked up.

=item 2)

Any function that is in a make expression (e.g., $(xyz abc)) attempts to call
a perl function in the make package, and failing that, in this package.  "f_"
is prefixed to the name first.

=back

All official subroutine names in this package are automatically exported to
each makefile package by Mpp::Makefile::load.  See the regexps in import, for
which ones are official.

=cut

package Mpp::Subs;

use strict qw(vars subs);

use Mpp::Text qw(index_ignoring_quotes split_on_whitespace requote
		unquote unquote_split_on_whitespace format_exec_args);
use Mpp::File;
use Mpp::FileOpt;
use Mpp::Event qw(wait_for when_done read_wait);
use Mpp::Glob qw(zglob zglob_fileinfo);
use Mpp::CommandParser;
use Mpp::CommandParser::Gcc;

# eval successfully or die with a fixed error message
our( $makefile, $makefile_line );
sub eval_or_die($$$) {
  my $code = $_[0];
  # Make $makefile and $makefile_line available to the perl code, so that it
  # can call f_* and s_* subroutines.
  local( undef, $makefile, $makefile_line ) = @_; # Name the arguments.

  (my $line = $makefile_line) =~ s/(.+):(\d+)(?:\(.+\))?$/#line $2 "$1"/;
  &touched_filesystem;
  $code = qq{
    no strict; package $makefile->{PACKAGE};
    \@Cxt=(\$Mpp::Subs::makefile, \$Mpp::Subs::makefile_line);
$line
$code};
  if( wantarray ) {
    my @result = eval $code;
    &touched_filesystem;
    die $@ if $@;
    @result;
  } elsif( defined wantarray ) {
    my $result = eval $code;
    &touched_filesystem;
    die $@ if $@;
    $result;
  } else {
    eval $code;
    &touched_filesystem;
    die $@ if $@;
  }
}

our $rule;

###############################################################################
#
# Command scanners included with makepp:
#

#
# Scan C command, looking for sources and includes and libraries.
# (Don't use 'our', because it does bad things to eval calls in this file.)
#
# TODO: is $ENV{INCLUDE} a reliable alternative on native Windows?  And if
# ActiveState is to call MinGW gcc, must makepp translate directory names?
our @system_include_dirs = grep -d, qw(/usr/local/include /usr/include);
our @system_lib_dirs = grep -d, qw(/usr/local/lib /usr/lib /lib);

sub scanner_gcc_compilation {
  shift;
  Mpp::CommandParser::Gcc->new( @_ );
}

sub scanner_c_compilation {
  shift;
  Mpp::CommandParser::Gcc->new_no_gcc( @_ );
}

sub scanner_esqlc_compilation {
  shift;
  require Mpp::CommandParser::Esqlc;
  Mpp::CommandParser::Esqlc->new( @_ );
}

sub scanner_vcs_compilation {
  shift;
  require Mpp::CommandParser::Vcs;
  Mpp::CommandParser::Vcs->new( @_ );
}

sub scanner_swig {
  shift;
  require Mpp::CommandParser::Swig;
  Mpp::CommandParser::Swig->new( @_ );
}

#
# This scanner exists only to allow the user to say ":scanner none" to suppress
# the default scanner.
#
sub scanner_none {
  $_[1]{SCANNER_NONE} = 1;
  shift;
  Mpp::CommandParser->new( @_ );
}

#
# This scanner simply moves to the next word that doesn't begin with
# - and scans again.
#
sub scanner_skip_word {
  #my ($action, $myrule, $dir) = @_;
  my ($action) = @_;		# Name the arguments.

  $action =~ s/^\s+//;		# Leading whitespace messes up the regular
				# expression below.
  while ($action =~ s/^\S+\s+//) { # Strip off another word.
    $action =~ s/^([\"\'\(])//;	# Strip off leading quotes in case it's
				# something like sh -c "cc ...".
    if( defined $1 ) {
      my $compl = ${{qw!" " ' ' ( \)!}}{$1};
      $action =~ s/$compl//;
    }
    next if $action =~ /^-/;	# Word that doesn't look like an option?
    local $_[1]{ACTION_SCANNER} if $_[1]{ACTION_SCANNER}; # Don't skip next word on recursion.
    local $_[1]{PARSER_OBJ} if $_[1]{PARSER_OBJ}; # ditto
    my $action_parser = new Mpp::ActionParser;
    $_[1]{SCANNER_NONE} = 1
      if Mpp::ActionParser::parse_command( $action_parser, $action, $_[1], $_[2], $_[1]{MAKEFILE}{ENVIRONMENT} );
    last;			# Don't go any further.
  }
  new Mpp::ActionParser;
}

#
# This array contains the list of the default scanners used for various
# command words.
# (Don't use 'our', because it does bad things to eval calls in this file.)
#
our %scanners =
  (
   # These words usually introduce another command
   # which actually is the real compilation command:
   ash		=> \&scanner_skip_word,
   bash		=> \&scanner_skip_word,
   csh		=> \&scanner_skip_word,
   ksh		=> \&scanner_skip_word,
   sh		=> \&scanner_skip_word,
   tcsh		=> \&scanner_skip_word,
   zsh		=> \&scanner_skip_word,

   ccache	=> \&scanner_skip_word,
   condor_compile => \&scanner_skip_word,
   cpptestscan	=> \&scanner_skip_word, # Parasoft c++test
   diet		=> \&scanner_skip_word, # dietlibc
   distcc	=> \&scanner_skip_word,
   fast_cc	=> \&scanner_skip_word,
   ignore_error	=> \&scanner_skip_word,
   libtool	=> \&scanner_skip_word,
   noecho	=> \&scanner_skip_word,
   purecov	=> \&scanner_skip_word,
   purify	=> \&scanner_skip_word,
   quantify	=> \&scanner_skip_word,
   time		=> \&scanner_skip_word,
   if		=> \&scanner_skip_word, # Sometimes people do things like
   then		=> \&scanner_skip_word, # "if gcc main.o -labc -o my_program; ..."
   elif		=> \&scanner_skip_word,
   else		=> \&scanner_skip_word,
   do		=> \&scanner_skip_word,
   while	=> \&scanner_skip_word,
   until	=> \&scanner_skip_word,

   # All the C/C++ compilers we have run into so far:
   cc		=> \&scanner_c_compilation,
  'c++'		=> \&scanner_c_compilation,
   CC		=> \&scanner_c_compilation,
   cxx		=> \&scanner_c_compilation,
   c89		=> \&scanner_c_compilation,
   c99		=> \&scanner_c_compilation,
   pcc		=> \&scanner_c_compilation,
   kcc		=> \&scanner_c_compilation, # KAI C++.
   ccppc	=> \&scanner_c_compilation, # Green Hills compilers.
   cxppc	=> \&scanner_c_compilation,
   aCC		=> \&scanner_c_compilation, # HP C++.
   lsbcc	=> \&scanner_c_compilation, # LSB wrapper around cc.
  'lsbc++'	=> \&scanner_c_compilation,
   xlc		=> \&scanner_c_compilation, # AIX
   xlc_r	=> \&scanner_c_compilation,
   xlC		=> \&scanner_c_compilation,
   xlC_r	=> \&scanner_c_compilation,
   cpp		=> \&scanner_c_compilation, # The C/C++ preprocessor.
   cl		=> \&scanner_c_compilation, # MS Visual C/C++
   bcc32	=> \&scanner_c_compilation, # Borland C++
   insure	=> \&scanner_c_compilation, # Parasoft Insure++

   vcs		=> \&scanner_vcs_compilation,

   ecpg		=> \&scanner_esqlc_compilation,
   esql		=> \&scanner_esqlc_compilation,
   esqlc	=> \&scanner_esqlc_compilation,
   proc		=> \&scanner_esqlc_compilation,
   yardpc	=> \&scanner_esqlc_compilation,

   swig         => \&scanner_swig
);

@scanners{ map "$_.exe", keys %scanners } = values %scanners
  if Mpp::is_windows;

# True while we are within a define statement.
our $s_define;

#
# An internal subroutine that converts Mpp::File structures to printable
# names.  Takes either a single Mpp::File structure, an array of Mpp::File
# structures, or a reference to an array of Mpp::File structures.
#
sub relative_filenames {
  my @ret_vals;

  my $cwd = $rule->build_cwd;
  foreach (@_) {
    next unless defined;	# Skip undef things--results in a blank.
    push @ret_vals, (ref() eq 'ARRAY') ? relative_filenames(@$_) : relative_filename $_, $cwd;
  }

  @ret_vals;
}

###############################################################################
#
# Functions that are intended to be invoked by make expressions.  These
# all begin with the prefix "f_", which is added before we look up the
# name of the function.	 These functions are called with the following
# arguments:
# a) The text after the function name in the makefile (with other macros
#    already expanded).
# b) The makefile.
# c) The line number in the makefile that this expression occured in.
#

#
# Define all the cryptic one-character symbols, and anything else that isn't a
# valid subroutine name:
#
our %perl_unfriendly_symbols =
  ('@' => \&f_target,
   '<' => \&f_dependency,
   '^' => \&f_dependencies,
   '?' => \&f_changed_dependencies,
   '+' => \&f_sorted_dependencies,
   '*' => \&f_stem,
   '&' => '',			# Perl makefiles use this for some reason, but
				# $& is a perl pattern match variable.
   '/' => Mpp::is_windows > 1 ? '\\' : '/',

   '@D' => \&f_target,		# Special handling in expand_variable for /^.[DF]$/.
   '@F' => \&f_target,
   '*D' => \&f_stem,
   '*F' => \&f_stem,
   '<D' => \&f_dependency,
   '<F' => \&f_dependency,
   '^D' => \&f_dependencies,
   '^F' => \&f_dependencies
  );

#
# Return the absolute filename of all the arguments.
#
sub f_absolute_filename {
  my $cwd = $_[1]{CWD};
  join ' ',
    map absolute_filename( file_info unquote(), $cwd ),
      split_on_whitespace $_[0];
}

sub f_absolute_filename_nolink {
  my $cwd = $_[1]{CWD};
  join ' ',
    map absolute_filename_nolink( file_info unquote(), $cwd ),
      split_on_whitespace $_[0];
}

sub f_addprefix {
  my ($prefix, $text) = split(/,\s*/, $_[0]); # Get the prefix.
  join ' ', map "$prefix$_", split ' ', $text;
}

sub f_addsuffix {
  my ($suffix, $text) = split(/,\s*/, $_[0]); # Get the prefix.
  join ' ', map "$_$suffix", split ' ', $text;
}

sub f_basename {
  join ' ', map { s!\.[^./,]*$!!; $_ } split ' ', $_[0];
}

sub f_call {
  my @args = split(/,\s*/, $_[0]);
  return '' unless $args[0];
  my( undef, $res ) = $_[1]->expand_variable( $args[0], $_[2], 1 );
  return '' unless $res;
  my $params = qr!\$(?:(\d+)|\{(\d+)\}|\((\d+)\))!;
  $res =~ s!\$\$!\000!g;                        # Switch away all $$
  {
    no warnings 'uninitialized';  # Don't care, getting undef from @args
    $res =~ s!$params!$args[$1||$2||$3||0]!ge; # Handle $n, ${n} and $(n)
  };
  $res =~ s!\000!\$\$!g;                        # Switch back to $$
  $res ? $_[1]->expand_text($res, $_[2]) : '';
}

sub f_dir {
  join ' ', map { m@^(.*/)@ ? $1 : './' } split ' ', $_[0];
}

sub f_dir_noslash {		# An internal routine that does the same
				# thing but doesn't return a trailing slash.
  join ' ', map { m@^(.*)/@ ? $1 : '.'} split ' ', $_[0];
}

sub f_error {
  die "$_[2]: *** $_[0]\n";	# Throw the text.
}

#
# Perform a pattern substitution on file names.	 This differs from patsubst
# in that it will perform correctly when alternate names for directories are
# given (as long as they precede the percent sign).  For example,
#
#  $(filesubst ./src/%.c, %.o, $(wildcard src/*.c))
#
# will work with filesubst but not with patsubst.
#
sub f_filesubst {
  my ($src, $dest, $words) = split(/\s*,\s*/, $_[0]);
				# Get the patterns.
  !defined($words) and die "filesubst: too few arguments";
  my $cwd = $_[1]{CWD};
#
# First we eat away at the directories on the source until we find the
# percent sign.	 We remember where this directory is.  Then we consider each
# of the words and strip off leading directories until we reach that
# directory.  Then we run through patsubst.
#
  my $startdir = ($src =~ s@^/+@@) ? $Mpp::File::root : $cwd;
				# The directory we're in if there are no
				# other directories specified.

  while ($src =~ s@([^%/]+)/+@@) { # Strip off a leading directory that
				# doesn't contain the % sign.
    $startdir = dereference file_info $1, $startdir;
				# Move to that directory.
  }

#
# Now eat away at the directories in the words until we reach the starting
# directory.
#
  my @words;
  foreach (split(' ', $words)) {
    my $thisdir = (s@^/+@@) ? $Mpp::File::root : $cwd;
    while ($thisdir != $startdir &&
	   s@([^/]+)/+@@) {	# Another directory?
      $thisdir = dereference file_info $1, $thisdir;
    }
    push @words, case_sensitive_filenames ? $_ : lc;
				# What's left is the filename relative to that
				# directory.
  }

  join ' ', Mpp::Text::pattern_substitution( case_sensitive_filenames ? $src : lc $src,
					    $dest,
					    @words );
}

sub f_filter {
  my ($filters, $words) = split(/,\s*/, $_[0]);

  !defined($words) and die "filter: too few arguments";
  my @filters = split(' ', $filters); # Can be more than one filter.
  foreach (@filters) {		# Convert these into regular expressions.
    s/([.+()])/\\$1/g;		# Protect all the periods and other special chars.
    s/[*%]/\.\*/g;		# Replace '*' and '%' with '.*'.
    $_ = qr/^$_$/;		# Anchor the pattern.
  }

  my @ret_words;
 wordloop:
  foreach (split(' ', $words)) { # Now look at each word.
    foreach my $filter (@filters) {
      if (/$filter/) {		# Does it match this filter?
	push @ret_words, $_;
	next wordloop;
      }
    }
  }

  join ' ', @ret_words;
}

sub f_filter_out {
  my ($filters, $words) = split(/,\s*/, $_[0]);

  !defined($words) and die "filter_out: too few arguments";
  my @filters = split(' ', $filters); # Can be more than one filter.
  foreach (@filters) {		# Convert these into regular expressions.
    s/([.+()])/\\$1/g;		# Protect all the periods and other special chars.
    s/[*%]/\.\*/g;		# Replace '*' and '%' with '.*'.
    $_ = qr/^$_$/;		# Anchor the pattern.
  }

  my @ret_words;
 wordloop:
  foreach (split(' ', $words)) { # Now look at each word.
    foreach my $filter (@filters) {
      next wordloop if /$filter/; # Skip if it matches this filter.
    }
    push @ret_words, $_;
  }

  join ' ', @ret_words;
}

sub f_filter_out_dirs {
  #my ($text, $mkfile) = @_; # Name the arguments.
  join ' ', grep { !is_or_will_be_dir file_info $_, $_[1]{CWD} } split ' ', $_[0];
}

#
# Find one of several executables in PATH.  Optional 4th arg means to return found path.
# Does not consider last chance rules or autoloads if PATH is used.
#
# On Windows this is ugly, because an executable xyz is usually not present,
# instead there is xyz.exe.  If we want the full path with the builtin rules
# we need to depend on xyz as long as xyz.exe hasn't been built, because
# that's where Unix makefiles put the dependencies.  To make matters worse,
# stat may lie about xyz when only xyz.exe exists.
#
sub f_find_program {
  my $mkfile = $_[1];		# Access the other arguments.

  my @pathdirs;			# Remember the list of directories to search.
  my $first_round = 1;
  foreach my $name ( split ' ', $_[0]) {
    if( $name =~ /\// || Mpp::is_windows > 1 && $name =~ /\\/ ) { # Either relative or absolute?
      my $finfo = path_file_info $name, $mkfile->{CWD};
      my $exists = Mpp::File::exists_or_can_be_built $finfo;
      if( Mpp::is_windows && $name !~ /\.exe$/ ) {
	my( $exists_exe, $finfo_exe );
	$exists_exe = Mpp::File::exists_or_can_be_built $finfo_exe = Mpp::File::path_file_info "$name.exe", $mkfile->{CWD}
	  if !$exists ||
	    $_[3] && $Mpp::File::stat_exe_separate ? !exists $finfo->{EXISTS} : !open my $fh, '<', absolute_filename $finfo;
				# Check for exe, but don't bother returning it, unless full path wanted.
				# If stat has .exe magic, EXISTS is meaningless.
	return $_[3] ? absolute_filename( $finfo_exe ) : $name if $exists_exe;
      }
      return $_[3] ? absolute_filename( $finfo ) : $name if $exists;
      next;
    }
    @pathdirs = Mpp::Text::split_path( $mkfile->{EXPORTS} ) unless @pathdirs;
    foreach my $dir (@pathdirs) { # Find the programs to look for in the path:
      # Avoid publishing nonexistent dirs in the path.  This works around
      # having unquoted drive letters in the path looking like relative
      # directories.
      if( $first_round ) {
	$dir = path_file_info $dir, $mkfile->{CWD};
	undef $dir unless is_or_will_be_dir $dir;
      }
      next unless $dir;
      my $finfo = file_info $name, $dir;
      my $exists = Mpp::File::exists_or_can_be_built $finfo, undef, undef, 1;
      if( Mpp::is_windows && $name !~ /\.exe$/ ) {
	my( $exists_exe, $finfo_exe );
	$exists_exe = Mpp::File::exists_or_can_be_built $finfo_exe = file_info( "$name.exe", $dir ), undef, undef, 1
	  if !$exists ||
	    $_[3] && $Mpp::File::stat_exe_separate ? !exists $finfo->{EXISTS} : !open my $fh, '<', absolute_filename $finfo;
				# Check for exe, but don't bother returning it, unless full path wanted.
	return $_[3] ? absolute_filename( $finfo_exe ) : $name if $exists_exe;
      }
      return $_[3] ? absolute_filename( $finfo ) : $name if $exists;
    }
    $first_round = 0;
  }

  Mpp::log NOT_FOUND => $_[0], $_[2];
  'not-found';			# None of the programs were executable.
}

#
# Find a file in a specified path, or in the environment variable PATH if
# nothing is specified.
#
sub f_findfile {
  my ($name, $path) = split(/,\s*/, $_[0]); # Get what to look for, and where
				# to look for it.
  my $mkfile = $_[1]; # Access the other arguments.
  my @pathdirnames = $path ? split( /\s+|:/, $path ) :
    Mpp::Text::split_path( $mkfile->{EXPORTS} );
				# Get a separate list of directories.
  my @names = split(' ', $name); # Get a list of names to find.
  foreach $name (@names) {	# Look for each one in the path:
    foreach my $dir (@pathdirnames) {
      my $finfo = file_info($name, file_info($dir, $mkfile->{CWD}));
				# Get the finfo structure.
      if( file_exists( $finfo )) { # Found it?
	$name = absolute_filename( $finfo ); # Replace it with the full name.
	last;			# Skip to the next thing to look for.
      }
    }
  }

  join ' ', @names;
}

#
# Find a file by searching for it in the current directory, then in ., ..,
# etc.
# Modified from function contributed by Matthew Lovell.
#
# Two versions are supplied: $(find_upwards ...) is the original function:
# its behavior, when given multiple filenames, it attempts to find all
# the requested files
#
# $(find_first_upwards ...) is similar, but reverses the order of the loop.
# It looks for any of the named files at one directory-level, before going
# to "..", where it then also looks for any of the filenames. It returns the
# first file that it finds.
sub f_find_upwards {
  my $cwd = $_[1]{CWD};

  my @ret_names;

  my $cwd_devid;		# Remember what device this is mounted on
				# so we can avoid crossing file system boundaries.

  for( split_on_whitespace $_[0] ) {
    $_ = unquote;
    my( $found, $finfo );
    for( my $dirinfo = $cwd;
	 $dirinfo &&
	 (stat_array $dirinfo)->[Mpp::File::STAT_DEV] ==
	   ($cwd_devid ||= (stat_array $cwd)->[Mpp::File::STAT_DEV]);
				# Don't cross device boundaries.  This is
				# intended to avoid trouble with automounters
				# or dead network file systems.
	 $dirinfo = $dirinfo->{'..'} ) { # Look in all directories above us.
      $finfo = file_info $_, $dirinfo;
      if( Mpp::File::exists_or_can_be_built $finfo ) { # Found file in the path?
	$found = 1;
	last;			# done searching
      }
    }
    $found or die "find_upwards: cannot find file $_\n";
    push @ret_names, relative_filename $finfo, $cwd;
  }

  join ' ', @ret_names;
}

sub f_find_first_upwards {
  my @fnames = unquote_split_on_whitespace $_[0];
  my $cwd = $_[1]{CWD};

  my $cwd_devid;		# Remember what device this is mounted on
				# so we can avoid crossing file system boundaries.

  for( my $dirinfo = $cwd;
       $dirinfo &&
       (stat_array $dirinfo)->[Mpp::File::STAT_DEV] ==
	 ($cwd_devid ||= (stat_array $cwd)->[Mpp::File::STAT_DEV]);
				# Don't cross device boundaries.  This is
				# intended to avoid trouble with automounters
				# or dead network file systems.
       $dirinfo = $dirinfo->{'..'} ) { # Look in all directories above us.
    for( @fnames ) {
      my $finfo = file_info $_, $dirinfo;
      return relative_filename $finfo, $cwd
	if Mpp::File::exists_or_can_be_built $finfo; # Found file in the path?
    }
  }
  die "find_first_upwards cannot find any of the requested files: @fnames\n";
}

sub f_findstring {
  my ($find, $in) = split(/,/, $_[0]);

  (index($in, $find) >= 0) ? $find : '';
}

sub f_firstword {
  (split ' ', $_[0], 2)[0] || '';
}

#
# Return the first available file of a list of possible candidates.
# This can be used to make your makefiles work in several different
# environments.
#
sub f_first_available {
  foreach my $fname (split(' ', $_[0])) {
    Mpp::File::exists_or_can_be_built( file_info $fname, $_[1]->{CWD} ) and return $fname;
  }
  '';
}

#
# The if function is unusual, because its arguments have not
# been expanded before we call it.  The if function is defined so that
# only the expression that is actually used is expanded.  E.g., if the
# if statement is true, then only the then expression is expanded, and
# any side effects of the else expression do not happen.
#
sub f_if {
  my $iftrue = shift if ref $_[0];
  my ($text, $mkfile, $mkfile_line) = @_; # Name the arguments.
  my $comma = index_ignoring_quotes($text, ',');
				# Find the first comma.
  $comma >= 0 or die "$mkfile_line: \$(if ) with only one argument\n";
  my $cond = $mkfile->expand_text(substr($text, 0, $comma), $mkfile_line);
				# Evaluate the condition.
  $cond =~ s/^\s+//;		# Strip out whitespace on the response.
  $cond =~ s/\s+$//;

  $text = substr $text, $comma+1; # Get the text w/o the comma.

  $comma = index_ignoring_quotes($text, ',');
				# Find the boundary between the then and the
				# else clause.
  if ($cond || (!$iftrue && $cond ne "")) {	# Is the condition true?
    $text = substr $text, 0, $comma
      if $comma >= 0;		# Was there an else clause?
				# Otherwise no else clause, then clause is the rest.
  } else {			# Condition was false.	Extract the else clause.
    $comma >= 0 or return '';	# No else clause.
    $text = substr $text, $comma+1; # Get the text.
  }
  $text =~ s/^\s+//;		# Strip out leading whitespace.
  $text =~ s/\s+$//;		# Strip out trailing whitespace.
  $mkfile->expand_text($text, $mkfile_line);
}
sub f_iftrue {
    unshift @_, \1;
    goto &f_if;
}

#
# Infer the linker command from a list of objects.  If any of the objects
# is fortran, we use $(FC) as a linker; if any of the objects is C++, we
# use $(CXX); otherwise, we use $(CC).
#
# This function is mostly used by the default link rules (see
# makepp_builtin_rules.mk).
#
sub f_infer_linker {
  my ($text, $mkfile, $mkfile_line) = @_; # Name the arguments.
  my @objs = split(' ', $text);	# Get a list of objects.
#
# First build all the objs.  Until we build them, we don't actually know what
# source files went into them.	They've probably been built, but we must
# make sure.
#
  my @build_handles;
  &Mpp::maybe_stop;
  foreach my $obj (@objs) {
    $obj = file_info($obj, $mkfile->{CWD}); # Replace the name with the
				# fileinfo.
    my $bh = prebuild( $obj, $mkfile, $mkfile_line );
				# Build this one.
    $bh and push @build_handles, $bh;
  }

  my $status = wait_for @build_handles;	# Wait for them all to build.
  $status and die "Error while compiling\n"; # Maybe I'll come up with a better
				# error message later.

#
# Now see what source files these were built from.  Unfortunately, the
# dependencies have been sorted, so we can't just look at the first one.
#
  my $linker;
  foreach my $obj (@objs) {
    foreach my $source_name(split(/\01/, Mpp::File::build_info_string($obj, 'SORTED_DEPS') || '')) {
      # TODO: Why is $(FC) only Fortran 77?  What about .f90 files?
      $source_name =~ /\.f(?:77)?$/ and $linker = '$(FC)';
      $source_name =~ /\.(?:c\+\+|cc|cxx|C|cpp|moc)$/ and $linker ||= '$(CXX)';
    }
  }
  $linker ||= '$(CC)';	# Assume we can use the ordinary C linker.

  $mkfile->expand_text($linker, $mkfile_line);
				# Figure out what those things expand to.
}

#
# Usage:
#    target : $(infer_objs seed-list, list of possible objs)
#
sub f_infer_objects {
  my ($text, $mkfile, $mkfile_line) = @_; # Name the arguments.
  my ($seed_objs, $candidate_list) = split(/,\s*/, $text);
				# Get the arguments.

  $candidate_list or die "infer_objects called without a candidate list\n";

  my $build_cwd = $rule ? $rule->build_cwd : $mkfile->{CWD};

#
# Build up a list of all the possibilities:
#
  my %candidate_objs;
  foreach my $candidate_obj (map(Mpp::Glob::zglob_fileinfo_atleastone($_, $build_cwd),
				 split(' ', $candidate_list))) {
				# Get a list of all the possible objs.
    my $objname = $candidate_obj->{NAME};
    $objname =~ s/\.[^\.]+$//;	# Strip off the extension.
    if ($candidate_objs{$objname}) { # Already something by this name?
      ref($candidate_objs{$objname}) eq 'ARRAY' or
	$candidate_objs{$objname} = [ $candidate_objs{$objname} ];
				# Make into an array as appropriate.
      push @{$candidate_objs{$objname}}, $candidate_obj;
    }
    else {			# Just one obj?
      $candidate_objs{$objname} = $candidate_obj;
    }
  }
#
# Now look at the list of all the include files.  This is a little tricky
# because we don't know the include files until we've actually built the
# dependencies.
#
  my %source_names;		# These are the names of include files for
				# which are look for the corresponding objects.

  my @build_handles;		# Where we put the handles for building objects.
  my @deps = map zglob_fileinfo($_, $build_cwd), split ' ', $seed_objs;
				# Start with the seed files themselves.
  @deps == 0 and die "infer_objects called with no seed objects that exist or can be built\n";
  Mpp::log INFER_SEED => \@deps
    if $Mpp::log_level;

  foreach (@deps) {
    my $name = $_->{NAME};
    $name =~ s/\.[^\.]+$//;	# Strip off the extension.
    $source_names{$name}++;	# Indicate that we already have this as a
				# source file.
  }


  my $dep_idx = 0;

  &Mpp::maybe_stop;
#
# Build everything, so we know what everything's dependencies are.  Initially,
# we'll only have a few objects to start from, so we build all of those, in
# parallel if possible.	 (That's why the loop structure is so complicated
# here.)  Then we infer additional objects, build those in parallel, and
# so on.
#
  for (;;) {
    while ($dep_idx < @deps) {	# Look at each dependency currently available.
      my $o_info = $deps[$dep_idx]; # Access the Mpp::File for this object.
      my $bh = prebuild( $o_info, $mkfile, $mkfile_line );
				# Start building it.
      my $handle = when_done $bh, # Build this dependency.
      sub {			# Called when the build is finished:
	defined($bh) && $bh->status and return $bh->status;
				# Skip if an error occured.
	my @this_sources = split(/\01/, Mpp::File::build_info_string($o_info,'SORTED_DEPS') || '');
				# Get the list of source files that went into
				# it.
	foreach (@this_sources) {
	  my $name = $_;	# Make a copy of the file.
	  $name =~ s@.*/@@;	# Strip off the path.
	  $name =~ s/\.[^\.]+$//; # Strip off the extension.
	  unless ($source_names{$name}++) { # Did we already know about that source?
	    if (ref($candidate_objs{$name}) eq 'Mpp::File') { # Found a file?
	      Mpp::log INFER_DEP => $candidate_objs{$name}, $_
		if $Mpp::log_level;
	      push @deps, $candidate_objs{$name}; # Scan for its dependencies.
	    }
	    elsif (ref($candidate_objs{$name}) eq 'ARRAY') { # More than 1 match?
	      Mpp::print_error('`', $mkfile_line, "' in infer_objects: more than one possible object for include file $_:\n  ",
			    join("\n  ", map absolute_filename( $_ ), @{$candidate_objs{$name}}),
			    "\n");
	    }
	  }
	}
      };

      if (defined($handle)) {   # Something we need to wait for?
        $handle->{STATUS} && !$Mpp::keep_going and
          die "$mkfile_line: infer_objects failed because dependencies could not be built\n";
        push @build_handles, $handle;
      }
      ++$dep_idx;
    }

    last unless @build_handles;	# Quit if nothing to wait for.
    my $status = wait_for @build_handles; # Wait for them all to build, and
				# try again.
    @build_handles = ();	# We're done with those handles.
    $status and last;		# Quit if there was an error.
  }

#
# At this point, we have built all the dependencies, and we also have a
# complete list of all the objects.
#
  join ' ', map relative_filename( $_, $build_cwd ), @deps;
}

sub f_info {
  print "$_[0]\n";		# Print the text.
  '';
}

sub f_join {
  my ($words1, $words2) = split(/,/, $_[0]);
				# Get the two lists of words.
  defined($words2) or die "$_[2]: $(join ) called with < 2 arguments\n";
  my @words1 = split(' ', $words1);
  my @words2 = split(' ', $words2);

  for my $word ( @words1 ) {
    last unless @words2;
    $word .= shift @words2;
  }
  push @words1, @words2;
  join ' ', @words1;
}

#
# map Perl code to variable values
#
sub f_map {
  my $comma = index_ignoring_quotes $_[0], ',' or
    die "$_[2]: too few arguments to map\n";
  my $code = eval_or_die 'sub {' . substr( $_[0], $comma + 1 ) . "\n;defined}",
    $_[1], $_[2];
  $_[1]->cd;			# Make sure we're in the correct directory
  join ' ', grep &$code, split_on_whitespace $_[1]->expand_text( substr( $_[0], 0, $comma ), $_[2] );
}
*f_makemap = \&f_map;		# This will expand 1st arg twice, but 2nd time there should be no $ left

#
# make a temporary file name, similarly to the like named Unix command
#
our @temp_files;
END { Mpp::File::unlink $_ for @temp_files }
sub f_mktemp {
  my( $template, $mkfile ) = @_;
  $mkfile ||= \%Mpp::Subs::;	# Any old hash for default LAST_TEMP_FILE & CWD
  return $mkfile->{LAST_TEMP_FILE} || die "No previous call to \$(mktemp)\n" if $template eq '/';
  $template ||= 'tmp.';
  my $Xmax = 9;
  $Xmax = length( $1 ) - 1 if $template =~ s/(X+)$//;
  my $finfo;
  for( 0..999 ) {		# Should not normally loop at all.
    my $X = '';
    for( 0..$Xmax ) {
      my $chr = (!$_ && $Xmax) ? $$ % (26 + 26 + 10) : int rand 26 + 26 + 10;
				# First is from pid, if at least two given.
      $X .= $chr < 10 ?
	$chr :
	chr $chr - 10 + ($chr < 26 + 10 ?
			 ord 'a' :
			 -26 + ord 'A');
    }
    $mkfile->{LAST_TEMP_FILE} = $template . $X;
    $finfo = file_info $mkfile->{LAST_TEMP_FILE}, $mkfile->{CWD};
				# Default to global CWD, to make this easier to use without makefile.
    unless( $finfo->{MKTEMP}++ || file_exists $finfo ) {
      push @temp_files, $finfo;
      return $mkfile->{LAST_TEMP_FILE};
    }
  }
  die "$_[2]: too many tries necessary to make unique filename for $_[0]\n";
}

#
# Force all the targets to be made.
#
sub f_prebuild {
  my( $names, $mkfile, $mkfile_line ) = @_;

  my @build_handles;
  &Mpp::maybe_stop;
  for( split_on_whitespace $names ) {
    push @build_handles, prebuild( file_info( unquote(), $mkfile->{CWD} ),
				   $mkfile, $mkfile_line  );
                                # Start building this target.
  }
  my $status = wait_for @build_handles; # Wait for them all to complete before
                                # we continue.
  $status and die "\$(prebuild $names) failed\n";

  $names;			# Return arguments verbatim now that we have
                                # built them.
}
*f_make = \&f_prebuild;

sub f_notdir {
  join ' ', map { m@^.*/([^/]+)@ ? $1 : $_ } split ' ', $_[0];
}

#
# Return only the files in the list that are actually targets of some rule:
#
sub f_only_targets {
  my ($text, undef, undef, $phony) = @_; # Name the arguments.
  my $cwd = $_[1]{CWD};
  my @ret_files;


  foreach (split(' ', $_[0])) {
    foreach my $finfo (zglob_fileinfo($_, $cwd, 0, $phony)) {
      $phony || exists($finfo->{RULE}) and
	push @ret_files, relative_filename $finfo, $cwd;
    }
  }

  join ' ', @ret_files;
}

#
# Return only the targets in the list that are phony:
#
sub f_only_phony_targets {
  #my ($text, $mkfile) = @_;	# Name the arguments.
  f_only_targets $_[0], $_[1], 0, 1;
}

#
# Return only the files in the list that are not targets of some rule:
#
sub f_only_nontargets {
  #my ($text, $mkfile) = @_;	# Name the arguments.
  my $cwd = $_[1]{CWD};
  my @ret_files;

  foreach (split(' ', $_[0])) {
    foreach my $finfo (Mpp::Glob::zglob_fileinfo_atleastone($_, $cwd)) {
      exists($finfo->{RULE}) or
	push @ret_files, relative_filename $finfo, $cwd;
    }
  }

  join ' ', @ret_files;
}

#
# Returns only the existing files that were generated by makepp, according
# to the build info.
#
sub f_only_generated {
  #my ($text, $mkfile) = @_;	# Name the arguments.
  my $cwd = $_[1]{CWD};
  my @ret_files;

  foreach (split(' ', $_[0])) {
    foreach my $finfo (Mpp::Glob::zglob_fileinfo_atleastone($_, $cwd, 0,0,1)) {
      Mpp::File::was_built_by_makepp( $finfo ) and
	push @ret_files, relative_filename $finfo, $cwd;
    }
  }

  join ' ', @ret_files;
}

#
# Returns only the existing files that were generated by makepp, according
# to the build info, but are no longer targets.
#
sub f_only_stale {
  #my ($text, $mkfile) = @_;	# Name the arguments.
  my $cwd = $_[1]{CWD};
  my @ret_files;

  foreach (split(' ', $_[0])) {
    foreach my $finfo (Mpp::Glob::zglob_fileinfo_atleastone($_, $cwd, 0,0,1)) {
      Mpp::File::is_stale( $finfo ) and
	push @ret_files, relative_filename $finfo, $cwd;
    }
  }

  join ' ', @ret_files;
}

#
# Figure out where a variable came from:
#
sub f_origin {
  my( $varname, $mkfile ) = @_;
  $mkfile->{COMMAND_LINE_VARS}{$varname} ? 'command line' :
  $perl_unfriendly_symbols{$varname} ? 'automatic' :
  defined( ${$mkfile->{PACKAGE} . "::$varname"} ) ? 'file' :
  $mkfile->{ENVIRONMENT}{$varname} ? 'environment' :
  !defined( *{$mkfile->{PACKAGE} . "::f_$varname"}{CODE} ) ? 'undefined' :
  $varname =~ /^(?:foreach|targets?|dependency|dependencies|inputs?|outputs?)$/ ? 'automatic' :
    'default';	# Must be a variable like "CC".
}

#
# Perform a pattern substitution:
#
sub f_patsubst {
  my ($src, $dest, $words) = split(/,\s*/, $_[0]);
				# Get the arguments.
  join ' ', Mpp::Text::pattern_substitution( $src, $dest,
					    split_on_whitespace($words));
}

#
# evaluate Perl code as a function
#
sub f_perl {
  $_[1]->cd;			# Make sure we're in the correct directory
  join ' ', grep { defined } &eval_or_die;
}
*f_makeperl = \&f_perl;

#
# Mark targets as phony:
#
sub f_phony {
  #my ($str, $mkfile) = @_; # Name the arguments.

  undef file_info( unquote(), $_[1]{CWD} )->{IS_PHONY}
    for split_on_whitespace $_[0];

  $_[0];			# Just return our argument.
}

sub f_print {
  print "$_[0]\n";		# Print the text.
  $_[0];			# Just return it verbatim.
}

#
# Return a filename for a given file relative to the current directory.
# (Modified from Matthew Lovell's contribution.)
#
sub f_relative_filename {
  my( $files, $slash ) = split /, */, $_[0];
  my $cwd = $_[1]{CWD};
  join ' ',
    map {
      $_ = relative_filename file_info( unquote(), $cwd ), $cwd;
      !$slash || m@/@ ? $_ : "./$_"
    } split_on_whitespace $files;
}

#
# Return a filename relative to a given directory.
# Syntax: $(relative_to file1 file2, path/to/other/directory)
#
sub f_relative_to {
  my ($files, $dir, $slash, @extra_junk) = split(/, */, $_[0]);
  my $cwd = $_[1]{CWD};
  defined($dir) && @extra_junk == 0 or
    die "wrong number of arguments to \$(relative_to file, dir)\n";
  $dir =~ s/^\s+//;		# Trim whitespace.
  $dir =~ s/\s+$//;
  my $dirinfo = file_info unquote( $dir ), $cwd;
				# Directory this is relative to.
  join ' ',
    map {
      $_ = relative_filename file_info( unquote(), $cwd ), $dirinfo;
      !$slash || m@/@ ? $_ : "./$_"
    } split_on_whitespace $files;
}

sub f_shell {
  my ($str, $mkfile, $mkfile_line) = @_; # Name the arguments.

  local %ENV;			# Pass all exports to the subshell.
  $mkfile->setup_environment;

  $mkfile->cd;	# Make sure we're in the correct directory.
  my $shell_output = '';
  if( Mpp::is_windows ) {	# Doesn't support forking well?
    if( Mpp::is_windows != 1 ) {
      $shell_output = `$str`;	# Run the shell command.
    } else {			# ActiveState not using command.com, but `` still does
      my @cmd = format_exec_args $str;
      if( @cmd == 3 ) {		# sh -c
	substr $cmd[2], 0, 0, '"';
	$cmd[2] .= '"';
      }
      $shell_output = `@cmd`;
    }
    $? == 0 or
      warn "shell command `$str' returned `$?' at `$mkfile_line'\n";
  } else {
#
# We used to use perl's backquotes operators but these seem to have trouble,
# especially when doing parallel builds.  The backquote operator doesn't seem
# to capture all of the output.	 Every once in a while (sometimes more often,
# depending on system load and whether it's a parallel build) the backquote
# operator returns without giving any output, even though the shell command
# is actually executed; evidently it's finishing before it's captured all
# the output.  So we try a different approach here.
# This is about the third different technique that I've tried, and this one
# (finally) seems to work.  I'm still not 100% clear on why some of the
# other ones didn't.
#
    local (*INHANDLE, *OUTHANDLE); # Make a pair of file handles.
    pipe(INHANDLE, OUTHANDLE) or die "can't make pipe--$!\n";
    my $proc_handle = new Mpp::Event::Process sub { # Wait for process to finish.
      #
      # This is the child process.  Redirect our standard output to the pipe.
      #
      close INHANDLE;		# Don't read from the handle any more.
      close STDOUT;
      open(STDOUT,'>&OUTHANDLE') || die "can't redirect stdout--$!\n";
      exec(format_exec_args($str));
      die "exec $str failed--$!\n";
    }, ERROR => sub {
      warn "shell command `$str' returned `$_[0]' at `$mkfile_line'\n";
    };

    close OUTHANDLE;		# In parent, get rid of the output handle.
    my $line;
    my $n_errors_remaining = 3;
    for (;;) {
      my $n_chars = sysread(INHANDLE, $line, 8192); # Try to read.
      if (!defined($n_chars)) {	 # An error on the read?
	$n_errors_remaining-- > 0 and next; # Probably "Interrupted system call".
	die "read error--$!\n";
      }
      last if $n_chars == 0;	# No characters read--other process closed pipe.
      $shell_output .= $line;
    }
    wait_for $proc_handle; 	# Should not really be necessary.
    close INHANDLE;
  }
  $shell_output =~ s/\r?\n/ /g	# Get rid of newlines.
    unless $s_define;
  $shell_output =~ s/\s+$//s;	# Strip out trailing whitespace.
  $shell_output;
}

sub f_sort {
#
# Sort is documented to remove duplicates as well as to sort the string.
#
  my $last = '';
  join ' ', map { $last eq $_ ? () : ($last = $_) } sort split ' ', $_[0];
}

sub f_stem {
  defined $rule or die "\$(stem) or \$* used outside of rule\n";
  defined $rule->{PATTERN_STEM} and
    return $rule->{PATTERN_STEM};

  f_basename &f_target;		# If there's no stem, just strip off the
				# target's suffix.  This is what GNU make
				# does.
}

sub f_strip {
  join ' ', split ' ', $_[0];
}

sub f_subst {
  my ($from, $to, $text) = split(/,/, $_[0]);
  $from = quotemeta($from);
  join ' ', map { s/$from/$to/g; $_ } split ' ', $text;
}

sub f_suffix {
  join ' ', map { m@\.([^\./]*)$@ ? $1 : () } split ' ', $_[0];
}

#
# Mark targets as temporary:
#
sub f_temporary {
  #my ($str, $mkfile) = @_; # Name the arguments.

  for( split_on_whitespace $_[0] ) {
    file_info( unquote(), $_[1]{CWD} )->{IS_TEMP} = 1;
  }

  $_[0];			# Just return our argument.
}


sub f_wildcard {
  #my ($text, $mkfile) = @_; # Name the arguments.
  my $cwd = $rule ? $rule->build_cwd : $_[1]{CWD};
				# Get the default directory.

  join ' ', map zglob($_, $cwd), split ' ', $_[0];
}

sub f_word {
  my ($wordidx, $text) = split(/,\s*/, $_[0]);
  (split ' ', $text)[($wordidx < 0) ? $wordidx : $wordidx-1] || '';
}

sub f_wordlist {
  my ($startidx, $endidx, $text) = split(/,\s*/, $_[0]);
  if( defined $text ) {
    my @wordlist = split ' ', $text;
    $_ < 0 and $_ += @wordlist + 1 for $startidx, $endidx;

    # These are defined behaviors in GNU make, so we generate no warnings:
    return '' if $startidx > $endidx;
    $endidx = @wordlist if $endidx > @wordlist;

    join ' ', @wordlist[$startidx-1 .. $endidx-1];
  } else {
    join ' ', (split ' ', $endidx)[map { $_ > 0 ? $_ - 1 : $_ } split ' ', $startidx];
  }
}

sub f_words {
  # Must map split result, or implicit assignment to @_ takes place
  scalar map undef, split ' ', $_[0];
}

###############################################################################
#
# Define special automatic variables:
#
sub f_target {
  defined $rule or die "\$(target) or \$\@ used outside of rule\n";
  relative_filename $rule->{EXPLICIT_TARGETS}[$_[0] ? ($_[0] > 0 ? $_[0] - 1 : $_[0]) : 0],
    $rule->build_cwd;
}
*f_output = \&f_target;

sub f_targets {
  defined $rule or
    die "\$(targets) or \$(outputs) used outside of rule\n";
  join ' ', relative_filenames
    $_[0] ?
      [@{$rule->{EXPLICIT_TARGETS}}[map { $_ > 0 ? $_ - 1 : $_ } split ' ', $_[0]]] :
      $rule->{EXPLICIT_TARGETS};
}
*f_outputs = *f_targets;

sub f_dependency {
  defined $rule or
    die "\$(dependency) or \$(input) used outside of rule\n";
  my $finfo = $rule->{EXPLICIT_DEPENDENCIES}[$_[0] ? ($_[0] > 0 ? $_[0] - 1 : $_[0]) : 0];
  $finfo or return '';		# No dependencies.

  relative_filename $finfo, $rule->build_cwd;
}
*f_input = *f_dependency;

sub f_dependencies {
  defined $rule or
    die "\$(dependencies) or \$(inputs) or \$^ used outside of rule\n";
  join ' ', relative_filenames
    $_[0] ?
      [@{$rule->{EXPLICIT_DEPENDENCIES}}[map { $_ > 0 ? $_ - 1 : $_ } split ' ', $_[0]]] :
      $rule->{EXPLICIT_DEPENDENCIES};
}
*f_inputs = *f_dependencies;

#
# Return the list of inputs that have changed.  Note that this function
# should only be called in the action of a rule, which means that we're
# only called from find_all_targets_dependencies.
#
sub f_changed_inputs {
  defined $rule && defined $rule->{EXPLICIT_TARGETS} or
    die "\$(changed_inputs) or \$(changed_dependencies) or \$? used outside of rule action\n";

  my @changed_dependencies =
    $rule->build_check_method->changed_dependencies
      ($rule->{EXPLICIT_TARGETS}[0],
       $rule->signature_method,
       $rule->build_cwd,
       @{$rule->{EXPLICIT_DEPENDENCIES}});

  # Somehow we can't pass this to sort directly
  my @filenames = relative_filenames(@changed_dependencies);
  join ' ', sort @filenames;
}
*f_changed_dependencies = \&f_changed_inputs;

sub f_sorted_dependencies {
  defined $rule or die "\$(sorted_dependencies) or \$(sorted_inputs) or \$\^ used outside of rule\n";
  Mpp::Subs::f_sort join ' ', relative_filenames $rule->{EXPLICIT_DEPENDENCIES};
}
*f_sorted_inputs = *f_sorted_dependencies;

#
# Foreach is a little bit trick, since we have to support the new
# $(foreach) automatic variable, but also the old GNU make function
# foreach.  We can tell the difference pretty easily by whether we have
# any arguments.
#
sub f_foreach {
  my ($text, $mkfile, $mkfile_line) = @_; # Name the arguments.
  if ($text !~ /\S/) {		# No argument?
    defined $rule && defined $rule->{FOREACH} or
      die "\$(foreach) used outside of rule, or in a rule that has no :foreach clause\n";
    return relative_filename $rule->{FOREACH}, $rule->build_cwd;
  }

#
# At this point we know we're trying to expand the old GNU make foreach
# function.  The syntax is $(foreach VAR,LIST,TEXT), where TEXT is
# expanded once with VAR set to each value in LIST.  When we get here,
# because of some special code in expand_text, VAR,LIST,TEXT has not yet
# been expanded.
#
  my $comma = index_ignoring_quotes($text, ','); # Find the variable name.
  $comma >= 0 or
    die "$mkfile_line: $(foreach VAR,LIST,TEXT) called with only one argument\n";
  my $varname = $mkfile->expand_text(substr($text, 0, $comma));
				# Get the name of the variable.
  $varname =~ s/^\s+//;		# Strip off leading and trailing whitespace.
  $varname =~ s/\s+$//;

  $text = substr($text, $comma+1); # Get rid of the variable name.
  $comma = index_ignoring_quotes($text, ',');	# Find the next comma.
  $comma >= 0 or
    die "$mkfile_line: $(foreach VAR,LIST,TEXT) called with only two arguments\n";
  my $list = $mkfile->expand_text(substr($text, 0, $comma));
  $text = substr($text, $comma+1);

  my $ret_str = '';
  my $sep = '';
  $Mpp::Makefile::private ?
    (local $Mpp::Makefile::private->{PRIVATE_VARS}{$varname}) :
    (local $Mpp::Makefile::private);
  local $Mpp::Makefile::private->{VAR_REEXPAND}{$varname} = 0 if $Mpp::Makefile::private->{VAR_REEXPAND};
				# We're going to expand ourselves.  No need to
				# override this if there are no values,
				# leading to a false lookup anyway.
  for( split ' ', $list ) {	# Expand text:
    $Mpp::Makefile::private->{PRIVATE_VARS}{$varname} = $_;
				# Make it a private variable so that it
				# overrides even any other variable.
				# The local makes it so it goes away at the
				# end of the loop.
    $ret_str .= $sep . $mkfile->expand_text($text, $mkfile_line);
    $sep = ' ';			# Next time add a space
  }

  $ret_str;
}

sub f_warning {
  warn "$_[0] at `$_[2]'\n";	# Print the text.
  '';
}

sub f_xargs {
  my ($text, $mkfile, $mkfile_line) = @_; # Name the arguments.
  my $max_length = 1000;

  my $comma = index_ignoring_quotes($text, ','); # Find the command
  $comma >= 0 or
    die "$mkfile_line: $(xargs CMD,LIST) called with only one argument\n";
  my $command = $mkfile->expand_text(substr($text, 0, $comma));

  $text = substr($text, $comma+1); # Get rid of the variable name.
  $comma = index_ignoring_quotes($text, ',');	# Find the next comma.
  my $list;
  my $postfix = '';
  if($comma >= 0) {
    $list = $mkfile->expand_text(substr($text, 0, $comma));
    $text = substr($text, $comma+1);
    $postfix = $mkfile->expand_text($text, $mkfile_line);
  }
  else {
    $list = $mkfile->expand_text($text);
  }
  my @list = split(' ', $list);
  undef $list;
  my @pieces;
  while(@list) {
    my $piece = $command.' '.shift(@list);
    while(@list) {
      my $next = shift(@list);
      my $try .= $piece.' '.$next;
      if(length($try.' '.$postfix) > $max_length) {
        unshift(@list, $next);
        last;
      }
      $piece = $try;
    }
    push(@pieces, $piece.' '.$postfix);
  }
  join "\n", @pieces;
}

# Internal function for builtin rule on Windows.
# This is a hack hack hack to make a phony target xyz that indirectly depends on
# xyz.exe.  We must mark xyz as a phony target *after* we have associated
# a rule with the target, or else the rule will not work because makepp
# specifically rejects builtin rules for phony targets (to prevent disasters).
# (See code in set_rule().)  So we evaluate $(phony ) only after the
# rule has been set.  This kind of shenanigan is never necessary in normal
# makefiles because there are no special restrictions about rules from anywhere
# except this file
*f__exe_magic_ = sub {
  undef $rule->{EXPLICIT_TARGETS}[0]{IS_PHONY};
  my $exe_rule = file_info( $rule->{EXPLICIT_TARGETS}[0]{NAME} . '.exe', $rule->build_cwd )->get_rule;
  $exe_rule->{DEPENDENCY_STRING} .= " $rule->{DEPENDENCY_STRING}";
  f_make( $rule->{EXPLICIT_TARGETS}[0]{NAME} . '.exe',
	  $rule->{MAKEFILE},
	  $rule->{RULE_SOURCE} );
  '';
} if Mpp::is_windows;

#
# $(MAKE) needs to expand to the name of the program we use to replace a
# recursive make invocation.  We pretend it's a function with no arguments.
#
sub f_MAKE {
  require Mpp::Recursive;
  goto &f_MAKE;			# Redefined.
}

*f_MAKE_COMMAND = \&f_MAKE;

###############################################################################
#
# Makefile statements.	These are all called with the following arguments:
# a) The whole line of text (with the statement word removed).
# b) The makefile this is associated with.
# c) A printable string describing which line of the makefile the statement
#    was on.
#

#
# Define a build cache for this makefile.
#
sub s_build_cache {
  my ($arg, $mkfile, $mkfile_line) = @_;

  my $build_cache_fname = $mkfile->expand_text($arg, $mkfile_line);
  $build_cache_fname =~ s/^\s+//;
  $build_cache_fname =~ s/\s+$//; # Strip whitespace.

  if ($build_cache_fname eq 'none') { # Turn off build cache?
    $mkfile->{BUILD_CACHE} = undef;
  } else {
    $build_cache_fname = absolute_filename( file_info( $build_cache_fname, $mkfile->{CWD} ));
				  # Make sure we work even if cwd is wrong.

    eval { require Mpp::BuildCache };	# Load the build cache mechanism.
    $mkfile->{BUILD_CACHE} and
      die "You cannot define multiple build caches for a makefile.\n";
    $mkfile->{BUILD_CACHE} = new Mpp::BuildCache($build_cache_fname);
  }
}

#
# Build_check statement.
#
sub s_build_check {
  my( undef, $mkfile, $mkfile_line ) = @_;
  my $name = $mkfile->expand_text( $_[0], $mkfile_line );
  $name =~ s/^\s*(\w+)\s*$/$1/ or
    die "$mkfile_line: invalid build_check statement\n";
  if( $name eq 'default' ) {	# Return to the default method?
    delete $mkfile->{DEFAULT_BUILD_CHECK_METHOD};
    return;
  }
  $mkfile->{DEFAULT_BUILD_CHECK_METHOD} = eval "use Mpp::BuildCheck::$name; \$Mpp::BuildCheck::${name}::$name" ||
    eval "use BuildCheck::$name; \$BuildCheck::${name}::$name"; # TODO: provisional
  die "$mkfile_line: invalid build_check method $name\n"
    unless defined $mkfile->{DEFAULT_BUILD_CHECK_METHOD};
}

#
# Handle the no_implicit_load statement.  This statement marks some
# directories not to be loaded by the implicit load mechanism, in case
# there are makefiles there that you really don't want to load.
#
sub s_no_implicit_load {
  my ($text_line, $mkfile, $mkfile_line) = @_; # Name the arguments.

  $text_line = $mkfile->expand_text($text_line, $mkfile_line);
  my $cwd = $rule ? $rule->build_cwd : $mkfile->{CWD};
				# Get the default directory.

  local $Mpp::implicitly_load_makefiles = 0;
				# Temporarily turn off makefile loading for
				# the expansion of this wildcard.

  my @dirs = map zglob_fileinfo($_, $cwd),
    split(' ', $mkfile->expand_text($text_line, $mkfile_line));
				# Get a list of things matching the wildcard.
  foreach my $dir (@dirs) {
    is_or_will_be_dir $dir and $dir->{NO_IMPLICIT_LOAD} = 1;
				# Tag them so they don't load later.
  }
}

#
# Define statement.
# 5 args means we're called from Mpp::Makefile::parse_assignment, because the new form was used:
# define var +=		# or := etc.
#
sub s_define {
  my( $varname, $mkfile, $mkfile_line, $type, $override ) = @_; # Name the arguments.

  if( @_ < 5 ) {	      # If not called from Mpp::Makefile::parse_assignment.
#
# Parse the rest of the statement line.	 There should be a single word
# which is the name of the variable to define.
#
    $varname = $mkfile->expand_text( $varname, $mkfile_line );
				# Get the name of the variable being defined.
    $varname =~ s/^\s+//;	# Strip leading and trailing whitespace.
    $varname =~ s/\s+$//;
    $varname =~ /[\s:\#]/ and
      die "illegal variable \"$varname\" at $mkfile_line\n";
  }

#
# Read the remaining lines in.	Note that statements are executed while we're
# in the middle of reading the makefile, so we can grab the next line easily.
#
  my $var_value = '';
  local $_;
  local $s_define = 1;
  # GNU make only unites backslashed lines and looks for endef
  while( defined( $_ = Mpp::Makefile::read_makefile_line_stripped( 1, 1 ))) {
    /^\s*endd?ef\s*(?:$|#)/ and last;	# End of definition.
    $var_value .= $_;
  }
  chomp $var_value;
  Mpp::Makefile::assign( $mkfile, $varname, $type || 0, $var_value, $override, $mkfile_line, "\n" );
}

#
# Export statement.  If it contains an assignment that is handled by
# Mpp::Makefile::parse_assignment, which calls this only to mark it for export.
#
sub s_export {
  #my ($text_line, $mkfile, $mkfile_line) = @_; # Name the arguments.

  undef $_[1]{EXPORTS}{$_} for
    split ' ', $_[1]->expand_text( $_[0], $_[2] );
				# Mark these variables for export.  We'll
				# fill out their values later.
}

#
# Global statement.  If it contains an assignment that is handled by
# Mpp::Makefile::parse_assignment.
#
sub s_global {
  $Mpp::Makefile::global ||= {};
  my $reexpandref = $_[1]{VAR_REEXPAND};
  for( split ' ', $_[1]->expand_text( $_[0], $_[2] )) {
				# Mark these variables for export.  We'll
				# fill out their values later.
    (my $reexpand, ${"Mpp::global::$_"} ) = $_[1]->expand_variable( $_, $_[2], 1 );
    if( defined ${"Mpp::global::$_"} ) { # Maybe turning a local to global.
      undef ${"$_[1]{PACKAGE}::$_"};
      delete $reexpandref->{$_} if $reexpandref;
    } else {
      ${"Mpp::global::$_"} = '';	# Make it at least exist globally.
    }
    $Mpp::Makefile::global->{VAR_REEXPAND}{$_} = 1 if $reexpand;
  }
  delete $_[1]{VAR_REEXPAND} if $reexpandref && !%$reexpandref;
}

#
# Include statement:
#
sub s_include {
  my ($text_line, $mkfile, $mkfile_line) = @_;
				# Name the arguments.

  my @files = split ' ', $mkfile->expand_text( $text_line, $mkfile_line );
				# Get a list of files.
  my $cwd_devid;		# Remember what device this is mounted on
				# so we can avoid crossing file system boundaries.

  foreach my $file (@files) {
    my $file_makepp = "$file.makepp"; # Search for special makepp versions of
                                # files as well.
    my $finfo;
    for( my $dirinfo = $mkfile->{CWD};
	 $dirinfo &&
	 (stat_array $dirinfo)->[Mpp::File::STAT_DEV] ==
	   ($cwd_devid ||= (stat_array $mkfile->{CWD})->[Mpp::File::STAT_DEV]);
	 $dirinfo = $dirinfo->{'..'} ) { # Look in all directories above us.
      $finfo = file_info $file_makepp, $dirinfo;
      unless( Mpp::File::exists_or_can_be_built $finfo ) {
        $finfo = file_info $file, $dirinfo;
	next unless Mpp::File::exists_or_can_be_built $finfo;
      }
      wait_for prebuild( $finfo, $mkfile, $mkfile_line ) and
				# Build it if necessary, or link
				# it from a repository.
	die "can't build " . absolute_filename( $finfo ) . ", needed at $mkfile_line\n";
				# Quit if the build failed.
      last;
    }

#
# If it wasn't found anywhere in the directory tree, search the standard
# include files supplied with makepp.  We don't try to build these files or
# link them from a repository.
#
    unless( file_exists( $finfo )) { # Not found anywhere in directory tree?
      foreach (@{$mkfile->{INCLUDE_PATH}}) {
	$finfo = file_info($file, $_); # See if it's here.
	last if file_exists( $finfo );
      }
      file_exists( $finfo ) or
	die "makepp: can't find include file `$file'\n";
    }

    Mpp::log LOAD_INCL => $finfo, $mkfile_line
      if $Mpp::log_level;
    $mkfile->read_makefile($finfo); # Read the file.
  }
  '';
}

#
# This subroutine does exactly the same thing as include, except that it
# doesn't die with an error message if the file doesn't exist.
#
sub s__include {
  my ($text_line, $mkfile, $mkfile_line) = @_;
				# Name the arguments.
  foreach (split(' ', $mkfile->expand_text($text_line, $mkfile_line))) {
    eval { s_include($_, $mkfile, $mkfile_line) };
    $@ or next;                 # No error.
    $@ =~ /can\'t find include file/ or die $@; # Ignore not found errors.
  }

  $@ = '';			# Discard any error.
}

#
# Load one or several makefiles.
#
sub s_load_makefile {
  my ($text_line, $mkfile, $mkfile_line) = @_; # Name the arguments.

  my @words = split_on_whitespace($mkfile->expand_text($text_line, $mkfile_line));

  $mkfile->cleanup_vars;
  my %command_line_vars = %{$mkfile->{COMMAND_LINE_VARS}};
				# Extra command line variables.	 Start out
				# with a copy of the current command line
				# variables.
  my @include_path = @{$mkfile->{INCLUDE_PATH}};
				# Make a copy of the include path (so we can
				# modify it with -I).
#
# First pull out the variable assignments.
#
  my @makefiles;
  while (defined($_ = shift @words)) { # Any words left?
    if (/^(\w+)=(.*)/) {	# Found a variable?
      $command_line_vars{$1} = unquote($2);
    }
    elsif (/^-I(\S*)/) {	# Specification of the include path?
      unshift @include_path, ($1 || shift @words);
				# Grab the next word if it wasn't specified in
				# the same word.
    }
    else {			# Unrecognized.	 Must be name of a makefile.
      push @makefiles, $_;
    }
  }

  my $set_do_build = $Mpp::File::root->{DONT_BUILD} &&
    $Mpp::File::root->{DONT_BUILD} == 2 && # Was set implicitly through root makefile.
    !Mpp::File::dont_build( $mkfile->{CWD} );
				# Our dir is to be built, so propagate that to
				# loaded makefiles' dirs.
#
# Now process the makefiles:
#
  foreach (@makefiles) {
    s/^-F//;			# Support the archaic syntax that put -F
				# before the filename.
    my $mfile = file_info $_, $mkfile->{CWD};
				# Get info on the file.
    my $mdir = $mfile;		# Assume it is actually a directory.
    is_or_will_be_dir $mfile or $mdir = $mfile->{'..'};
				# Default directory is the directory the
				# makefile is in.
    if( $set_do_build && Mpp::File::dont_build( $mdir ) && $mdir->{DONT_BUILD} == 2 ) {
				# Inherited from '/'.
      my @descend = $mdir;
      while( @descend ) {
	my $finfo = shift @descend;
	next unless $finfo->{DONT_BUILD} && $finfo->{DONT_BUILD} == 2;
				# Not yet propagated from '/' or manually set?
	undef $finfo->{DONT_BUILD};
	push @descend, values %{$finfo->{DIRCONTENTS}} if $finfo->{DIRCONTENTS};
      }
    }
    Mpp::Makefile::load( $mfile, $mdir, \%command_line_vars, '', \@include_path,
		    $mkfile->{ENVIRONMENT} ); # Load the makefile.
  }
}

#
# Read a block either optionally indented {{ to }} or single braced.
# The latter must finish on the same line or at the very beginning of
# a following line.
#
sub read_block($) {
  my $code = $_[0];		# Name the arguments.
  my $re = ($code =~ /\{\{/) && qr/^\s*\}\}/;
				# {{ is stronger than } at EOL
  if ($re || $code !~ /\}\s*$/) { # Code is not entirely inline?
    $code .= "\n";		# Put the newline in that got removed.
    my $line;
    while (defined($line = &Mpp::Makefile::read_makefile_line)) { # Get the next line.
      $code .= $line;
      $re ||= ($line =~ /^\s*\{\{/s) ? qr/^\s*\}\}/ : qr/^\}/;
				# Give {{ a chance on 2nd line.
      last if $line =~ /$re/s;	# Stop at a brace at the left margin.
    }
  }
  $code;
}

#
# This function allows the user to do something in the makefile like:
# makeperl {
#   ... perl code
# }
#
sub s_makeperl { s_perl( @_, 1 ) }

#
# This function allows the user to do something in the makefile like:
# makesub subname {
#   ... perl code
# }
#
sub s_makesub { s_sub( @_, 1 ) }

#
# Begin a whole block of perl { } code.
#
sub s_perl {
  my ($perl_code, $mkfile, $mkfile_line, $expand) = @_;
				# Name the arguments.
  $perl_code = read_block $perl_code;
  $perl_code = $mkfile->expand_text($perl_code, $mkfile_line) if defined $expand;
  $mkfile->cd;		# Make sure we're in the correct directory
				# because some perl code will expect this.
  eval_or_die $perl_code, $mkfile, $mkfile_line;
}


#
# Begin a whole block of perl code.
#
sub s_perl_begin {
  my ($junk, $mkfile, $mkfile_line) = @_;
				# Name the arguments.
  my $perl_code = "\n";		# To get line numbers right in messages
  my $line;
  while (defined($line = &Mpp::Makefile::read_makefile_line)) { # Get the next line.
    last if $line =~ /^\s*perl_end\b/; # Found the terminator?
    $perl_code .= $line;
  }
  $mkfile->cd;		# Make sure we're in the correct directory
				# because some perl code will expect this.
  eval_or_die $perl_code, $mkfile, $mkfile_line;
}

#
# Built targets immediately.
# Useful when the list of targets depends on files that might be generated.
#
sub s_prebuild {
  my ($text_line, $mkfile, $mkfile_line) = @_;
  my (@words) = split_on_whitespace($mkfile->expand_text($text_line, $mkfile_line));

  &Mpp::maybe_stop;
  for my $target (@words) {
    my $finfo = file_info($target, $mkfile->{CWD});
    # TBD: If prebuild returns undef, then that could mean that the file
    # didn't need to be built, but it could also means that there was a
    # dependency loop. We ought to generate an error in the latter case.
    wait_for prebuild( $finfo, $mkfile, $mkfile_line ) and
      die "failed to prebuild $target\n";
  }
}
sub prebuild {
  my ($finfo, $mkfile, $mkfile_line ) = @_;
  my $myrule = Mpp::File::get_rule( $finfo );
  Mpp::log PREBUILD => $finfo, $mkfile_line
    if $Mpp::log_level;
  if($myrule && !UNIVERSAL::isa($myrule, 'Mpp::DefaultRule') &&
    !exists($finfo->{BUILD_HANDLE})
  ) {
    # If the file to be built is governed by the present Makefile, then
    # just initialize the Mpp::Makefile and build it based on what we know so far,
    # because then the file will *always* be built with the same limited
    # knowledge (unless there are multiple rules for it, in which case a
    # warning will be issued anyway). On the other hand, if the file is
    # governed by another Makefile that isn't fully loaded yet, then issue
    # a warning, because then you could get weird dependencies on the order in
    # which Makefiles were loaded. Note that this warning isn't guaranteed to
    # show up when it's called for, because targets that are built via direct
    # calls to Mpp::build() don't undergo this check.
    unless($myrule->makefile == $mkfile || $myrule->makefile->{INITIALIZED}) {
      warn 'Attempting to build ' . absolute_filename( $finfo ) .
	" before its makefile is completely loaded\n";
    }
  }
  Mpp::build($finfo);
}

#
# Register an autoload.
# Usage from the makefile:
#    autoload filename ...
#
sub s_autoload {
  my ($text_line, $mkfile, $mkfile_line) = @_; # Name the arguments.

  ++$Mpp::File::n_last_chance_rules;
  my (@fields) = split_on_whitespace($mkfile->expand_text($text_line, $mkfile_line));
  push @{$mkfile->{AUTOLOAD} ||= []}, @fields;
}

#
# Register an action scanner.
# Usage from the makefile:
#    register_scanner command_word scanner_subroutine_name
#
#
sub s_register_scanner {
  my ($text_line, $mkfile, $mkfile_line) = @_; # Name the arguments.

  my (@fields) = split_on_whitespace $mkfile->expand_text($text_line, $mkfile_line);
				# Get the words.
  @fields == 2 or die "$mkfile_line: invalid register_scanner line, need 2 arguments\n";
  my $command_word = unquote $fields[0]; # Remove quotes, etc.
  $fields[1] =~ tr/-/_/;
  my $scanner_sub = $fields[1] =~ /^(?:scanner_)?none$/ ?
    undef : (*{"$mkfile->{PACKAGE}::$fields[1]"}{CODE} || *{"$mkfile->{PACKAGE}::scanner_$fields[1]"}{CODE});
				# Get a reference to the subroutine.
  $mkfile->register_scanner($command_word, $scanner_sub);
}

#
# Register a command parser (alternate form of a scanner).
# Usage from the makefile:
#    register_command_parser command_word command_parser_class_name
#
#
sub s_register_command_parser {
  my( $text_line, $mkfile, $mkfile_line ) = @_; # Name the arguments.

  my (@fields) = split_on_whitespace $mkfile->expand_text( $text_line, $mkfile_line );
				# Get the words.
  @fields == 2 or die "$mkfile_line: invalid register_command_parser line\n";
  my $command_word = unquote $fields[0]; # Remove quotes, etc.
  my $class = unquote $fields[1];
  substr $class, 0, 0, 'Mpp::CommandParser::' unless $class =~ /^Mpp::CommandParser::/;
  my $scanner_sub = eval qq{
    sub {
      \$mkfile->cd;
      require $class;
      shift;
      return $class->new( \@_ );
    }
  } or die $@;
  $mkfile->register_scanner( $command_word, $scanner_sub );
}

#
# Register an input filename suffix for a particular command.
# Usage from the makefile:
#    register_input_suffix command_word suffix ...
#
sub s_register_input_suffix {
  my ($text_line, $mkfile, $mkfile_line) = @_; # Name the arguments.

  my( $command_word, @fields ) = # Get the words.
    unquote_split_on_whitespace($mkfile->expand_text($text_line, $mkfile_line));

  no strict 'refs';
  my $hashref = \%{$mkfile->{PACKAGE} . '::input_suffix_hash'};
  push @{$hashref->{$command_word} ||= []}, @fields;
}

#
# Load from repositories:
#
sub s_repository {
  require Mpp::Repository;
  goto &s_repository;		# Redefined.
}

#
# Add runtime dependencies for an executable.
#
sub s_runtime {
  my ($text, $mkfile, $mkfile_line) = @_; # Name the arguments.

  my $first_comma = index_ignoring_quotes($text, ','); # Find the command
  $first_comma >= 0 or
    die "$mkfile_line: runtime EXE,LIST called with only one argument\n";
  my $exelist = $mkfile->expand_text(substr($text, 0, $first_comma), $mkfile_line);
  substr $text, 0, $first_comma+1, ''; # Get rid of the variable name.
  my @deps = map file_info($_, $mkfile->{CWD}), split_on_whitespace($mkfile->expand_text($text, $mkfile_line));
  for my $exe ( map file_info($_, $mkfile->{CWD}), split_on_whitespace( $exelist )) {
    for my $dep (@deps) {
      $exe->{RUNTIME_DEPS}{$dep} = $dep;
    }
  }
}

#
# Set the default signature method for all rules in this makefile:
#
sub s_signature {
  my( undef, $mkfile, $mkfile_line ) = @_;
  my $name = $mkfile->expand_text( $_[0], $mkfile_line );
  $name =~ s/^\s*(\w+)\s*$/$1/ or
    die "$mkfile_line: invalid signature statement\n";
  if( $name eq 'default' ) {	# Return to the default method?
    delete $mkfile->{DEFAULT_SIGNATURE_METHOD}; # Get rid of any previous
				# stored signature method.
    return;
  }
  $mkfile->{DEFAULT_SIGNATURE_METHOD} = eval "use Mpp::Signature::$name; \$Mpp::Signature::${name}::$name" ||
    eval "use Signature::$name; \$Signature::${name}::$name"; # TODO: provisional
  unless( defined $mkfile->{DEFAULT_SIGNATURE_METHOD} ) {
#
# The signature methods and build check methods used to be the same thing,
# so for backward compatibility, see if this is actually a build check
# method.
#
    $mkfile->{DEFAULT_BUILD_CHECK_METHOD} = eval "use Mpp::BuildCheck::$name; \$Mpp::BuildCheck::${name}::$name" ||
      eval "use BuildCheck::$name; \$BuildCheck::${name}::$name"; # TODO: provisional
    if( defined $mkfile->{DEFAULT_BUILD_CHECK_METHOD} ) {
      warn "$mkfile_line: requesting build check method $name via signature is deprecated.\n";
    } else {
      die "$mkfile_line: invalid signature method $name\n";
    }
  }
}

#
# This function allows the user to do something in the makefile like:
# sub subname {
#   ... perl code
# }
#
sub s_sub {
  my ($subr_text, $mkfile, $mkfile_line, $expand) = @_;
				# Name the arguments.
  $subr_text = read_block $subr_text;
  $subr_text = $mkfile->expand_text($subr_text, $mkfile_line) if defined $expand;
  eval_or_die "sub $subr_text", $mkfile, $mkfile_line;
}

#
# Don't export a variable to child processes.
#
sub s_unexport {
  my ($text_line, $mkfile, $mkfile_line) = @_;
				# Name the arguments.
  return unless $mkfile->{EXPORTS};

  delete @{$mkfile->{EXPORTS}}{split ' ', $mkfile->expand_text($text_line, $mkfile_line)}
				# Look at each variable listed.
}


#
# Execute an external Perl script within the running interpreter.
#
sub run(@) {
  local( $0, @ARGV ) = @_;		# Name the arguments.
  $0 = f_find_program $0,
    $rule ? $rule->{MAKEFILE} : $makefile,
    $rule ? $rule->{RULE_SOURCE} : $makefile_line
    unless -f $0;		# not relative or absolute
  local $SIG{__WARN__} = local $SIG{__DIE__} = 'DEFAULT';
  die $@ || "$0 failed--$!\n"
    if !defined do $0 and $@ || $!;
}

###############################################################################
#
# Default values of various variables.	These are implemented as functions
# with no arguments so that:
# a) They are visible to all makefiles, yet are easily overridden.
#    (If we just put them in makepp_builtin_rules.mk, then they are not
#    visible in the makefile except in rules, because makepp_builtin_rules.mk
#    is loaded after the makefile.  That's where they were for a while but
#    that was discovered not to work well.)
# b) The $(origin ) function can work with them.
#
sub f_AR()	{ 'ar' }
sub f_ARFLAGS()	{ 'rv' }
sub f_AS()	{ 'as' }
my $CC;
sub f_CC	{ $CC ||=
		    $_[1]->expand_expression('find_program gcc egcc pgcc c89 cc' . (Mpp::is_windows?' cl bcc32':''), $_[2]) }
sub f_CFLAGS	{ $_[1]->expand_expression('if $(filter %gcc, $(CC)), -g -Wall, ' . (Mpp::is_windows?' $(if $(filter %cl %cl.exe %bcc32 %bcc32.exe, $(CC)), , -g)':'-g'), $_[2]) }
sub f_CURDIR	{ absolute_filename( $_[1]{CWD} ) }
my $CXX;
sub f_CXX	{ $CXX ||= $_[1]->expand_expression('find_program g++ c++ pg++ cxx ' . (Mpp::is_windows?'cl bcc32':'CC aCC'), $_[2]) }
sub f_CXXFLAGS	{ $_[1]->expand_expression('if $(filter %g++ %c++, $(CXX)), -g -Wall, ' . (Mpp::is_windows?' $(if $(filter %cl %cl.exe %bcc32 %bcc32.exe, $(CXX)), , -g)':'-g'), $_[2]) }
my $F77;
sub f_F77	{ $F77 ||= $_[1]->expand_expression('find_program f77 g77 fort77', $_[2]) }
sub f_FC	{ $_[1]->expand_variable('F77', $_[2]) }
my $LEX;
sub f_LEX	{ $LEX ||= $_[1]->expand_expression('find_program lex flex', $_[2]) }
sub f_LIBTOOL()	{ 'libtool' }
sub f_LD()	{ 'ld' }
sub f_MAKEINFO() { 'makeinfo' }
*f_PWD = \&f_CURDIR;
# Can't use &rm -f, because it might get used in a complex Shell construct.
sub f_RM()	{ 'rm -f' }
my $YACC;
sub f_YACC	{ $YACC ||= $_[1]->expand_expression('if $(filter bison, $(find_program yacc bison)), bison -y, yacc', $_[2]) }

sub f_ROOT	{ $_[1]{CWD}{ROOT} ? relative_filename( $_[1]{CWD}{ROOT}, $_[1]{CWD} ) : '' }

# Don't use Exporter so we don't have to keep a huge list.
sub import() {
  my $package = caller;
  no warnings 'redefine';	# In case we are reimporting this
  for( keys %Mpp::Subs:: ) {
    $_[1] ? /^(?:$_[1])/ : /^[fs]_/ or # functions and statements only
      /^run/ or
      /^scanner_/ or
      next;
    my $coderef = *{"Mpp::Subs::$_"}{CODE};
    *{$package . "::$_"} = $coderef if $coderef;
  }
}

1;
