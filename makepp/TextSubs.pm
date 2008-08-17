# $Id: TextSubs.pm,v 1.39 2008/08/04 21:57:01 pfeiffer Exp $
package TextSubs;
require Exporter;
@ISA = qw(Exporter);

@EXPORT = qw(index_ignoring_quotes max_index_ignoring_quotes
	     split_on_whitespace join_with_protection split_on_colon
	     split_commands unquote unquote_split_on_whitespace
	     requote format_exec_args whitespace_len hash_neq
	     is_cpp_source_name is_object_or_library_name);

use Config;

# Centrally provide constants which are needed repeatedly for aliasing, since
# perl implements them as subs, and each sub takes about 1.5kb RAM.
BEGIN {
  eval "sub CONST$_() { $_ }" for 0..6; # More are defined in BuildCacheControl.pm
  *::is_perl_5_6 = $] < 5.008 ? \&CONST1 : \&CONST0;
  *::is_windows =
    $^O eq 'cygwin' ? sub() { -1 } : # Negative for Unix like
    $^O eq 'msys' ? sub() { -2 } :   # MinGW with sh & coreutils
    $^O =~ /^MSWin/ ? (exists $ENV{SHELL} && $ENV{SHELL} =~ /sh(?:\.exe)?$/i ? \&CONST1 : \&CONST2) :
    \&CONST0;

  my $perl = $ENV{PERL};
  if( $perl && -x $perl ) {	# Overridden successfully.
  } elsif( -x $^X ) {		# Use same as ourself.
    $^X =~ tr/\\/\// if ::is_windows() > 0;
    $perl = (::is_windows() ? $^X =~ /^(?:\w:)?\// : $^X =~ /^\//) ?
      $^X :
      eval "use Cwd; cwd . '/$^X'";
  } else {			# Emergency fallback.
    $perl = $Config{perlpath};	# Prefer appended version number for precision.
    my $version = sprintf '%vd', $^V;
    $perl .= $version if -x "$perl$version";
  }
  eval "sub ::PERL() { '$perl' }";
}

#
# This module contains a few subroutines for manipulating text, mostly for
# dealing with quoted strings and make expressions.
#

=head2 pattern_substitution

  @pieces = pattern_substitution($pattern, $dest, @words)

Performs a pattern substitution like the C<$(patsubst )> function (in fact,
C<$(patsubst )> is implemented using this.  $pattern contains a C<%> as a
wildcard, and $dest contains a matching C<%>.  The substitution is applied to
each word in @words, and the result returned as an array.

For example:

  @pieces = pattern_substitution('%.c', '%.o', 'file1.c', 'file2.c')

returns ('file1.o', 'file2.o').

=cut

sub pattern_substitution {
  my ($src, $dest, @words) = @_; # Name the arguments.
  my $percent_pos = index($src, '%'); # Find the percent char.
  $percent_pos < 0 and
    die "\$(patsubst ...) called with '$src' as first argument\n";

  my $src_prefix = substr($src, 0, $percent_pos);
  my $src_suffix = substr($src, $percent_pos+1);

  $dest =~ /%/ or
    die "\$(patsubst ...) called with '$dest' as second argument\n";

  my @ret_words;
  foreach (@words) {
    my $stem_len = length() - length($src_prefix) - length($src_suffix);
    if ($stem_len >= 0 &&	# Make sure prefix & suffix don't overlap.
	substr($_, 0, length($src_prefix)) eq $src_prefix &&
	substr($_, length($_)-length($src_suffix)) eq $src_suffix) {
      my $pattern_stem = substr $_, length($src_prefix), $stem_len;
      my $dest_copy;
      ($dest_copy = $dest) =~ s/%/$pattern_stem/g;
				# Replace all occurences of % with the stem.
      push @ret_words, $dest_copy;
				# Save the resulting word(s).  There may be
				# more than one if $dest contains spaces.
      defined($Makesubs::rule) and
	$Makesubs::rule->{PATTERN_STEM} = $pattern_stem;
				# Set it up so $* can return the stem.
    } else {
      push @ret_words, $_;	# If the pattern doesn't match, then we copy
				# it without modification to the output.
    }
  }

  @ret_words;
}

# Rather than cascade if( /\Gx/gc ), just look up the action
my %skip_over = (
  "'", \&skip_over_squote,
  '"', \&skip_over_dquote,
  '$', \&skip_over_make_expression,
  '\\', sub { ++pos });

=head2 index_ignoring_quotes

  my $index = index_ignoring_quotes($string, 'substr');

Works like C<index($string, 'substr')>, except that the substring may not be
inside quotes or a make expression.

=cut

sub index_ignoring_quotes {
  my $substr = $_[1];
  local $_ = $_[0];
  pos = 0;			# Start at the beginning.

  for (;;) {
    my $last_pos = pos;
    if( /\G([^"'\\\$]+)/gc ) { # Just ordinary characters?
      my $idx = index $1, $substr; # See if it's in those characters.
      $idx >= 0 and return $last_pos + $idx;
    }

    return -1 if length() <= pos; # End of string?  That means no match.
				# For reasons that I don't understand, testing
				# for /\G\z/gc doesn't work here.

    # It's one of the standard cases ", ', \ or $.
    &{$skip_over{substr $_, pos()++, 1}};
  }
}

=head2 max_index_ignoring_quotes

Like C<index_ignoring_quotes>, except that it returns the index to the last
instance rather than the first.

=cut
sub max_index_ignoring_quotes {
  use strict;
  my ($str, $sub) = @_;
  my $max = 0;
  my $len = length($sub) or return 0;
  while((my $next = index_ignoring_quotes $str, $sub) >= 0) {
    $max += $next + $len;
    substr $str, 0, $next + $len, '';
  }
  return $max ? $max - $len : -1;
}

=head2 split_on_whitespace

  @pieces = split_on_whitespace($string);

Works just like

  @pieces = split(' ', $string)

except that whitespace inside quoted strings is not counted as whitespace.
This should be called after expanding all make variables; it does not know
anything about things like "$(make expressions)".

There are three kinds of quoted strings, as in the shell.  Single quoted
strings are terminated by a matching single quote.  Double quoted strings are
terminated by a matching double quote that isn't escaped by a backslash.
Backquoted strings are terminated by a matching backquote that isn't escaped
by a backslash.

=cut

sub unquote_split_on_whitespace {
  # Can't call unquote when pushing because both use \G and at least in 5.6
  # localizing $_ doesn't localize \G
  map unquote(), &split_on_whitespace;
}
sub split_commands {
  split_on_whitespace( $_[0], 1 );
}
sub split_on_whitespace {
  my @pieces;
  my $cmds = @_ > 1;
  local $_ = $_[0];

  pos = 0;			# Start at the beginning.
  $cmds ? /^[;|&]+/gc : /^\s+/gc;			# Skip over leading whitespace.
  my $last_pos = pos;

  for (;;) {
    $cmds ? /\G[^;|&"'`\\\$]+/gc : /\G[^\s"'\\]+/gc;	# Skip over irrelevant things.

    last if length() <= pos;	# End of string.

    my $cur_pos = pos;		# Remember the current position.
    if ($cmds && /\G(?<=[<>])&/gc) {	# Skip over redirector, where & is not a separator
    } elsif ($cmds ? /\G[;|&]+/gc : /\G\s+/gc) { # Found some whitespace?
      push(@pieces, substr($_, $last_pos, $cur_pos-$last_pos));
      $last_pos = pos;		# Beginning of next string is after this space.
    } elsif (!$cmds and /\G"/gc) { # Double quoted string?
      while (pos() < length) {
	next if /\G[^\\"]+/gc;	# Skip everything except quote and \.
	/\G"/gc and last;	# We've found the end of the string.
	pos() += 2;		# Skip char after backslash.
      }
    } elsif (/\G'[^']*'/gc) {	# Skip until end of single quoted string.
    } elsif (/\G`/gc) {		# Back quoted string?
      while (pos() < length) {
	next if /\G[^\\`]+/gc;	# Skip everything except quote and \.
	/\G`/gc and last;	# We've found the end of the string.
	pos() += 2;		# Skip char after backslash.
      }
    } else {			# It's one of the standard cases ", \ or $.
      # $ only gets here in commands, where we use the similarity of make expressions
      # to skip over $(cmd; cmd), $((var|5)), ${var:-foo&bar}.
      # " only gets here in commands, where we need to catch nested things like
      # "$(cmd "foo;bar")"
      &{$skip_over{substr $_, pos()++, 1}};
    }
  }

  if (length() > $last_pos) {	# Anything left at the end of the string?
    push @pieces, substr($_, $last_pos);
  }

  @pieces;
}

=head2 join_with_protection

  $string = join_with_protection(@pieces);

Works just like

  $string = join(' ', @pieces)

except that strings in @pieces that contain shell metacharacters are protected
from the shell.

=cut

sub join_with_protection {
  join ' ',
    map {
      $_ eq '' ? "''" :
      /'/ ? map { s/'/'\\''/g; "'$_'" } "$_" : # Avoid modifying @_
      m|[^\w/.@%\-+=:]| ? "'$_'" :
      $_;
    } @_;
}

=head2 split_on_colon

  @pieces = split_on_colon('string');

This subroutine is equivalent to

  @pieces = split(/:+/, 'string');

except that colons inside double quoted strings or make expressions are passed
over.  Also, a semicolon terminates the expression; any colons after a
semicolon are ignored.	This is to support parsing of this horrible rule:

  $(srcdir)/cat-id-tbl.c: stamp-cat-id; @:

=cut

sub split_on_colon {
  my @pieces;

  local $_ = $_[0];
  my $last_pos = 0;
  pos = 0;			# Start at the beginning.

  for (;;) {
    /\G[^;:"'\\\$]+/gc;		# Skip over irrelevant stuff.
    last if length() <= pos;	# End of string?
				# For reasons that I don't understand, testing
				# for /\G\z/gc doesn't work here.

    if (/\G(:+)/gc) {		# Found our colon?
      push @pieces, substr $_, $last_pos, pos() - $last_pos - length $1;
      $last_pos = pos;		# Beginning of next string is after this space.
    } elsif (/\G;/gc) {		# Found end of the rule?
      pos = length;		# Don't look for any more colons.
    } else {			# It's one of the standard cases ", ', \ or $.
      &{$skip_over{substr $_, pos()++, 1}};
    }
  }

  if (length() > $last_pos) {	# Anything left at the end of the string?
    push @pieces, substr($_, $last_pos);
  }

  @pieces;
}


#
# This routine splits the PATH according to the current systems syntax.  An
# object may be optionally passed.  If that contains a non-empty entry {PATH},
# that is used instead of $ENV{PATH}.  Empty elements are returned as '.'.
# A second optional argument may be an alternative string to 'PATH'.
# A third optional argument may be an alternative literal path.
#
sub split_path {
  my $var = $_[1] || 'PATH';
  my $path = $_[2] || ($_[0] && $_[0]{$var} || $ENV{$var});
  if( ::is_windows ) {
    map { tr!\\"!/!d; $_ eq '' ? '.' : $_ }
      ::is_windows > 0 ?
	split( /;/, "$path;" ) :	# "C:/a b";C:\WINNT;C:\WINNT\system32
	split_on_colon( "$path:" );	# "C:/a b":"C:/WINNT":/cygdrive/c/bin
  } else {
    map { $_ eq '' ? '.' : $_ } split /:/, "$path:";
  }
}

#
# This routine is used to skip over a make expression.	A make expression
# is a variable, like "$(CXX)", or a funtion, like $(patsubst %.o, %.c, sdaf).
#
# The argument should be passed in the global variable $_ (not @_, as usual),
# and pos($_) should be the character immediately after the dollar sign.
# On return, pos($_) is the first character after the end of the make
# expression.
#
# This returns the length of the opening parens, i.e.: $@ = 0; $(VAR) = 1 and
# $((perl ...)) = 2, or undef if the closing parens don't match.
#
sub skip_over_make_expression {
  my( $nonre, $endre );
  if (/\G\(/gc) {		# Does the expression begin with $(?
    $nonre = qr/[^)"'\$]/;
    $endre = qr/\)/;
  } elsif (/\G\{/gc) {		# Does the expression begin with ${?
    $nonre = qr/[^}"'\$]/;
    $endre = qr/\}/;
  } elsif (/\G\[/gc) {		# Does the expression begin with $[?
    $nonre = qr/[^]"'\$]/;
    $endre = qr/\]/;
  } else {
    ++pos;			# Must be a single character variable.	Just
				# skip over it.
    return 0;
  }

  my $double = //gc || 0;	# Does the expression begin with $((, ${{ or $[[?

  if( /\G(?:perl|map())\s+/gc ) { # Is there plain Perl code we must skip blindly?
    if( defined $1 ) {		  # The first arg to map is normal make stuff.
      /\G[^"'\$,]/gc or &{$skip_over{substr $_, pos()++, 1}}
	while !/\G,/gc;
    }
    $double ? /\G.*?$endre$endre/gc : /\G.*?$endre/gc;
    return $double + 1;
  }

  for (;;) {
    /\G$nonre+/gc;		# Skip over irrelevant things.
    last if length() <= pos;	# Quit if end of string.  (Testing for \z
				# seems unreliable.)
    if( /\G$endre/gc ) {
      return $double + 1 if !$double or //gc; # Quit if closing parens.
      ++pos;			# A simple ) within $(( )) or } within ${{ }}
    } else {			# It's one of the standard cases ", ' or $.
      &{$skip_over{substr $_, pos()++, 1}};
    }
  }
  undef;
}


#
# This subroutine is used to skip over a double quoted string.	A double
# quoted string may have a make expression inside of it; we also skip over
# any such nested make expressions.
#
# The argument should be passed in the global variable $_ (not @_, as usual),
# and pos($_) should be the character immediately after the quote.
# On return, pos($_) is the first character after the closing quote.
#
sub skip_over_dquote {
  for (;;) {
    /\G[^"\\\$]+/gc;		# Skip over irrelevant characters.

    last if length() <= pos;	# Quit if end of string.  (Testing for \z
				# seems unreliable.)
    /\G"/gc and last;		# Found the closing quote.

    # It's one of the standard cases \ or $.
    &{$skip_over{substr $_, pos()++, 1}};
  }
}

#
# This subroutine is used to skip over a single quoted string.	A single
# quoted string may have a make expression inside of it; we also skip over
# any such nested make expressions.  The difference between a single and double
# quoted string is that a backslash is used to escape special chars inside
# a double quoted string, whereas it has no meaning in a single quoted string.
#
# The argument should be passed in the global variable $_ (not @_, as usual),
# and pos($_) should be the character immediately after the quote.
# On return, pos($_) is the first character after the closing quote.
#
sub skip_over_squote {
  for (;;) {
    /\G[^'\\\$]+/gc;		# Skip over irrelevant characters.

    last if length() <= pos;	# Quit if end of string.  (Testing for \z
				# seems unreliable.)
    /\G'/gc and last;		# Found the closing quote.

    # It's one of the standard cases \ or $.
    &{$skip_over{substr $_, pos()++, 1}};
  }
}

=head2 unquote

  $text = unquote($quoted_text)

Removes quotes and escaping backslashes from a name.  Thus if you give it as
an argument
    \""a bc"'"'

it will return the string

    "a bc"

You must already have expanded all of the make variables in the string.
unquote() knows nothing about make expressions.

=cut

sub unquote {
  my $ret_str = '';

  local $_ = $_[0] if @_;
  pos = 0;			# Start at beginning of string.

  for (;;) {
    /\G([^"'\\]+)/gc and $ret_str .= $1; # Skip over ordinary characters.
    last if length() <= pos;

    if (/\G"/gc) {		# Double quoted section of the string?
      for (;;) {
	/\G([^"\\]+)/gc and $ret_str .= $1; # Skip over ordinary chars.
	if( /\G\\/gc ) {	# Handle quoted chars.
	  if( length() <= pos ) {
	    die "single backslash at end of string '$_'\n";
	  } else {		# Other character escaped with backslash.
	    $ret_str .= substr $_, pos()++, 1; # Put it in verbatim.
	  }
	} else {
	  last if length() <= pos || # End of string w/o matching quote.
	    ++pos;		# Skip quote.
	}
      }
    } elsif (/\G'/gc) {		# Single quoted string?
      /\G([^']+)/gc and $ret_str .= $1; # Copy up to terminating quote.
      last if length() <= pos;	# End of string w/o matching quote.
      ++pos;			# Or skip quote.
    } else {
      ++pos;			# Must be '\', skip it
      if( length() <= pos ) {
	die "single backslash at end of string '$_'\n";
      } elsif (/\G([0-7]{1,3})/gc) { # Octal character code?
	$ret_str .= chr oct $1;	# Convert the character to binary.
      } elsif (/\G([*?[\]])/gc) { # Backslashed wildcard char?
				# Don't weed out backslashed wildcards here,
				# because they're recognized separately in
				# the wildcard routines.
	$ret_str .= '\\' . $1;	# Leave the backslash there.
      } else {			# Other character escaped with backslash.
	$ret_str .= substr $_, pos()++, 1; # Put it in verbatim.
      }
    }
  }

  $ret_str;
}

=head2 requote

  $quoted_text = requote($unquoted_text);

Puts quotes around the text, and escapes any quotes inside the text, so
that calling unquote() on $quoted_text will return the same string as
$unquoted_text.

=cut

sub requote {
  my( $str ) = @_;		# Get a modifiable copy of the string.
  $str =~ s/(["\\])/\\$1/g;	# Protect all backslashes and double quotes.
  $str =~ s{([\0-\037])}{sprintf '\%o', ord $1}eg; # Protect any binary characters.
  qq["$str"];			# Return the quoted string.
}

#
# Perl contains an optimization where it won't run a shell if it thinks the
# command has no shell metacharacters.	However, its idea of shell
# metacharacters is a bit too limited, since it doesn't realize that something
# like "XYZ=abc command" does not mean to execute the program "XYZ=abc".
# Also, perl's system command doesn't realize that ":" is a valid shell
# command.  So we do a bit more detailed check for metacharacters and
# explicitly pass it off to a shell if needed.
#
# This subroutine takes a shell command to execute, and returns an array
# of arguments suitable for exec() or system().
#
sub format_exec_args {
  my( $cmd ) = @_;
  return $cmd			# No Shell available.
    if ::is_windows > 1;
  if( ::is_windows == 1 && $cmd =~ /[%"\\]/ ) { # Despite multi-arg system(), these chars mess up command.com
    require Makesubs;
    my $tmp = Makesubs::f_mktemp( '' );
    open my $fh, '>', $tmp;
    print $fh $cmd;
    return ($ENV{SHELL}, $tmp);
  }
  return ($ENV{SHELL}, '-c', $cmd)
    if ::is_windows == -2 || ::is_windows == 1 ||
      $cmd =~ /[()<>\\"'`;&|*?[\]]/ || # Any shell metachars?
      $cmd =~ /\{.*,.*\}/ || # Pattern in Bash (blocks were caught by ';' above).
      $cmd =~ /^\s*(?:\w+=|[.:!]\s|e(?:val|xec|xit)\b|source\b|test\b)/;
				# Special commands that only
				# the shell can execute?

  return $cmd;			# Let perl do its optimization.
}

#
# Compute the length of whitespace when it may be composed of spaces or tabs.
# The leading whitespace is removed from $_.
# Usage:
#	$len = strip_indentation;
#
# If $_ is not all tabs and spaces, returns the length of the
# whitespace up to the first non-white character.
#

sub strip_indentation() {
  my $white_len = 0;
  pos = 0;			# Start at the beginning of the string.
  while( /\G(?:( +)|(\t+))/gc ) {
    if( $1 ) {			# Spaces?
      $white_len += length $1;
    } else {			# Move over next tab stops.
      $white_len = ($white_len + 8*length $2) & ~7;
				# Cheap equivalent for 8*int(.../8)
    }
  }
  substr $_, 0, pos, '';
  $white_len;
}

=head2 hash_neq

  if (hash_neq(\%a, \%b)) { ... }

Returns true (actually, returns the first key encountered that's different) if
the two associative arrays are unequal, and '' if not.

=cut

sub hash_neq {
  my ($a, $b) = @_;
#
# This can't be done simply by stringifying the associative arrays and
# comparing the strings (e.g., join(' ', %a) eq join(' ', %b)) because
# the order of the key/value pairs in the list returned by %a may differ
# even for identical hashes (if two keys happen to land in the same bucket).
#
  my %a_not_b = %$a;		# Make a modifiable copy of one of them.
  foreach (keys %$b) {
    !exists($a_not_b{$_}) and return $_ || '0_'; # Must return a true value.
    $a_not_b{$_} eq $b->{$_} or return $_ || '0_';
    delete $a_not_b{$_};	# Remember which things we've compared.
  }

  if (scalar %a_not_b) {	# Anything left over?
    return (%a_not_b)[0] || '0_'; # Return the first key value.
  }
  '';				# No difference.
}

=head2 is_cpp_source_name

  if (is_cpp_source_name($filename))  { ... }

Returns true if the given filename has the appropriate extension to be
a C or C++ source or include file.

=cut

# NOTE: NVIDIA uses ".pp" for generic files (not necessarily programs)
# that need to pass through cpp.
sub is_cpp_source_name {
  $_[0] =~ /\.(?:[ch](|[xp+])\1|([chp])\2|moc|x[bp]m|idl|ii?|mi)$/i;
				# i, ii, and mi are for the GNU C preprocessor
				# (see cpp(1)).	 moc is for qt.
}

=head2 is_object_or_library_name

  if (is_object_or_library_name($filename)) { ... }

Returns true if the given filename has the appropriate extension to be some
sort of object or library file.

=cut

sub is_object_or_library_name {
  $_[0] =~ /\.(?:l?[ao]|s[aol](?:\.[\d.]+)?)$/;
}

=head2 getopts

  getopts %vars, strictflag, [qw(o optlong), \$var, wantarg, handler], ...

Almost as useful as Getopt::Long and much smaller :-)

%vars is optional, any VAR=VALUE pairs get stored in it if passed.

strictflag is optional, means to stop at first non-option.

Short opt may be empty, longopt may be a regexp (grouped if alternative).

$var gets incremented for each occurence of this option or, if optional
wantarg is true, it gets set to the argument.  This can be undef if you don't
need it.

If an optional handler is given, it gets called after assigning $var, if it is
a ref (a sub).  Any other value is assigned to $var.

=cut

my $args;
my $argfile =
  ['A', qr/arg(?:ument)?s?[-_]?file/, \$args, 1,
   sub {
     open my $fh, $args or die "$0: cannot open args-file `$args'--$!\n";
     local $/;
     unshift @ARGV, unquote_split_on_whitespace <$fh>;
     close $fh;
   }];
sub getopts(@) {
  my $hash = 'HASH' eq ref $_[0] and
    my $vars = shift;
  my $mixed = ref $_[0]
    or shift;
  my( @ret, %short );
  while( @ARGV ) {
    my $opt = shift @ARGV;
    if( $opt =~ s/^-(-?)// ) {
      my $long = $1;
      if( $opt eq '' ) {	# nothing after -(-)
	if( $long ) {		# -- explicit end of opts
	  unshift @ARGV, @ret;
	  return;
	}
	push @ret, '-';		# - stdin; TODO: this assumes $mixed
	next;
      }
    SPECS: for my $spec ( @_, $argfile, undef ) {
	die "$0: unknown option -$long$opt\n" unless defined $spec;
	if( $long ) {
	  if( $$spec[3] ) {
	    next unless $opt =~ /^$$spec[1](?:=(.*))?$/;
	    ${$$spec[2]} = defined $1 ? $1 : @ARGV ? shift @ARGV :
	      die "$0: no argument to --$opt\n";
	  } else {		# want no arg
	    next unless $opt =~ /^$$spec[1]$/;
	    ${$$spec[2]}++;
	  }
	} else {		# short opt
	  next unless $$spec[0] && $opt =~ s/^$$spec[0]//;
	  if( $$spec[3] ) {
	    ${$$spec[2]} = $opt ne '' ? $opt : @ARGV ? shift @ARGV :
	      die "$0: no argument to -$$spec[0]\n";
	    $opt = '';
	  } else {
	    ${$$spec[2]}++;
	  }
	  print STDERR "$0: -$$spec[0] is short for --"._getopts_long($spec)."\n"
	    if $::verbose && !$short{$$spec[0]};
	  $short{$$spec[0]} = 1;
	}
	ref $$spec[4] ? &{$$spec[4]} : (${$$spec[2]} = $$spec[4]) if exists $$spec[4];
	goto SPECS if !$long && length $opt;
	last;
      }
    } elsif( $hash and $opt =~ /^(\w[-\w.]*)=(.*)/ ) {
      $vars->{$1} = $2;
    } elsif( $mixed ) {
      push @ret, $opt;
    } else {
      unshift @ARGV, $opt;
      return;
    }
  }
  @ARGV = @ret;
}

# Transform regexp to be human readable.
sub _getopts_long($) {
  my $str = "$_[0][1]";
  $str =~ s/.*?://;		# remove qr// decoration
  $str =~ s/\[-_\]\??/-/g;
  $str =~ s/\(\?:([^(]+)\|[^(]+?\)/$1/g; # reduce inner groups (?:...|...) to 1st variant
  $str =~ s/\|/, --/g;
  $str =~ tr/()?://d;
  $str;
}

1;
