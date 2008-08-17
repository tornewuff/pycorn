#!/usr/bin/perl -w
#
# This script asks the user the necessary questions for installing
# makepp and does some heavy HTML massageing.
#
# $Id: install.pl,v 1.78 2008/07/19 21:32:36 pfeiffer Exp $
#

use Config;
use File::Copy;
use TextSubs ();
use FileInfo ();		# ensure HOME is set

system $^X, 'makepp', '--version'; # make sure it got a chance to apply workarounds.

#
# First make sure this version of perl is recent enough:
#
eval { require 5.006 };
if ($@) {			# Not recent enough?
  die "I need perl version 5.6 or newer.  If you have it installed somewhere
already, run this installation procedure with that perl binary, e.g.,

	perl5.8.6 install.pl

If you don't have a recent version of perl installed (what kind of system are
you on?), get the latest from www.perl.com and install it.
";
}

print 'Using perl in ' . PERL . ".\n";

#
# Load the version number so it can be automatically inserted into the
# files.
#
our $eliminate = '';		# So you can say #@@eliminate
$eliminate = $eliminate if ::is_perl_5_6;
open(VERSION, "VERSION") || die "You are missing the file VERSION.  This should be part of the standard distribution.\n";
our $VERSION = <VERSION>;
our $BASEVERSION = $VERSION;
chomp $VERSION;
close VERSION;

if( $VERSION =~ s/beta// ) {
  $BASEVERSION = $VERSION;
#
# Grep all our sources for the checkin date.  Make a composite version
# consisting of the three most recent dates (shown as mmdd, but sorted
# including year) followed by the count of files checked in that day.  This
# assumes that we have at least three check ins a year.
#
  my %VERSION;
  for( <makepp *.pm */*.pm makepp_builtin_rules.mk> ) {
    open my( $fh ), $_;
    while( <$fh> ) {
      if( /\$Id: .+,v [.0-9]+ ([\/0-9]+)/ ) {
	$VERSION{$1}++;
	last;
      }
    }
  }
  $VERSION .= join '-', '',
    grep s!\d+/(\d+)/(\d+)!$1$2$VERSION{$_}!, (reverse sort keys %VERSION)[0..2];
}

#
# Now figure out where everything goes:
#
$prefix = "/usr/local";

$bindir = shift(@ARGV) ||
  read_with_prompt("
Makepp needs to know where you want to install it and its data files.
makepp is written in perl, but there is no particular reason to install
any part of it in the perl hierarchy; you can treat it as you would a
compiled binary which is completely independent of perl.

Where should the makepp executable be installed [$prefix/bin]? ") ||
  "$prefix/bin";

$bindir =~ m@^(.*)/bin@ and $prefix = $1;
				# See if a prefix was specified.

$datadir = shift @ARGV || read_with_prompt("
Makepp has a number of library files that it needs to install somewhere.  Some
of these are perl modules, but they can't be used by other perl programs, so
there's no point in installing them in the perl modules hierarchy; they are
simply architecture-independent data that needs to be stored somewhere.

Where should the library files be installed [$prefix/share/makepp]? ") ||
  "$prefix/share/makepp";
our $setdatadir;
if ($datadir !~ /^\//) {	# Make a relative path absolute.
  use Cwd;
  my $cwd = cwd;
  chdir $datadir;
  $setdatadir = "\$datadir = '" . cwd . "';";
  chdir $cwd;
} else {
  $setdatadir = "\$datadir = '$datadir';";
}

# prior installation may not have supported .makepp/*.mk files
-r "$datadir/FileInfo_makepp.pm" and
  (stat "$datadir/FileInfo_makepp.pm")[9] < 1102710870 || # check-in time
  do {
    my $found;
    open F, "$datadir/FileInfo_makepp.pm";
    while( <F> ) {
      $found = 1, last if /build_info_subdir.+\.mk/;
    }
    !$found;
  } and
  print '
Warning: the names of the metainformation files under .makepp have changed
with regard to your old installation of makepp.  Every user must issue the
following command at the tops of their build trees, to prevent the new makepp
from rebuilding everything.  Or sysadmins installing this version can issue
the command for the whole machine from the root directory.

  find `find . -name .makepp` -type f | xargs -i mv {} {}.mk

';

$mandir = shift @ARGV || read_with_prompt("
Where should the manual pages be installed?
Enter \"none\" if you do not want the manual pages.
Man directory [$prefix/man]: ") ||
  "$prefix/man";

$htmldir = shift @ARGV || read_with_prompt("
Where should the HTML documentation be installed?
Enter \"none\" if you do not want any documentation installed.
HTML documentation directory [$prefix/share/makepp/html]: ") ||
  "$prefix/share/makepp/html";
$htmldir_val = $htmldir;

use vars qw/$findbin/;
$findbin = shift @ARGV;
defined($findbin) or $findbin = read_with_prompt("
Where should the library files be sought relative to the executable?
Enter \"none\" to seek in $datadir [none]: ") || "none";
$findbin=0 if $findbin eq "none";
if($findbin) {
  $setdatadir = "use FindBin;\n" .
    qq{\$datadir = "\$FindBin::RealBin/$findbin";};
  $htmldir = qq{\$FindBin::RealBin/$findbin/html}
    if $htmldir eq $datadir . "/html";
}

@sig_num{split ' ', $Config{sig_name}} = split ' ', $Config{sig_num};
$USR1 = $sig_num{USR1}; $USR1 = $USR1; 	# suppress used-only-once warning

substitute_file( $_, $bindir, 0755, 1 ) for
  qw(makepp makeppbuiltin makeppclean makeppgraph makeppinfo makepplog makepp_build_cache_control);

substitute_file( $_, $datadir, 0644 ) for
  qw(recursive_makepp FileInfo_makepp.pm BuildCacheControl.pm);

make_dir("$datadir/$_") for
  qw(ActionParser BuildCheck CommandParser Scanner Signature);
foreach $module (qw(AutomakeFixer BuildCache FileInfo Glob MakeEvent
		    Makecmds Makefile Makesubs RecursiveMake Repository
		    Rule TextSubs Utils

		    ActionParser ActionParser/Legacy ActionParser/Specific

		    BuildCheck BuildCheck/architecture_independent
		    BuildCheck/exact_match BuildCheck/ignore_action
		    BuildCheck/symlink BuildCheck/target_newer
		    BuildCheck/only_action

		    CommandParser CommandParser/Esqlc CommandParser/Gcc
		    CommandParser/Swig CommandParser/Vcs

		    Scanner Scanner/C Scanner/Esqlc Scanner/Swig Scanner/Vera
		    Scanner/Verilog

		    Signature Signature/c_compilation_md5 Signature/md5
		    Signature/shared_object Signature/verilog_simulation_md5
		    Signature/verilog_synthesis_md5)) {
  copy("$module.pm", "$datadir/$module.pm");
  chmod 0644, "$datadir/$module.pm";
}

foreach $include (qw(makepp_builtin_rules makepp_default_makefile)) {
  copy("$include.mk", "$datadir/$include.mk");
  chmod 0644, "$datadir/$include.mk";
}

#
# From here on we treat the pod files
#
chdir 'pod';

@pods = <*.pod>;

#
# Install the man pages:
#
if ($mandir ne 'none') {
  make_dir("$mandir/man1");
  foreach $file (@pods) {
    my $manfile = $file;
    $manfile =~ s/\.pod$/.1/;   # Get the name of the man file.
    $manfile =~ s@^pod/@@;
    system("pod2man $file > $mandir/man1/$manfile 2>/dev/null");
                                # Ignore stderr because older versions of
                                # pod2man (e.g., perl 5.006) don't understand
                                # =head3.
    chmod 0644, "$mandir/man1/$manfile";
  }
}

#
# Now massage and install the HTML pages.
#
use Pod::Html ();

sub highlight_keywords() {
  s!\G(\s*)((?:noecho\s+|ignore_error\s+|makeperl\s+|perl\s+|[-\@]|&amp;(?:cat|chmod|cp|mv|cut|echo|expr|printf|yes|grep|sed|(?:un)?install|ln|mkdir|perl|preprocess|rm|sort|template|touch|uniq)\b)+)!$1<b>$2</b>! or

  s!\G((?:override )?(?:define|export|global)|override|ifn?def|makesub|sub)(&nbsp;| +)([-.\w]+)!<b>$1</b>$2<i>$3</i>! or
  s!\G(register_scanner|signature)(&nbsp;| +)([-.\w]+)!<b>$1</b>$2<u>$3</u>! or
  # repeat the above, because they may appear in C<> without argument
  s!\G(\s*(?:and |or |else )?if(?:n?(?:def|eq|sys|true|xxx)|(?:make)?perl)|build_cache|else|endd?[ei]f|export|global|fi|[_-]?include|load[_-]makefile|makeperl|no[_-]implicit[_-]load|override|perl(?:|[_-]begin|[_-]end)|repository|runtime|unexport|define|makesub|sub|register_scanner|signature)\b!<b>$1</b>! && s|xxx|<i>xxx</i>| or

    # highlight assignment
    s,\G\s*(?:([-.\w\s%*?\[\]]+?)(\s*:\s*))?((?:override\s+)?)([-.\w]+)(?= *(?:[:;+?!]|&amp;)?=),
      ($1 ? "<u>$1</u>$2" : '') . ($3 ? "<b>$3</b>" : '') . "<i>$4</i>"
    ,e or

    # highlight rule targets -- EOL trickery to mostly not underline Perl or C++
    $pre && !/define|export|global|override/ && s!\G(\s*)([^&\s].*?)(?=\s*:(?:$|.*?[^;{]\n))!$1<u>$2</u>!m;

  # highlight rule options
  s!(: *)(build_c(?:ache|heck)|quickscan|scanner|signature|smartscan)(&nbsp;| +)([-/\w]+)!$1<b>$2</b>$3<u>$4</u>! or
  # repeat the above, because they may appear in C<> without argument
  s!(: *)(foreach|quickscan|scanner|signature|smartscan)\b!$1<b>$2</b>!;
}

sub highlight_variables() {
  s((\$[\{\(]{1,2})([-\w]+)([\}\)]{0,2})){
    my( $prefix, $name, $suffix ) = ($1, $2, $3);
    $name = "<b>$name</b>" if
      $name =~ /absolute[_-]filename|
	add(?:pre|suf)fix|
	basename|
	call|
	CURDIR|
	dependenc(?:y|ies)|
	dir(?:[_-]noslash)?|
	error|
	filesubst|
	filter(?:[_-]out)?|
	find(?:[_-](?:program|upwards)|file|string)|
	first(?:[_-]available|word)|
	foreach|
	if|
	infer[_-](?:linker|objects)|
	inputs?|
	join|
	make(?:perl)?|
	map|
	mktemp|
	notdir|
	only[_-](?:generated|stale|(?:non)?targets)|
	origin|
	outputs?|
	patsubst|
	perl|
	phony|
	print|
	PWD|
	relative[_-](?:filename|to)|
	shell|
	sort(?:ed_dependencies|ed_inputs)?|
	stem|
	strip|
	subst|
	suffix|
	targets?|
	warning|
	wildcard|
	word(?:list|s)|
	xargs/x;
    $name =~ />(?:outputs?|stem|targets?)</ ?
      "<u>$prefix<i>$name</i>$suffix</u>" :
      "$prefix<i>$name</i>$suffix";
  }eg;
  s!(\$[\@%*])!<u>$1</u>!g;
}

if ($htmldir_val ne 'none') {
  my( %nolink, %link );
  my %alias = (makepp => 'Overview',
	       build_check => 'Build Check Methods',
	       builtin => 'Builtin Rules',
	       builtins => 'Builtin Commands',
	       command => 'Command Options',
	       faq => 'FAQ',
	       tutorial_compilation => 'Compilation Tutorial',
	       makeppbuiltin => 'makeppbuiltin',
	       makeppclean => 'makeppclean',
	       makeppgraph => 'makeppgraph',
	       makeppinfo => 'makeppinfo',
	       makepplog => 'makepplog');
  for( @pods ) {
    my $pod = $_;
    (my $file = $_) =~ s/pod$/html/;
    for( $pod ) {
      s/^makepp_//; s/\.pod$//;
      $alias{$_} ||= join ' ', map "\u$_", split '_';
      $_ = $alias{$_};
      $nolink{$file} = "<b>$_</b>";
      $link{$file} = "<a href='" . ($file eq 'makepp.html' ? '.' : $file) . "'>$_</a>";
    }
  }
  # Put these first/last alphabetically:
  my %order = ('makepp.html' => 0,
	    'makepp_release_notes.html' => 1,
	    'makepp_tutorial.html' => 2,
	    'makepp_tutorial_compilation.html' => 3,
	    'makepp_cookbook.html' => 4,
	    'makepp_faq.html' => 5,
	    'makepp_speedup.html' => 6,
	    'perl_performance.html' => 7,
	    'makeppbuiltin.html' => '~0',
	    'makeppclean.html' => '~1',
	    'makeppgraph.html' => '~2',
	    'makeppinfo.html' => '~3',
	    'makepplog.html' => '~4');
  my $home = ($htmldir_val =~ /\/[0-9.]+(?:\/|$)/);
  my @links =
      sort { (exists $order{$a} ? $order{$a} : $nolink{$a}) cmp (exists $order{$b} ? $order{$b} : $nolink{$b}) }
      keys %link;
  # Separate menu into 3 sections
  $nolink{$_} = "<hr />$nolink{$_}", $link{$_} = "<hr />$link{$_}" for
      qw(makepp_build_algorithm.html makeppbuiltin.html);
  $link{'..'} = "<a href='..'>Home</a>", unshift @links, '..' if $home;

  # Nuke the pod cache, because otherwise it doesn't get updated when new
  # pages are added (on Perl 5.6.1, Pod::Html 1.03, i686-linux2.4)
  unlink <pod2htm*~~ pod2htm?.tmp>;
  my $tmp = '/tmp/makepp' . substr rand, 1;
  make_dir($htmldir_val);
  $htmldir_val =~ s!^([^/])!../$1!;
  my $empty_line = '';
  for( @pods ) {
    my $has_commands_with_args = /makepp(?:_(?:builtins|command|extending|functions|statements)|builtin|graph|log)\.pod/;
    my( $timestamp, $author ) = ('', '');
    {
      open my( $podfile ), $_;
      for( 1..10 ) {		# Expect it in first 10 lines.
	if( <$podfile> =~ m!\$Id: .+,v [0-9.]+ (\d{4})/(\d{2})/(\d{2}) ! ) {
	  $timestamp = "$1-$2-$3";
	}
      }
    }
    Pod::Html::pod2html qw'--libpods=makepp_functions:makepp_rules:makepp_statements:makepp_variables:makepp_signatures:makepp_build_cache
			   --podpath=. --podroot=. --htmlroot=. --css=makepp.css --infile', $_, '--outfile', $tmp;
    open my( $tmpfile ), $tmp;
    s/pod$/html/;
    my $img = '<img src="makepp.gif" width="142" height="160" title="makepp" border="0">';
    $img = "<a href='" . ($home ? '/' : '.') . "'>$img</a>" if $home || length() > 11;
    my $nav = "<table id='Nav'><tr><th>$img<th></tr><tr><th></th></tr>
  <tr><th>
    <a href='http://sourceforge.net/projects/makepp/'>at
    <img src='sflogo.png' width='88' height='31'
	 border='0' align='middle' alt='SourceForge' /></a></th></tr>
  <tr><th><form method='GET' action='http://google.com/search'>
    <input type='hidden' name='as_sitesearch' value='makepp.sourceforge.net' />
    <input type='text' name='as_q' size='14' /><br />
    <button type='submit'><img src='google.png' width='16' height='16' align='middle' alt='Google'> Site Search</button>
  </form></th></tr><tr><td>";
    { # No link to self.
      local $link{$_} = $nolink{$_};
      $nav .= join '<br />', map $link{$_}, @links;
    }
    $nav .= '</td></tr></table>';
    open my $outfile, ">$htmldir_val/$_" or die "can't create `$htmldir_val/$_'--$!";
    chmod 0644, "$htmldir_val/$_";
    s/\.html$//;
    my $title = (s/^makepp(?:_|$)// ? 'Makepp ' : '') . $alias{$_ || 'makepp'};
    my $index;
    my %count;
    while( <$tmpfile> ) {
      s/<(\/?[\w\s]+)/<\L$1/g if $Pod::Html::VERSION < 1.04; # Perl 5.6
      if( defined $unread ) {
	$_ = $unread . $_;
	undef $unread;
      }
      if ( /^<\/head>/../<h1>.*(?:DESCRIPTION|SYNOPSIS)/ ) {
	if ( /<(?:\/?ul|li)>/ ) {
	  # These are visible anyway when the index is.
	  next if /NAME|DESCRIPTION|SYNOPSIS|AUTHOR/;
	  $index .= $_;
	}
	# Such a line MUST be present in every POD.
	next unless s!<p>(makepp\w*) -- (.+)!$2!;
	# Paragraph maybe not complete, grab rest
	$_ .= <$tmpfile> while ! s!</p>$!</b>!i;
	for ( $index ) {
	  # Rearrange the index, because we threw out some items, discard it if empty.
	  s!<ul>\s+</ul>!!;
	  m!<ul>\s+<ul>! &&
	  s!</ul>\s+</ul>!</ul>! &&
	  s!<ul>\s+<ul>!<ul>!;
	}
	$_ = "<link rel='icon' href='url.png' type='image/png' />
<meta name='keywords' content='makepp, make++, Make, build tool, repository, cache, Perl, Make alternative, Make replacement, Make enhancement, Make improvement, Make substitute, dmake, gmake, GNU Make, nmake, pmake, easymake, imake, jmake, maketool, mmake, omake, ppmake, PVM gmake, shake, SMake, ant, maven, cook, jam, Cons, SCons, cc -M, gcc -MM, g++ -MM, makedepend, makedep, mkdep, CCache, Compilercache, cachecc1, Make::Cache, automake' />
</head><body>$nav<h1>$title</h1>\n<p><b>$_</p>$index";
      } elsif ( s/<pre>\n// ) {
	$pre = 1;
      } elsif ( $pre ) {

	if( /^(.*#.*\|.*\|.*#.*\|.*\|.*)<\/pre>$/ ) { # Special case for compatibility table.

	  my @list = split /[#|]/, $1;
	  s/^\s+//, s/\s+$// for @list;
	  if( $list[0] ) {
	    $_ = '<tr><th align="left">' . shift( @list ) . '</th>';
	    for my $elem ( @list ) {
	      if( $elem eq 'x' ) {
		$_ .= '<th class="good">x</th>';
	      } elsif( $elem eq '/' ) {
		$_ .= '<th class="bad"><i>/</i></th>';
	      } elsif( $elem ) {
		$_ .= "<th class='soso'>x<a href='#\L$elem'><sup>*)</sup></a></th>";
	      } else {
		$_ .= '<th>&nbsp;</th>';
	      }
	    }
	  } else {		# Heading line.
	    shift @list;
	    $_ = '<tr><th></th>';
	    if( $list[0] ne '.0' ) {
	      $_ .= '<th colspan="3">5.6</th><th colspan="' . (@list - 4) . '">5.8</th><th>5.10</th>';
	    } else {
	      for my $elem ( @list ) {
		$_ .= "<th>&nbsp;$elem&nbsp;</th>";
	      }
	    }
	  }
	  $_ .= "</tr>\n";
	  $pre = 0;

	} else {

	  # unindent initial whitespace which marks <pre> in pod
	  s/^ {1,7}\t(\t*)(?!#)/$1    / or s/^    ?//;

	  if( /^\s+$/ ) {
	    $empty_line = '<span class="tall">&nbsp;</span>';
	    next;
	  } else {
	    # don't parse comments
	    $end = s/(#(?: .*?)?)((?:<\/pre>)?)$// ? "<span class='comment'>$1</span>$empty_line$2" : $empty_line;
	    $empty_line = '';
	  }

	  s!^([%\$]? ?)(makepp(?:builtin|clean|log|graph|_build_cache_control)?)\b!$1<b>$2</b>!g or
	  s!^([%\$]? ?)(mpp(?:[bclg]c{0,2})?)\b!$1<b>$2</b>!g or
				# g creates BOL \G for keywords
	    highlight_keywords;
	  highlight_variables;

	  # put comment back in
	  s/$/$end/ if $end;
	  $_ = '<pre>' . $_, $pre++ if $pre == 1;
	  $pre = 0 if m!</pre>!;

	}
      } elsif ( /^<\/dd>$/ ) { # repair broken nested =over
	$dd = 1;
	next;
      } elsif ( $dd and /^<(?:d[dt]|\/dl)>/ ) {
	$dd = 0;
	s!(<strong>-. )(.+?<)!$1<i>$2/i><! ||	# Repetitions of same don't get itemized.
	s!("item_[^"]*">--[^<=]*=)(.+?) ?<!$1<i>$2</i><! ||
	s!("item_[^"]*">[^ <,]* )(.+?) ?<!$1<i>$2</i><!
	  if $has_commands_with_args;		# italicize args
	s!"item_(\w+)[^"]*">(\1)!"$1">$2!i;	# fix =item anchor
	s!"item_(_2d\w+)">!"$1">! ||		# fix =item hexcode-anchor
	s! name="item_(_\w+?)(?:__[25][db].*?)?">!$seen{$1}++ ? '>' : " name='$1'>"!e;
	s!"item_(%[%\w]+)">!(my $i = $1) =~ tr/%/_/; "'$i'>"!e; # Perl 5.6
      } elsif( ! s/<title>\w+/<title>$title/ ) {
	s!([\s>]|^)([Mm]ake)pp([\s,.:])!$1<i><span class="makepp">$2</span>pp</i>$3!g;
	s!("#_)(?=\w+">&amp;)!${1}26!g;		# fix builtins index link
	s!<li></li>!<li>!;
	s!(<strong>-. )(.+?<)!$1<i>$2/i><! ||	# Repetitions of same don't get itemized.
	s!("item_[^"]*">--[^<=]*=)(.+?) ?<!$1<i>$2</i><! ||
	s!("item_[^"]*">[^ <,]* )(.+?) ?<!$1<i>$2</i><!
	  if $has_commands_with_args;		# italicize args
	s!"item_(\w+)[^"]*">(\1)!"$1">$2!i;	# fix =item anchor
	s!"item_(_2d\w+)">!"$1">! ||		# fix =item hexcode-anchor
	s! name="item_(_\w+?)(?:__[25][db].*?)?">!$seen{$1}++ ? '>' : " name='$1'>"!e;
	s!#item_(\w+)">!#$1">!g;		# fix =item link
	s!"item_(%[%\w]+)">!(my $i = $1) =~ tr/%/_/; "'$i'>"!e; # Perl 5.6
	s!#item_(%[%\w]+)">!(my $i = $1) =~ tr/%/_/; "#$i\">"!ge; # Perl 5.6
	s!\./(\.html.+? in the (.+?) manpage<)!$2$1!g;		  # at least up to 5.8.5
	highlight_keywords while /<code>/g;	# g creates "pseudo-BOL" \G for keywords
	highlight_variables;
	if ( /<h1.+AUTHOR/ ) {	# Put signature at bottom right.
	  $_ = <$tmpfile>;
	  /p>(.+?) (?:\(|&lt;)(<a href="mailto:.+?">)/i;
	  $author = "$2$1</a>";
	  $_ = '';
	} elsif( $timestamp || $author and /<\/body>/ ) {
	  if( $timestamp ) {
	    if( $author ) {
	      $author .= '<br />';
	    } else {
	      $author = '<hr />'; # In this case there was none.
	    }
	    $timestamp = "Last modified: $timestamp";
	  }
	  $_ = "<address>$author$timestamp</address>$_";
	}
      }
				# uniquify =item labels (and simplify them)
      no warnings 'uninitialized';
      s!dt><strong><a name=['"](.+?)['"]!dt id="$1$count{$1}"! &&
	++$count{$1} &&
	s!</a></strong>!!;
      s!\./\./makepp!makepp!g;
      print $outfile $_;
    }
  }

  unlink $tmp;

  for( qw'google.png makepp.gif makepp.css pre.png sflogo.png url.png' ) {
    copy $_, "$htmldir_val/$_";
    chmod 0644, "$htmldir_val/$_";
  }

  symlink 'makepp.html', "$htmldir_val/index.html";
}

#
# Figure out whether we need to do anything about Digest::MD5.
#
eval "use Digest::MD5";		# See if it's already installed.
if ($@) {
  print "\nIt looks like you don't have the perl module Digest::MD5 installed.
Makepp likes to have Digest::MD5 installed because it is a better technique
for checking whether files have changed than just looking at the dates.
Makepp will work without it, however.
   If you would like this feature, you'll have to install this perl module.
If you installed perl from a binary distribution (e.g., from a linux package),
you can probably get a precompiled version of this module from the same place.
Otherwise, you can get it from CPAN
(http://www.cpan.org/authors/id/G/GA/GAAS/Digest-MD5-2.33.tar.gz).
After you've downloaded it, do the following:
	gzip -dc Digest-MD5-2.33.tar.gz | tar xf -
	cd Digest-MD5-2.33
	perl Makefile.PL
	make
	make test
	make install
There is no need to reinstall makepp after installing Digest::MD5.

";
}

print "makepp successfully installed.\n";

#
# This subroutine makes a copy of an input file, substituting all occurences
# of @xyz@ with the perl variable $xyz.  It also fixes up the header line
# "#!/usr/bin/perl" if it sees one.
#
# Arguments:
# a) The input file.
# b) The output directory.
# c) The protection to give the file when it's installed.
#
sub substitute_file {
  my ($infile, $outdir, $prot, $abbrev) = @_;

  local *INFILE;
  open(INFILE, $infile) || die "$0: can't read file $infile--$!\n";
  make_dir($outdir);

  open(OUTFILE, "> $outdir/$infile") || die "$0: can't write to $outdir/$infile--$!\n";

  local $_;
  while( <INFILE> ) {
    my $perl = PERL;
    s@^\#!\s*(\S+?)/perl(\s|$)@\#!$perl$2@o	# Handle #!/usr/bin/perl.
       if $. == 1;
    s/\\?\@(\w+)\@/${$1}/g;		# Substitute anything containg @xyz@.
    if( /^#\@\@(\w+)/ ) {		# Substitute anything containg #@@xyz ... #@@
      1 until substr( <INFILE>, 0, 3 ) eq "#\@\@";
      $_ = $$1;
    }

    print OUTFILE $_;
  }
  close(OUTFILE);
  close(INFILE);

  chmod $prot, "$outdir/$infile";
  for( $abbrev ) {
    last unless defined;
    $_ = $infile;
    {
      no warnings 'uninitialized';
      s/makepp(?:_?(.)[^_]+(?:_(.)[^_]+(?:_(.)[^_]+)?)?)?/mpp$1$2$3/;
    }
    link "$outdir/$infile", "$outdir/$_"
      or symlink "$outdir/$infile", "$outdir/$_";
    unless( -f "$outdir/$_" ) {
      copy "$outdir/$infile", "$outdir/$_";
      chmod $prot, "$outdir/$_";
    }
    if( is_windows > 0 ) {
      open my $outfile, "> $outdir/$infile.bat" or die "$0: can't write to $outdir/$infile.bat--$!\n";
      print $outfile '@' . PERL . " $outdir/$infile %1 %2 %3 %4 %5 %6 %7 %8 %9\n";
      close $outfile;
      copy "$outdir/$infile.bat", "$outdir/$_.bat";
    }
  }
}

#
# Make sure a given directory exists.  Makes it and its parents if necessary.
# Arguments: the name of the directory.
#
sub make_dir {
  my $dirname = '';
  foreach (split(/\//, $_[0])) {
    if ($_ ne '') {		# Skip the top level / directory.
      $dirname .= $_;		# Make the new directory name.
      -d $dirname or
	mkdir($dirname, 0755);
    }
    $dirname .= '/';
  }
}

sub read_with_prompt {
  local $| = 1;			# Enable autoflush on STDOUT.

  print @_;			# Print the prompt.
  $_ = <STDIN>;			# Read a line.
  chomp $_;
#
# Expand environment variables and home directories.
#
  s/\$(\w+)/$ENV{$1}/g;		# Expand environment variables.
  if (s/^~(\w*)//) {		# Is there a ~ expansion to do?
    if ($1 eq '') {
      $_ = "$ENV{HOME}$_";
    } else {
      my ($name, $passwd, $uid, $gid, $quota, $comment, $gcos, $dir, $shell) = getpwnam($1);
				# Expand from the passwd file.
      if ($dir) {		# Found it?
	$_ = "$dir$_";
      } else {
	$_ = "~$1";		# Not found.  Just put the ~ back.
      }
    }
  }
  return $_;
}
