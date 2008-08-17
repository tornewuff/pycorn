# $Id: Swig.pm,v 1.11 2006/11/18 16:53:25 pfeiffer Exp $

=head1 NAME

CommandParser::Swig - makepp command parser for swig

=head1 DESCRIPTION

Scans a SWIG (Simplified Wrapper and Interface Generator, http://www.swig.org)
command line for dependencies.

=cut

use strict;
package CommandParser::Swig;

use CommandParser;
our @ISA = qw/CommandParser/;

use FileInfo qw(file_info);

#
# Swig has a convenient option to get where its library directories are, so
#

sub new {
  my $self = &CommandParser::new;
  require Scanner::Swig;
  $self->{SCANNER} = new Scanner::Swig($self->rule, $self->dir);
  $self;
}

my( $cplusplus, $idash, $importall, $includeall );

my %opt =
  ('c++' => \$cplusplus,
   'I-' => \$idash,
   importall => \$importall,
   includeall => \$includeall);

# languages
$opt{$_} = 1 for qw(allegrocl chicken csharp guile java modula3 mzscheme ocaml
		    perl perl5 php php4 pike python ruby sexp tcl xml);
# unhandled options with argument
$opt{$_} = 2 for qw(MD MF MMD co dllimport dlname feature features generateconst
		    generaterename generatetypemap globals identifier-converter
		    interface module namespace outdir package prefix typemaplang
		    w withc withcxx withincs withlibs xmllang);



#
# This is the main command parser, implementing
# CommandParser::xparse_command().  See CommandParser.pm for its
# inputs and outputs.
#
my %info_string = (user => 'SWIG_INCLUDES',
		   sys => 'SWIG_SYSTEM_INCLUDES');
my $swig_library_path;
sub xparse_command {
  my( $self, $command, $setenv ) = @_;

  my @input_files;              # The files mentioned on the command line.
  my $output_file;
  my @swig_libs;                # Libraries mentioned with -l.
  my $swig_language = 'tcl';	# The language we're translating into.
  my @includes;
  my @defines;
  my @files;
  $cplusplus = $idash = $importall = $includeall = 0;

  my( $swig_binary, @words ) = @$command; # Make a copy of the command words.

  local $_;
  while( defined( $_ = shift @words )) {
    if( !s/^-// ) {		# Is this a filename?  Swig also takes any
				# unknown -option as a filename, but handling
				# that would require having an up to date list
				# of all options understood by swig, and
				# handling them depending on the language.
				# Hopefully nobody will use this misfeature.
      push @files, $_;
    #
    # It's an option.  Parse it:
    #
    } elsif( $opt{$_} ) {
      if( ref $opt{$_} ) {
	${$opt{$_}} = 1;
      } elsif( $opt{$_} == 1 ) {
	$swig_language = $_;
      } else {
	shift @words;
      }
    #} elsif (/^E$/) {
      # Not sure what to do here.
    } elsif( s/^l// ) {
      push @swig_libs, $_;
    } elsif( /^(?:oh?|xml(?:out)?)$/ ) {
      $self->add_target( shift @words );
    } elsif( s/^D// ) {
      push @defines, $_;
    } elsif( s/^I// ) {
      push @includes, $_;
    }
  }

#
# Set up some things in the scanner:
#
  my $scanner = $self->{SCANNER};
  $scanner->{LANGUAGE} = $swig_language;
  $scanner->should_find("user");
  $scanner->info_string( \%info_string );
  foreach my $def (@defines) {
    if ($def =~ /^(\w+)=(.*)/) {
      $scanner->set_var($1, $2);
    } else {
      $scanner->set_var($def, 1);
    }
  }
  $importall and $scanner->{IMPORTALL} = 1;
  $includeall and $scanner->{INCLUDEALL} = 1;

  my $rule = $self->rule;
  $rule->set_signature_method_scanner("c_compilation_md5");

  $scanner->set_var(SWIG => 1);
  $scanner->set_var("SWIG" . uc($swig_language), 1);
  $scanner->set_var( __STDC__ => 1 );
  $scanner->set_var( __cplusplus => 1) if $cplusplus;

#
# Get the whole include path.  Swig can tell us the system library path
# by running it with the -swiglib option.
#
  $swig_library_path = Makesubs::f_shell "$swig_binary -swiglib", $rule->makefile, $rule->source
    if !defined $swig_library_path;
                                # This is more or less equivalent to caching
                                # `$swig_binary -swiglib`.  See f_shell for
                                # why we don't just use backquotes.
  $scanner->add_include_dir("user", undef) unless $idash;
  foreach my $include (@includes, ".", "$swig_library_path") {
    $scanner->add_include_dir("user", $include);
    $scanner->add_include_dir("sys", $include);
    my $inc_swig_language = "$include/$swig_language";
    if (FileInfo::is_or_will_be_dir( file_info( $inc_swig_language ))) {
                                # If the subdirectory given by the language
                                # exists, it is a valid place to look for
                                # include files too.
      $scanner->add_include_dir("user", $inc_swig_language);
      $scanner->add_include_dir("sys", $inc_swig_language);
    }
  }

#
# Scan any files imported with -l.
#
  my $context = $scanner->get_context;
  foreach my $swig_lib (@swig_libs) {
    $scanner->scan_file($self, "c", "$swig_lib.i") or return undef;
  }

#
# Scan files listed on the command line.
#
  $scanner->reset( $context );
  foreach my $file (@files) {
    $scanner->reset( $context );
    $scanner->scan_file($self, "c", $file) or return undef;
  }

  return 1;
}

1;
