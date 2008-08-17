# $Id: Gcc.pm,v 1.28 2008/07/09 21:36:01 pfeiffer Exp $

=head1 NAME

CommandParser::Gcc - makepp command parser for Gcc

=head1 DESCRIPTION

Scans a gcc compile command for implicit dependencies.

This class is readily subclassed for similar things, such as the
Embedded SQL preprocessor/compiler.

=cut

use strict;
package CommandParser::Gcc;

use CommandParser;
our @ISA = 'CommandParser';

use TextSubs;
use FileInfo_makepp;

sub new {
  my $self = &CommandParser::new;
  require Scanner::C;
  $self->{SCANNER} = new Scanner::C($self->rule, $self->dir);
  $self;
}

sub new_no_gcc {
  my $self = &new;
  undef $self->{NO_GCC};
  $self;
}

sub set_default_signature_method {
  my( $self, $leave_comments ) = @_;

  # Use the MD5 signature checking when we can.
  $main::has_md5_signatures and
    $self->rule->set_signature_method_scanner( $leave_comments ? 'md5' : 'c_compilation_md5' );
}

# The subclass can override these. Don't start doing any actual scanning here,
# because the signature method isn't necessarily set yet.
*parse_opt = \&TextSubs::CONST0; # Ignore unknown option.
sub parse_arg {
  #my( undef, $arg, undef, $files ) = @_;

  push @{$_[3]}, $_[1]
    if TextSubs::is_cpp_source_name $_[1];
}


our( $stop_at_obj, $leave_comments, $nostdinc);

my %opt =
  (c => \$stop_at_obj,
   E => \$stop_at_obj,
   S => \$stop_at_obj,
   C => \$leave_comments,
   nostdinc => \$nostdinc);

my %info_string = (user => 'INCLUDES',
		   sys => 'SYSTEM_INCLUDES',
		   lib => 'LIBS');
my @suffix_list = qw(.la .so .sa .a .sl);
sub xparse_command {
  my( $self, $command, $setenv ) = @_;

  my $dir=$self->dir;
  my $scanner=$self->{SCANNER};

  $scanner->should_find( 'user' );
  $scanner->info_string( \%info_string );
  $scanner->add_include_suffix_list( lib => \@suffix_list );

  my @prefiles;
  my @files;
  my $idash; # already saw -I-
  my @incdirs;
  my @inc1dirs; # specified *before* -I-
  my @idirafter;
  my $iprefix = '';
  my @libs;
  my @obj_files;
  my @cpp_cmd;
  my $file_regexp = $self->input_filename_regexp($command->[0]);

  $stop_at_obj = $leave_comments = $nostdinc =  0;
  my( $cmd, @words ) = @$command;
  $cmd =~ s@.*/@@ || ::is_windows > 1 && $cmd =~ s/.*\\//;
  push (@cpp_cmd, $cmd);
  local $_;
  while( defined( $_ = shift @words )) {
    if( !s/^-// ) {
      if( /\.(?:[ls]?[oa]|s(?:l|o\.[\d.]+)|obj|dll)$/ ) { # object file?
	if( /[\*\?\[]/ ) {		# wildcard?
 # TBD: Why is this disabled?  Probably because zglob finds more than the Shell will.  Need chdir & glob.
 if(0) {
	  require Glob;
	  push @obj_files,
	    Glob::zglob($_, $self->dirinfo);
 }
	} else {
	  push @obj_files, $_;
	}
      } elsif($file_regexp && /$file_regexp/) {
        push @files, $_;
      } else {
	$self->parse_arg( $_, \@words, \@files );
      }
    } elsif( $opt{$_} ) {
      ${$opt{$_}} = 1;
    } elsif( s/^I// ) {
      $_ ||= shift @words;
      if( $_ eq '-' ) {
	$idash = 1;
      } else {
	push @{$idash ? \@incdirs : \@inc1dirs}, $_;
      }
    } elsif( s/^i(?:quot(e)|syste(m)|dirafte(r)|(nclude|macros)|prefi(x)|withprefix(before)?)// ) {
      my $val = $_ || shift @words; # yes, value can be glued :-{
      if( $1 || $2 ) {		# -iquote, -isystem
	$scanner->add_include_dir( user => $val );
	$scanner->add_include_dir( sys => $val ) if $2;
      } elsif( $3 ) { 		# -idirafter
	push @idirafter, $val;
      } elsif( $4 ) {		# -include or -imacros
	push @prefiles, $val;
      } elsif( $5 ) { 		# -iprefix
	$iprefix = $val;
      } elsif( $6 ) { 		# -iwithprefixbefore
	push @{$idash ? \@incdirs : \@inc1dirs}, $iprefix . $val;
      } else {			# -iwithprefix
	push @idirafter, $iprefix . $val;
      }
    } elsif( s/^L// ) {
      $_ ||= shift @words;
      $scanner->add_include_dir( lib => $_ );
    } elsif( s/^l// ) {
      $_ ||= shift @words;
      push @libs, $_;
    } elsif( s/^D// ) {
      $_ ||= shift @words;
      if( /^(\w+)=(.*)/ ) {
	$scanner->set_var( $1, $2 );
      } else {
	$scanner->set_var( $_, 1 );
      }
    } elsif( s/^U// ) {
      $_ ||= shift @words;
      $scanner->set_var( $_, undef );
    } elsif( s/^o// ) {
      $self->add_target( $_ || shift @words );
    } else {
      push @cpp_cmd, "-$_";	# collect non parsed options for preprocessor
      $self->parse_opt( $_, \@words, \@files );
    }
  }

  $self->set_default_signature_method( $leave_comments );

  unless( $idash ) {
    unshift @incdirs, splice @inc1dirs;
    $scanner->add_include_dir( user => undef );
  }
  push @incdirs, split ':', $setenv->{CPATH}
    if $setenv->{CPATH} && $command->[0] =~ /^g/i;
  $scanner->add_include_dir( user => $_ )
    for @inc1dirs, @incdirs;
  $scanner->add_include_dir( sys => $_ )
    for @incdirs;
  if( $nostdinc ) {
    $scanner->should_find( 'sys' );
  } else {
    require Makesubs;
    for my $dir ( @Makesubs::system_include_dirs ) {
      $scanner->add_include_dir( $_, $dir )
	for qw(user sys);
    }
  }
  for my $dir ( @idirafter ) {
    $scanner->add_include_dir( $_, FileInfo::absolute_filename FileInfo::file_info $dir, $self->{RULE}{MAKEFILE}{CWD} )
      for qw(user sys);
  }
  my $context = $scanner->get_context;
  for( @files ) {
    $scanner->reset( $context );
    $self->xset_preproc_vars(\@cpp_cmd, $_) if $scanner->{CONDITIONAL};
    # scan each file included on command line with  set of preproc. vars
    for (@prefiles) {
      $scanner->scan_file( $self, c => $_ ) or return undef;
    }
    $scanner->scan_file( $self, c => $_ ) or return undef;
  }

  unless( $stop_at_obj ) {
    $scanner->add_include_dir( lib => $_ )
      for @Makesubs::system_lib_dirs;
    $scanner->add_dependency( $self, lib => "lib$_" )
      for @libs;
    $self->add_simple_dependency( $_ )
      for @obj_files;
  }
  return 1;
}

sub _set_def {
  my ($self, $key, $value) = @_;
  $self->{SCANNER}->set_var( $key => $value )
    unless exists $self->{SCANNER}{VARS}{$key};
}
my %var_cache;
sub xset_preproc_vars {
  my( $self, $command ) = @_;
  my $file_end = substr $_[2], -4;
  FileInfo::case_sensitive_filenames or $file_end =~ tr/A-Z/a-z/;

#figure out file type
  my $cplusplus = $file_end =~ /\.(?:c(?:c|xx|pp|\+\+)|C)$/;
  my $used_cpp;

  my $traditional = grep { $_ eq '-traditional' } @$command;
  my $no_gcc = exists $self->{NO_GCC} || grep { $_ eq '-no-gcc' } @$command;
#very basic setup any compiler/OS
  my $cmd;
  unless( exists $self->{NO_GCC} ) {
#use preprocessor
    my @command = @$command;
    splice(@command, 1, 0, '-E', '-dM', '-x', $cplusplus ? 'c++' : 'c', '/dev/null');
    $cmd=TextSubs::join_with_protection(@command);
    if($var_cache{$cmd}) {
      my %copy = %{$var_cache{$cmd}};
      $self->{SCANNER}{VARS} = \%copy;
      return;
    }
#now call preprocessor to get rest of variables, but do not
#override variables which were already set up via -D and -U
    local $_;
    open(my $cpp, "$cmd|") or die;
    while(<$cpp>) {
      chomp;
      next if(/^\#\s+\d/);	# gcc 3.4 reports an explicit line number
      # The second space is optional, because gcc doesn't always print it
      # for certain built-in macros such as __FILE__, and when that happens
      # the value is the empty string.
      if(/^\#define (\S+) ?(.*)$/) {
	my ($name, $val)=($1, $2);
	_set_def $self, $name, $val;
      } else {
	warn "$cmd produced unparsable `$_'";
      }
    }
    close($cpp);
    if($?) {
      warn "Preprocessor exited with status $? from command: $cmd\n";
    }
    else {
      $used_cpp=1;
    }
  }
  if(!$used_cpp) {
    my $vars = $self->{SCANNER}{VARS};
    _set_def $self, __STDC__ => 1 unless $traditional;
    _set_def $self, __GNUC__ => 1 unless $no_gcc;
    _set_def $self, __cplusplus => 1 if $cplusplus;
  }
  unless( exists $self->{NO_GCC} ) {
    my %copy = %{$self->{SCANNER}{VARS}};
    $var_cache{$cmd} = \%copy;
  }
  return;
}
1;
