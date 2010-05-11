# $Id: Vcs.pm,v 1.27 2009/02/11 23:22:37 pfeiffer Exp $

=head1 NAME

Mpp::CommandParser::Vcs - makepp command parser for Synopsys VCS

=head1 DESCRIPTION

Scans a vcs command for implicit dependencies.

=cut

use strict;
package Mpp::CommandParser::Vcs;

use Mpp::CommandParser;
our @ISA = qw/Mpp::CommandParser/;

use Mpp::File;
use Mpp::Text;

sub new {
  my $self = &Mpp::CommandParser::new;
  require Mpp::Scanner::Verilog;
  require Mpp::Scanner::Vera;
  $self->{SCANNER}=new Mpp::Scanner::Verilog($self->rule, $self->dir);
  $self->{VERA_SCANNER}=new Mpp::Scanner::Vera($self->rule, $self->dir);
  $self;
}

# free function
sub updir {
  my ($name) = @_;
  return $name if $name =~ m@^/@;
  return "../$name";
}

# This can be modified from the outside for site-specific suffixes:
our @verilog_suffixes = qw(v gv vlib vt bvrl vg vgs vgp sv);

my %info_string = (user => 'VERA_INCLUDES',
		   sys => 'VERA_SYSTEM_INCLUDES');
sub xparse_command {
  my( $self, $command, $setenv ) = @_;

  # Use the MD5 signature checking when we can.
  $self->rule->set_signature_method_scanner( 'verilog_simulation_md5' )
    if $Mpp::has_md5_signatures;

  my @words=@$command;
  my $dir=$self->dir;
  my $file_regexp = $self->input_filename_regexp(
    $command->[0], [map { ".$_" } @verilog_suffixes]
  );

  my $scanner=$self->{SCANNER};
  $scanner->should_find("vinc");
  $scanner->add_include_dir("vinc", ".");
  $scanner->should_find("vlog");
  # NOTE: We can't set an info_string for the "vlog" tag, because
  # whether the files are actually needed depends on whether the
  # module containing the instance was itself instantiated. And
  # because Mpp::Scanner::Verilog goes out of its way to model that,
  # instances can get imputed to the wrong module! And since we
  # can't set it for "vlog", we can't set it for any tag, or else
  # we violate the requirements of the Mpp::Scanner base class.

  my $vera_scanner=$self->{VERA_SCANNER};
  $vera_scanner->should_find("user");
  $vera_scanner->info_string( \%info_string );
  $vera_scanner->add_include_dir("user", undef);

  my %visited;
  my @libs; # -v files
  my @files;
  my @vera_files;
  my @c_args;
  my $cmd = shift @words;	# Remove command name
  WORD: while(defined($_ = shift @words)) {
    if(ref $_) {
      # This was inserted by a -f option to indicate that
      # we're no longer reading args that came from the
      # particular file. Neat hack, eh?
      delete $visited{$_};
    }
    elsif(/^\+incdir\+(.*)/) {
      my @args=map { $_ ? ($_) : () } (split(/\+/, $1));
      for(@args) {
	$scanner->add_include_dir("vinc", $_);
      }
    }
    elsif(/^\+define\+(.*)/) {
      my @defs=map { $_ ? ($_) : () } (split(/\+/, $1));
      for(@defs) {
	if(/^(\w+)=(.*)/) {
	  $scanner->set_var($1, $2);
	}
	else {
	  $scanner->set_var($_, "");
	}
      }
    }
    elsif(/^-ntb_incdir$/) {
      my @args=map { $_ ? ($_) : () }
	(split(/\+/, shift @words));
      for(@args) {
	$vera_scanner->add_include_dir("user", $_);
	$vera_scanner->add_include_dir("sys", $_);
      }
    }
    elsif(/^-ntb_define$/) {
      my @defs=map { $_ ? ($_) : () }
	(split(/\+/, shift @words));
      for(@defs) {
	if(/^(\w+)=(.*)/) {
	  $vera_scanner->set_var($1, $2);
	}
	else {
	  $vera_scanner->set_var($_, "");
	}
      }
    }
    elsif(/^\+libext\+(.*)/) {
      my @sfxs = split /\+/, $1;
      push @sfxs, '' if /\+$/;
      $scanner->add_include_suffix_list( vlog => \@sfxs );
    }
    # TBD: What about -F and -file? How are they different from -f?
    elsif(/^-f$/) {
      my $file=shift @words;
      my $finfo=$self->add_dependency($file);
      next WORD unless $finfo;
      my $absname = absolute_filename $finfo;
      if($visited{$finfo}++) {
	warn "vcs argument file `$absname' already visited\n";
      } else {			# Remember that we're in a -f
	unshift(@words, $finfo);
	if(open(IN, "<", $absname)) {
	  my @read_args;
	  local $_;
	  while(<IN>) {
	    s!//.*!!;
	    push(@read_args, split);
	  }
	  unshift(@words, @read_args);
	} else {
	  warn "couldn't read vcs arguments from $absname--$!\n";
	}
      }
    }
    elsif(/^-w\d+/) {
      # For verilint, ignore exclude pattern specifications
      shift @words;
      if($words[$[] =~ m!^/!) {
	do { $_ = shift @words } until m!/$!;
      }
    }
    elsif(/^-P$/) {
      my $file=shift @words;
      $self->add_simple_dependency($file);
    }
    elsif(/^-y$/) {
      my $vdir=shift @words;
      $scanner->add_include_dir("vlog", $vdir);
    }
    elsif(/^-v$/) {
      my $file=shift @words;
      push(@libs, $file);
    }
    elsif($file_regexp && /$file_regexp/) {
      push(@files, $_);
    }
    # TBD: Do OVA files need to be scanned (also -ova_file)?
    elsif(/\.(:?vro|ova)$/) {
      $self->add_simple_dependency($_);
    }
    elsif(/\.vr[il]?$/) {
      push(@vera_files, $_);
    }
    elsif(!/^[-+]/) {
      push(@c_args, updir($_))
	if is_cpp_source_name($_) ||
	  is_object_or_library_name($_);
    }
    # NOTE: For here on, only options can be matched.
    elsif(/^-(C|LD)FLAGS$/) {
      local $_ = shift @words;
      push(@c_args, split);
    }
    elsif(/^-l./) {
      push(@c_args, $_);
    }
    # TBD: What does -cm_name do?
    elsif(/^-cm_(?:assert_hier|constfile|fsmcfg|hier|opfile|resetfilter|sigfile|stoppropagation|tglfile)$/ || /^-ova_(?:cov_hier|file)$/) {
      my $file = shift @words;
      $self->add_simple_dependency($file);
    }
    elsif(/^(?:\+ad=|\+applylearn\+|\+optconfigfile\+|-Minclude=)(.*)/) {
      $self->add_simple_dependency($1);
    }
    elsif(/^-(?:vcd2vpd|vpd2vcd)$/) {
      my $file = shift @words;
      $self->add_simple_dependency($file);
      $file = shift @words;
      $self->add_target($file);
    }
    elsif(/^-l$/) {
      my $file = shift @words;
      $self->add_target($file);
    }
    elsif(/^\+vslogfile(?:sim)?\+(.*)/) {
      $self->add_target($1);
    }
    elsif(/^-C$/i) {
      push(@c_args, "-c");
    }
    elsif(/^-o$/) {
      push(@c_args, $_, updir(shift @words));
    }
  }

  require Mpp::Subs;
  foreach (@Mpp::Subs::system_include_dirs) {
    for my $tag ("user", "sys") {
      $vera_scanner->add_include_dir( $tag, $_ );
    }
  }

  for my $file (@libs) {
    $scanner->scan_file($self, "vlib", $file) or return undef;
  }
  for my $file (@files) {
    $scanner->scan_file($self, "vinc", $file) or return undef;
  }
  my $files_left = 1;
  while($files_left) {
    $files_left = $scanner->resolve($self) &&
      $scanner->continue_scanning($self);
  }
  return undef unless defined($files_left);
  for(@vera_files) {
    $vera_scanner->scan_file($self, "vera", $_) or return undef;
  }
  if(@c_args) {
    # Because some relative paths in @c_args are interpretted
    # by VCS as being relative to the csrc subdirectory, we model
    # that by having the scanner think that it's in that dir,
    # but munge the @c_args that *aren't* relative to csrc before
    # we get here.
    Mpp::File::mark_as_directory file_info "csrc", $self->dirinfo;
    require Mpp::CommandParser::Gcc;
    my $c_parser=new_no_gcc Mpp::CommandParser::Gcc(
      $self->rule, $self->dir."/csrc"
    );
    $c_parser->xparse_command([$cmd, @c_args], $setenv)
      or return undef;
  }
  return 1;
}

1;
