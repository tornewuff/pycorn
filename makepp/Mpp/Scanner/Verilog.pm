# $Id: Verilog.pm,v 1.22 2010/11/17 21:35:52 pfeiffer Exp $

=head1 NAME

Mpp::Scanner::Verilog - makepp scanner for Verilog files

=head1 DESCRIPTION

Scans a verilog file for C<`include>'s and instances.

Tags are:

=over 6

=item vinc

File scanned due to an `include directive or specified on the command line.

=item vlog

File scanned due to a requested instance.

=item vlib

File scanned due to a -v option.

=back

=cut

use strict;
package Mpp::Scanner::Verilog;

use Mpp::Scanner;
use Mpp::File;
our @ISA = 'Mpp::Scanner';

sub new {
  my $self = &Mpp::Scanner::new;

  # MODULE_MAP maps a module to the list of modules it instantiates
  $self->{MODULE_MAP}={};

  # RESOLVED maps modules that have been resolved to 1
  $self->{RESOLVED}={};

  # UNRESOLVED maps unresolved modules that have been instanced to 1
  $self->{UNRESOLVED}={};

  # MODULE is the name of the current module being scanned, if any.
  # Although this seems like it ought to be tied to a each file, in
  # reality the real VCS parser is flat, meaning that it treats the
  # input files as though they had been concatenated into a single input
  # file.
  $self->{MODULE}=undef;

  $self;
}

sub info_string {
  die "info_string doesn't work with Mpp::Scanner::Verilog";
}

# Always scan, because Verilog compilers (typically) also do linking, and
# therefore we need to look for module definitions even if they're stable.
*dont_scan = $Mpp::Text::N[0];

sub resolve_module {
  my( $self, $cp, $module, $finfo, $visited ) = @_;
  $visited ||= {};
  return 1 if $self->{RESOLVED}{$module};
  my $map=$self->{MODULE_MAP}{$module};
  #Mpp::print_log("RESOLVING: $module");
  if($map) {
    my ($nfinfo, @map)=@$map;
    for(@map) {
      if($visited->{$_}) {
	warn "module $_ contains itself\n";
      }
      else {
	my $v={%$visited};
	$v->{$_}=1;
	$self->resolve_module($cp, $_, $nfinfo, $v)
	  or return undef;
      }
    }
  }
  else {
    $self->include($cp, "vlog", $module, $finfo) or return undef;
  }
  $self->{RESOLVED}{$module}=1;
  #Mpp::print_log("RESOLVED: $module");
  return 1;
}

sub resolve {
  my( $self, $cp ) = @_;
  while(my @keys=keys %{$self->{UNRESOLVED}}) {
    my $key=shift(@keys);
    $self->resolve_module($cp, $key, $self->{UNRESOLVED}{$key})
      or return undef;
    delete $self->{UNRESOLVED}{$key};
  }
  1;
}

my @primitives = qw{
  and always assign attribute begin buf bufif0 bufif1 case cmos deassign
  default defparam disable else endattribute end endtable endtask event
  for force forever fork function highz0 highz1 if initial inout input
  integer join large medium nand negedge nor not notif0 notif1 nmos or
  output parameter pmos posedge pulldown pullup pull0 pull1 rcmos reg release
  repeat rnmos rpmos rtran rtranif0 rtranif1 scalared small specify specparam
  strong0 strong1 supply0 supply1 table task tran tranif0 tranif1 time
  tri triand trior trireg tri0 tri1 vertored wait wand weak0 weak1 while
  wire wor xor xnor
};
my %is_prim;
@is_prim{@primitives} = (1..@primitives);

sub scan_comment {
    # The base Verilog scanner ignores comments, but subclasses might need to process them
    return 1;
}

sub xscan_line
{
    my ($self, @scan_Ctx) = @_;
    my (undef, undef, $finfo, undef, $fh) = @scan_Ctx;

    my $state = $finfo->{"Mpp::Scanner::Verilog state"} ||= {};

    local $_ = <$fh>;

    return unless defined;

    if ($state->{in_comment})
    {
        if (s@^(.*?)\*/@@) {
            # end of multi-line block comment
            $self->scan_comment($1, @scan_Ctx) or return [];
            $state->{in_comment} = 0;
        } else {
            # continue within the multi-line block comment
            $self->scan_comment($_, @scan_Ctx) or return [];
            return "";
        }
    }

    # NOTE: If there are overlapping "//" and "/*" comments, then
    # the one that was introduced first wins. For example,
    # "/* // */ foo" produces " foo", and "// /*" does *not* set
    # $state->{in_comment}.

    while (s@//(.*)@@)
    {
        # line-comment found, but it might be within a block-comment
        my $line_comment = $1;

        # first, remove any prior block comments on the same line
        while (s@/\*(.*?)\*/@@) {
            $self->scan_comment($1, @scan_Ctx) or return [];
        }

        if (s@/\*(.*)@@) {

            # the line-comment is within a block comment.
            my $block_comment = $1;
            if ($line_comment =~ s@^(.*?)\*/@@)
            {
                # the block comment is closed on the current line, so the line comment
                # is part of that block, and the line continues after the close.
                $self->scan_comment( $block_comment . "//" . $1, @scan_Ctx ) or return [];
                $_ .= $line_comment;
            }
            else
            {
                # the block comment is multi-line, and the line comment is part of the block
                $self->scan_comment( $block_comment . "//" . $line_comment, @scan_Ctx ) or return [];
                $state->{in_comment} = 1;
            }
        }
        else
        {
            # a pure line comment (no block-comments interfere)
            $self->scan_comment( $line_comment, @scan_Ctx ) or return [];
        }
    }

    # eliminate any remaining single-line block comments
    for my $comment (s@/\*(.*?)\*/@@g) {
        $self->scan_comment($comment, @scan_Ctx) or return [];
    }

    # any remaining "/*" is the start of a multi-line block comment
    if (s@/\*(.*)@@) {
        $self->scan_comment($1, @scan_Ctx) or return [];
        $state->{in_comment} = 1;
    }

    return $_;
}

sub xscan_file {
  my ($self, @scan_Ctx) = @_;
  my ($cp, $tag, $finfo, $conditional, $fh) = @scan_Ctx;
  my $absname = absolute_filename $finfo;
  my $module = $self->{MODULE};
  my $pend_module;
  my $pend_imodule;

  local $_;

  while( defined($_ = $self->xscan_line(@scan_Ctx)) )
  {
    if (ref $_) { # strange error token, because "undef" is EOF
        return undef;
    }

    my $directive;

    if(s/^\s*\`\s*(\w+)\s+//) {
      $directive=$1;
      if($directive eq "include" && /^\"([^"]*)\"/) {
	$self->include($cp, "vinc", $1, $finfo)
	  or return;
      }
      elsif($conditional) {
	if($directive eq "define" &&
	  /^(\w+)\s*(.*?)\s*$/
	) {
	  $self->set_var($1, $2);
	}
	elsif($directive eq "undef" &&
	  /^(\w+)\s*$/
	) {
	  $self->set_var($1, undef);
	}
	elsif($directive=~/^if(n)?def$/ &&
	  /^(\w+)\s*$/
	) {
	  my $def=(defined $self->get_var($1));
	  my $no=($directive eq "ifndef");
	  $self->push_scope($no ? !$def : $def);
	}
	elsif($directive eq "else") {
	  $self->push_scope(!$self->pop_scope);
	}
	elsif($directive eq "endif") {
	  $self->pop_scope();
	}
      }
    }
    elsif($self->is_active) {
      my $pend_module_next;
      my $pend_imodule_next;
      my $imodule;		# TBD: Scope should be smaller
      if(/^\s*(?:module|primitive)\s+(\w+|\\\S+)/
        or ($pend_module and /^\s*(\w+|\\\S+)/)
      ) {
	warn "no closing endmodule for module $module in `$absname:$.'\n"
	  if $module;
	$module=$1;
	$self->{MODULE} = $module if $conditional;
	my $map=$self->{MODULE_MAP};
	if($map->{$module}) {
	  my $n2 = absolute_filename $map->{$module}[0];
	  warn "module $module multiply defined in `$absname' and `$n2'\n";
	}
	$map->{$module}=[$finfo];
      }
      elsif(m!^\s*end(?:module|primitive)\s*$!) {
	undef $module;
	$self->{MODULE} = undef if $conditional;
      }
      elsif(
	($imodule) = /^\s*(\w+|\\\S+)(?:\s+\#\(.*\))?\s+(?:\w+|\\\S+)\s*\(/ or
	($imodule = $pend_imodule and /^\s*\(/)
      ) { # balance ))
	if($is_prim{$imodule}) {
				# Do nothing
	}
	elsif($module) {
				#Mpp::print_log("INSTANCE: $imodule IN $module");
	  push(@{$self->{MODULE_MAP}{$module}},
	    $imodule
	  );
	  $self->{UNRESOLVED}{$imodule}=$finfo
	    unless $tag eq "vlib" ||
	    $self->{RESOLVED}{$imodule};
	}
	else {
	  warn "instance of $imodule not enclosed in a module in `$absname:$.'\n";
	}
      }
      elsif(/^\s*(\w+|\\\S+)(?:\s+\#\(.*\))?\s+(?:\w+|\\\S+)\s*$/) {
	$pend_imodule_next = $1;
      }
      elsif(/^\s*(?:module|primitive)\s*$/) {
	$pend_module_next = 1;
      }
      elsif(/^\s*$/) {
	$pend_module_next = $pend_module;
	$pend_imodule_next = $pend_imodule;
      }
      $pend_module = $pend_module_next;
      $pend_imodule = $pend_imodule_next;
    }
  }
  warn "no closing endmodule for module $module in `$absname'\n"
    if $module && $tag ne "vinc";
  1;
}

=head1 FUTURE WORK

I'm not sure whether a module that is defined by a file `include'ed by
a B<-v> file has its instances resolved immediately (the current scanner
implementation), or whether it's deferred.
In the latter case, we would need to add a new "vlibinc" tag to model this.
On the other hand, this is a pretty weird case that probably never occurs
in practice, and arguably you deserve what you get if you do it.

=cut

1;
