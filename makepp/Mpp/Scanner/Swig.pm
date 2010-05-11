# $Id: Swig.pm,v 1.5 2009/02/09 22:45:23 pfeiffer Exp $

=head1 NAME

Mpp::Scanner::Swig - makepp scanner for SWIG's .i files

=head1 DESCRIPTION

Scans a .i file for additional dependencies.  The only thing of interest
is the %include directive and #include directives if -includeall is specified
on the swigcommand line.

=cut

use strict;
use Mpp::Scanner::C;
package Mpp::Scanner::Swig;

use Mpp::Scanner;
our @ISA = qw/Mpp::Scanner::C/;

#
# Override Mpp::Scanner::C::get_directive.  We need to do the following:
# 1) Ignore everything between %{ and %}.  We
# 2) Ignore #include unless -includeall is present.
# 3) Handle swig's additional directives.
#
# However, we want to use the rest of xscan_file's mechanism to
# handle preprocessing directives.
#
sub get_directive {
  my $self = $_[0];
  if ($self->{IN_VERBATIM_BLOCK}) {
    if (s/^\s*\%\}\s*//) {      # End of verbatim block?
      $self->{IN_VERBATIM_BLOCK} = 0;
    }

    return undef;               # Don't let xscan_file process anything in a
                                # verbatim block.
  }

  if (s/^\s*\%\{//) {           # Start of verbatim block?
    $self->{IN_VERBATIM_BLOCK} = 1;
    return;
  }

  if(s/^\s*\#\s*(\w+)\s*//) {   # Ordinary C preprocessing statement?
    if ($1 eq 'include' &&
        !$self->{INCLUDEALL}) {
      return undef;             # Don't let xscan_file process #include
                                # statements unless -includeall was specified
                                # on the command line.
    }
    return $1;                  # Return the directive.
  }
  elsif (s/^\s*\%\s*(\w+)\s*//) { # Swig preprocessing statement?
    return "%$1";
  }
  return;
}

#
# Overrides Mpp::Scanner::C:other_directive.
# Return 0 if not interested, or undef to abort.
# This handles %include, %import, and magic around #include.
# Everything else gets handled by Mpp::Scanner::C:xscan_file.
#
sub other_directive {
  my ($self, $cp, $finfo, $conditional, $directive) = @_;

  if ($directive eq '%include' || $directive eq '%import' ||
      ($directive eq 'include' && $self->{INCLUDEALL})) {
    # Copied from xscan_file's handling for #include:
    $_ = $self->expand_macros($_) if $conditional;
    if(s/^\<([^>]*)\>//) {
      local $_; # Preserve $_ for later
      $self->include($cp, 'sys', $1, $finfo)
        or return undef;
    }
    elsif(s/^\"([^"]*)\"// ||
          s/(\S+)//) {          # Swig doesn't actually require quotes at all.
      local $_; # Preserve $_ for later
      $self->include($cp, 'user', $1, $finfo)
        or return undef;
    }
    return 1;                   # We handled this directive.
  }

  return 0;                     # Let xscan_file handle this directive.
}

1;
