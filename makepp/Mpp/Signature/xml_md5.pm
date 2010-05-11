utf-16be FEFF A: 0041
utf-16le FFFE A: 4100
utf-8 EFBBBF A: 41


# $Id: Text.pm,v 1.41 2009/02/10 22:55:49 pfeiffer Exp $

=head1 NAME

Mpp::Signature::xml_md5 - a signature class that ignores changes to comments and optionally whitespace

=cut

use strict;
package Mpp::Signature::xml_md5;
use Digest::MD5;
our @ISA = qw(Mpp::Signature);

our $xml_md5 = bless \@ISA; # Make the singleton object.

# Things that can be overridden by a derived class:
sub build_info_key { 'XML_MD5_SUM' }
sub important_comment_keywords { qw// }
sub excludes_file { is_object_or_library_name( $_[1]->{NAME} ) }
sub recognizes_file { is_cpp_source_name( $_[1]->{NAME} ) }

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
  #my( $self, $fname ) = @_;	# Name the arguments.

  open my $infile, $_[1] or	# File exists?
    return '';

  my $ret_str = '';

  local $_ = $_[0] if @_;
  s/\s+\Z//;			# Space outside of root tag is not relevant.
  pos = 0;			# Start at beginning of string.

  for (;;) {			# Stuff before root tag.
    /\G\s+/gc;
    if( /\G<(\?[-:.\w]+)/gc ) {	# Special initial tag?
      my $tag = $1;
      my %attributes;
      for (;;) {
	/\G\s+/gc;
	last if /\G\?>/gc;	# Special end of initial tag?
	if( /\G([-:.\w]+)/gc ) {
	  if( /\G([-:.\w]+)/gc ) {
	    my $attribute = $1;
	    if( /\G=/gc ) {
	      $attributes{$attribute} = $1 if /\G"(.*?)"/gc or /\G'(.*?)'/gc or /\G([^\s?>]*/gc;
	    } else {
	      $attributes{$attribute} = $attribute; # HTML style
	    }
	  }
	}
      }
    } elsif( !/\G<!--.*?-->/gc ) { # Not comment?
      last;
    }
  }

  for (;;) {
    $space and /\G\s+/gc and $ret_str .= ' '; # Skip over space
    /\G([^"'\\]+)/gc and $ret_str .= $1; # Skip over ordinary characters.
    last if length() <= pos;

    if (/\G</gc) {		# Tag?
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

1;
