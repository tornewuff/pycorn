# $Id: BuildCacheControl.pm,v 1.23 2008/05/17 14:21:36 pfeiffer Exp $

package BuildCacheControl;
use strict;
require Exporter;

our @ISA = 'Exporter';
our @EXPORT = qw(c_clean c_create c_show c_stats);

use FileInfo qw(file_info absolute_filename);
use BuildCache;
use FileInfo_makepp;
use Makecmds;
use POSIX ':errno_h';

BEGIN {
  *DEV = \&TextSubs::CONST0;
  *MODE = \&TextSubs::CONST2;
  *EXTLINK = \&TextSubs::CONST3;
  *UID = \&TextSubs::CONST4;
  *GID = \&TextSubs::CONST5;
  *BIUID = \&TextSubs::CONST6;
  *SIZE = sub() { 7 };
  *ATIME = sub() { 8 };
  *MTIME = sub() { 9 };
  *CTIME = sub() { 10 };

  *::propagate_pending_signals = \&TextSubs::CONST0 unless defined &::propagate_pending_signals;

  no warnings;
  *ESTALE = \&BuildCache::ESTALE; # Overridden on Win ActiveState.
}

=head2 group 'path/to/build_cache', ...

Recursively collect all build caches which can be found in the GROUP attribute
in the $BuildCache::options_file of all the given directories.  Actually the
file may contain two hashes, only the last of which is read by
BuildCache::new.  This function augments each object with values in the first
of the two hashes, if available.  After calling this function, these variables
are set:

=head3 @group

This is set to a list of one or more BuildCache objects.  These have more
attributes than the same objects in makepp:

    ..		The FileInfo of the build cache directory.
    PREFERRED	This is a preferred build cache if this key exists.

=head3 $preferred

This is set to the number of preferred build caches in the group.  These are
sorted at the beginning of C<@group>.

=head3 @unreachable (private)

This contains the directory names of caches which should have been loaded by
the above logic, but weren't, possibly because the disk or server is offline.

=cut

our @group;
our $preferred = 0;
my @unreachable;
sub group(@) {
  my %bc;
  my @list = @_;
  for( @list ) {
    my $dinfo = file_info $_;
    next if exists $bc{int $dinfo};
    $bc{int $dinfo} = $dinfo;

    my $opt = "$_/$BuildCache::options_file";
    unless( -r $opt ) {		# Disk or NFS server  might be down.
      push @unreachable, $_ if $! == ENOENT || $! == ENOTDIR;
      undef $bc{int $dinfo};	# Note it so we don't warn for it again.
      warn "Can't read $opt--$!\n";
      next;
    }
# do() fails to return list on one instance of 5.6.1, hence alternative on next line:
#    my @tmp = do $opt or die $@ =~ / $opt / ? $@ : "$opt: $@";
    open my $fh, '<', $opt; local $/; my @tmp = eval <$fh> or die $@ =~ / $opt / ? $@ : "$opt: $@";
    $tmp[-1]{'..'} = $dinfo;	# [0] for non grouped, [1] for grouped.
    $dinfo->{BC} = new BuildCache $_, $tmp[-1];

    if( @tmp > 1 ) {		# Was already grouped
      push @list, @{$tmp[0]{GROUP}} if exists $tmp[0]{GROUP};
				# Superset, in case GROUP got out of sync.
      $preferred++, undef $tmp[1]{PREFERRED} if exists $tmp[0]{PREFERRED};
    }
  }
  @group = sort {
    (exists $b->{PREFERRED} || 0) <=> (exists $a->{PREFERRED} || 0)
    or $a->{DIRNAME} cmp $b->{DIRNAME};
  } map defined() ? $_->{BC} : (), values %bc;
  die "$0: no group members were readable\n" unless @group;
}

=head2 ARGVgroups { code }

This calls C<group> for each element in C<@ARGV>, and calls I<code> for each
group that wasn't already identified by an earlier element.

=cut

our $blend;
my $blendopt = ['b', qr/blend(?:[-_]?groups?)?/, \$blend];
sub ARGVgroups(&) {
  unless( @ARGV ) {
    -f $BuildCache::options_file
      or die "$0: no build cache directories given and not in one\n";
    @ARGV = '.';
  }
  if( $blend ) {
    group @ARGV;
    &{$_[0]};
  } else {
    my %seen;
    for( @ARGV ) {		# Might specify more than one group.
      group $_;
      # TODO: warn if we have partially overlapping groups.
      next if exists $seen{int $group[0]{'..'}}; # Already handled this group.
      &{$_[0]};
      @seen{map int( $_->{'..'} ), @group} = (); # Remeber we've treated these BCs.
    }
  }
}

=head2 groupfind { code } [$try]

This function walks the virtual superposition of all caches in the group.
Call I<code> only once for every cache entry, whether it is in one cache of
the group or replicated to others.  The first argument to code is an array of
all the absolute path names in the different caches, virtually pointing to the
same directory.  The list is in the same order as C<@group>.  The second arg
is the path to the current file, relative to the cache root.

C<group> must have been called for this to work, even if the "group" consists
of only one build cache.  The subdir we are currently inspecting relative to
the build cache root is in C<$try>, and gets automatically added during the
recursive descent.

In addition to the parameters passed to I<code> there are some global
variables:

=head3 $_

This holds the name of the current file.  Mapping the concatenation of this to
the list of dirs gives the pathes to the replicates.

=head3 @lstats

This is a list of arrays containing the list returned by C<lstat>.  The list
is in the same order as C<@group>.  For inexistent files this contains undef
instead of an array.  For symbolic links this contains ext-links,
i.e. nlinks-1, the number of external links instead of an array.

Two fields have non-standard meanings:

    3 EXTLINK (nlink)  Number of links to the file, not counting the one in the cache.
    6 BIUID (rdev)     The uid of the build info or undef if none.

=head3 @combined_lstat

This is the virtual lstat for the file, where the times are the maximum of all
replicates' times.  EXTLINK is the sum of all replicates' EXTLINK.

=cut

our( @lstats, @combined_lstat );
our $clean_empty;
sub groupfind(&;$) {
  my( $code, $try ) = @_;
  my $top = 1 unless defined $try;
  my( @dirs, @contents );
  @dirs = map $top ? $_->{DIRNAME} : "$_->{DIRNAME}/$try", @group;
  for( @dirs ) {
    if( opendir my( $dh ), $_ ) {
      my %contents;
      @contents{(readdir $dh)} = (); # Parens needed for list context to readdir.
      delete @contents{qw(. ..), $FileInfo::build_info_subdir};
      delete @contents{$BuildCache::options_file, $BuildCache::incoming_subdir}
	if $top;
      push @contents, \%contents;
    } else {
      push @contents, undef;
    }
  }
  my %combined_contents;
  @combined_contents{keys %$_} = () for @contents; # Merge all the individual contents.
				# Merely make the keys exist.
  FILE: for( keys %combined_contents ) {
    @combined_lstat = @lstats = ();
    $combined_lstat[EXTLINK] = 0;
    for( my $i = 0; $i < @dirs; $i++ ) { # Look at all group members.
      unless( exists $contents[$i]{$_} ) { # Not present in this cache.
	push @lstats, undef;	# Placeholder so we stay in sync with @group.
	next;
      }
      unless( defined -l "$dirs[$i]/$_" ) { # What's wrong, concurrent clean?
	my $msg = "$0: lstat $_: $!\n";
	if( $! == ENOENT || $! == ESTALE ) {
	  warn $msg;
	  next;
	}
	die $msg;
      };
      if( -l _ ) {
	push @lstats, (lstat _)[EXTLINK] - 1;
				# nlink: Don't count cached symlink itself.
	$combined_lstat[EXTLINK] += $lstats[-1];
      } elsif( -d _ ) {
	&groupfind( $code, $top ? $_ : "$try/$_" ); # Ignore prototype for $code.
	next FILE;		# We just treated whole group recursively.
      } else {			# A plain file.
	push @lstats, [lstat _];
	$combined_lstat[EXTLINK] += --$lstats[-1][EXTLINK];
				# nlink: Don't count cached file itself.
	@combined_lstat[MODE, UID, SIZE] = @{$lstats[-1]}[MODE, UID, SIZE];
	!defined $combined_lstat[$_] || $combined_lstat[$_] < $lstats[-1][$_]
	  and $combined_lstat[$_] = $lstats[-1][$_]
	  for ATIME, MTIME, CTIME;	# Max.
	defined( $lstats[-1][BIUID] =	# Redefine field from what lstat put there.
		 (lstat "$dirs[$i]/$FileInfo::build_info_subdir/$_.mk")[UID] )
	  and !-l _		# Real build_info file?
	  and $combined_lstat[BIUID] = $lstats[-1][BIUID];
      }
    }
    &$code( \@dirs, $top ? $_ : "$try/$_" );
  }

  # This is only used by clean.  Have to do it here, as callback is only for files:
  if( $clean_empty ) {
    DIR: for( map( "$_/$FileInfo::build_info_subdir", @dirs ), @dirs ) {
      opendir my( $dh ), $_ or next;
      my $entry;
      $entry =~ /^\.\.?$/ or next DIR while $entry = readdir $dh;
      closedir $dh;
      rmdir or warn "$0: can't delete `$_'--$!\n";
    }
  }
}


sub c_clean {
  local @ARGV = @_;
  my( $min_atime, $atime, $max_atime,
      $min_mtime, $mtime, $max_mtime,
      $min_inc_mtime, $inc_mtime, $max_inc_mtime,
      $min_ctime, $ctime, $max_ctime,
      $min_size, $size, $max_size,
      $bi_check, $link_check, $group, $user, $predicate, $weekbase);
  my %unit =
    (s => 1,
     m => 60,
     h => 60 * 60,
     d => 24 * 60 * 60,
     w => 7 * 24 * 60 * 60);
  $unit{''} = $unit{d};
  my $time = time;
  $inc_mtime = '+2h';		# default is 2 hours old or older.

  my ($target_files_deleted, $build_info_files_deleted) = (0, 0);

  Makecmds::frame {
    if( $weekbase ) {
      $weekbase = $unit{w};	# 7 days after epoch.
      my( $min, $hour, $wday ) = (localtime $weekbase)[1, 2, 6];
      $weekbase -= --$wday * $unit{d} + $hour * $unit{h} + $min * $unit{m};
				# Count back to monday 0:00.
    }
    map {
      if( defined $_->[1] ) {
	%unit =
	  ('' => 1,
	   c => 1,
	   k => 2 ** 10,
	   M => 2 ** 20,
	   G => 2 ** 30) if $_->[3];
	# '+-1' is useful for testing.  We rely on ([-+]?) being ungreedy here.
	$_->[1] =~ /^([-+]?)(\d+(?:\.\d+)?|-1)([wdhmsckMG]?)/ or
	  die "$0: `$_->[1]' is not a valid specification\n";
        # We unlink the ones that are IN the range, so '+' (unlink older than)
        # means to set the max, and '-' (unlink newer than) means to set the
        # min (except that size is opposite).
        if($_->[3]) { # size
	  if( $1 eq '-' ) {
	    ${$_->[2]} = $2 * $unit{$3}; # max
	  } else {
	    ${$_->[0]} = $2 * $unit{$3}; # min
	    ${$_->[2]} = ${$_->[2]} + $unit{$3} if !$1; # range
	  }
        } else { # time
	  if( $1 eq '-' ) {
	    ${$_->[0]} = $time - $2 * $unit{$3}; # min
	  } else {
	    ${$_->[2]} = $time - $2 * $unit{$3}; # max
	    ${$_->[0]} = ${$_->[2]} - $unit{$3} if !$1; # range
	  }
	  if( defined $weekbase ) {
	    defined and
	    $_ -= (int( ($time - $weekbase) / $unit{w} ) - int( ($_ - $weekbase) / $unit{w} )) *
				# Count both weeks since monday after the epoch.
	      2 * $unit{d}	# Subtract number of weeks times 2 days.
	      for ${$_->[0]}, ${$_->[2]};
	  }
        }
      }
    } [\$min_atime,	$atime,		\$max_atime],
      [\$min_mtime,	$mtime,		\$max_mtime],
      [\$min_inc_mtime,	$inc_mtime,	\$max_inc_mtime],
      [\$min_ctime,	$ctime,		\$max_ctime],
      [\$min_size,	$size,		\$max_size, 1]; # NOTE: $size must be last!
    $min_inc_mtime and die "$0: minimum incoming mtime not supported\n";

    # Traverse desired filesystems
    local $clean_empty = 1;
    local $::force_bc_copy = 1;
    ARGVgroups {		# Might specify more than one group.
      # Special rule for incoming subdir:
      for( @group ) {
	my $inc = "$_->{DIRNAME}/$BuildCache::incoming_subdir";
	opendir my( $dh ), $inc or next;
	-e "$inc/$_" && !-d _ && (stat _)[MTIME] < $max_inc_mtime && unlink "$inc/$_"
	  for readdir $dh;
      }

      my $delete = sub {
	my $file = $_[0];	# Copy, because perform { } has own @_.
	eval { Makecmds::perform { unlink $file } "delete `$file'" };
	if( $::verbose ) {
	  if( $@ ) { warn $@ }
	  else { ++$target_files_deleted }
	}
	if( @_ == 1 || unlink $_[1] ) {
	  ++$build_info_files_deleted;
	} elsif( $::verbose ) {
	  warn "unlink $_[1]--$!\n";
	}
      };

      my $round_robin = 0;
      groupfind {
	if( $combined_lstat[EXTLINK] ) { # File has external links.
	RETAIN:
	  my( $found_idx, $found, $found_build_info, $found_extlink );
	  for( my $i = 0; $i < @group; $i++ ) {
	    next unless ref $lstats[$i]; # Look at all real group members.
	    my $build_info = "$_[0][$i]/$FileInfo::build_info_subdir/$_.mk";
	    undef $build_info unless -f $build_info;
	    my $file = "$_[0][$i]/$_";
	    if( $build_info and $bi_check ? defined FileInfo::load_build_info_file file_info $file : 1 ) {
	      if( defined $user && $user != $lstats[$i][UID] ) {
		$lstats[$i][UID] = $user;
		Makecmds::perform { chown $user, $lstats[$i][GID], $file } "set owner $user for `$file'";
	      }
	      if( !defined $found_idx || $preferred && $i < $preferred && $found_extlink < $lstats[$i][EXTLINK] ) {
		$found_idx = $i;
		$found = $file;
		$found_build_info = $build_info;
		$found_extlink = $lstats[$i][EXTLINK];
	      }
	    } elsif( $time - $lstats[$i][MTIME] > 600 ) { # Missing or corrupted build info (see BuildCache::fix_ok).
	      &$delete( $file ); # load_build_info_file wiped build_info.
	    }
	  }
	  goto UNLINK unless $found;
	  if( $preferred && $found_idx >= $preferred ) {
				# Found a file but not in a preferred BC.
	    for my $i ( $round_robin+1..$preferred-1, 0..$round_robin, undef ) {
	      return unless defined $i;	# No free slot in any preferred BC.
	      unless( $lstats[$i] ) { # No file or symlink without ext links.
		&$delete( "$_[0][$i]/$_", "$_[0][$i]/$FileInfo::build_info_subdir/$_.mk" )
		  if defined $lstats[$i]; # Symlink is in the way.
		$round_robin = $i;
		last;
	      }
	    }
	    if( $group[$round_robin]->cache_file( file_info( $found ), "$_[0][$round_robin]/$_", \(my $reason), $lstats[$found_idx][ATIME] )) {
				# Succeeded in copying it, pretend it's the one we found.
	      $found = "$_[0][$round_robin]/$_";
	      $found_build_info = "$_[0][$round_robin]/$FileInfo::build_info_subdir/$_.mk";
				# Copy file attrs too:
	      chown @{$lstats[$found_idx]}[UID, GID], $found;
	      chown @{$lstats[$found_idx]}[BIUID, GID], $found_build_info;
	      @{$lstats[$round_robin] ||= []}[ATIME, MTIME, UID, GID, BIUID] =
		@{$lstats[$found_idx]}[ATIME, MTIME, UID, GID, BIUID];
	      $found_idx = $round_robin;
	    }
	  }
	  my $copied;
	  for( my $i = 0; $i < @group; $i++ ) {
	    next if $i == $found_idx;
	    my $build_info = "$_[0][$i]/$FileInfo::build_info_subdir/$_.mk";
	    if( $lstats[$i] ) {
	      next unless ref $lstats[$i] && !$lstats[$i][EXTLINK] && exists $group[$i]{SYMLINK};
	      &$delete( "$_[0][$i]/$_", $build_info );
	    } elsif( defined $lstats[$i] ) { # Symlink with no ext links.
	      next unless $link_check;
	      next if $found eq readlink "$_[0][$i]/$_"
		&& $found_build_info eq readlink $build_info;
	      &$delete( "$_[0][$i]/$_", $build_info );
	    } elsif( lstat $build_info ) { # Build info without file
	      $time - (lstat _)[MTIME] > 600 and
		unlink $build_info and
		++$build_info_files_deleted;
	    }
	    if( exists $group[$i]{SYMLINK} ) {
	      -d "$_[0][$i]/$FileInfo::build_info_subdir"
		|| eval { Makecmds::c_mkdir( $group[$i]{MKDIR_OPT}, "$_[0][$i]/$FileInfo::build_info_subdir" ) }
		and symlink $found_build_info, $build_info
		and symlink $found, "$_[0][$i]/$_";
	    } elsif( $group[$i]->cache_file( file_info( $found ), "$_[0][$i]/$_", \(my $reason), $lstats[$found_idx][ATIME] )) {
	      $copied = 1;
	      # Copy owners too:
	      chown @{$lstats[$found_idx]}[UID, GID], "$_[0][$i]/$_";
	      chown @{$lstats[$found_idx]}[BIUID, GID], $build_info;
	    }
	  }
	  utime @{$lstats[$found_idx]}[ATIME, MTIME], $found
	    if $copied;		# Don't note cache_file as a read.
	} else {		# Clean only matching files not used elsewhere.
				# There may still be copies though.
	  if( $predicate ) {
	    my $value = &$predicate;
	    goto UNLINK if $value;
	    goto RETAIN if defined $value;
	  }

	  map {			# Test against deletion options.
	    goto RETAIN
	      if defined $_->[0] &&	      $combined_lstat[$_->[1]] < $_->[0]
	      or defined $_->[2] && $_->[2] < $combined_lstat[$_->[1]]; # Found one that's out of bounds.
	  } [$min_atime, ATIME, $max_atime],
	    [$min_mtime, MTIME, $max_mtime],
	    [$min_ctime, CTIME, $max_ctime],
	    [$min_size,  SIZE,  $max_size]
	    if defined $combined_lstat[UID]; # Do we have a real file at all?
	UNLINK:
	  for( my $i = 0; $i < @group; $i++ ) {
	    &$delete( "$_[0][$i]/$_", "$_[0][$i]/$FileInfo::build_info_subdir/$_.mk" )
	      if defined $lstats[$i]; # Look at all group members.
	  }
	}
      };
    };

    print "Deleted $target_files_deleted target files and $build_info_files_deleted build info files.\n"
      if $::verbose;
  } ['a', qr/a(?:ccess[-_]?)?time/, \$atime, 1],
    $blendopt,
    ['c', qr/c(?:hange[-_]?)?time/, \$ctime, 1],
    ['g', qr/(?:new[-_]?)?gro?u?p/, \$group, 1,
     sub {
       defined( $group = getgrnam $group ) or die "$0: group unknown\n" if $group !~ /^\d+$/;
       $( = $) = $group;
       die "$0: newgrp $group failed--$!\n" if $!;
     }],
    ['i', qr/(?:build[-_]?)?info(?:[-_]?check)?/, \$bi_check],
    ['l', qr/(?:sym(?:bolic)?[-_]?)?link(?:[-_]?check)?/, \$link_check],
    ['m', qr/m(?:odification[-_]?)?time/, \$mtime, 1],
    ['M', qr/in(?:coming)?[-_]?m(?:odification[-_]?)?time/, \$inc_mtime, 1],
    ['p', qr/p(?:erl|redicate)/, \$predicate, 1,
     sub { $predicate = Makecmds::eval_or_die( "sub { $predicate }" ) }],
    [qw(s size), \$size, 1],
    ['u', qr/(?:set[-_]?)?user/, \$user, 1,
     sub { defined( $user = getpwnam $user ) or die "$0: user unknown\n" if $user !~ /^\d+$/ }],
    [qw(w workdays), \$weekbase];
}


#
# Create the build cache for the first time.
#
sub c_create {
  local @ARGV = @_;
  my( $extend, $force, $mode, $preferred, $subdir_chars );
  Makecmds::frame {
    @ARGV or die "$0: no build cache directories given\n";

    if( defined $mode ) {
      $mode =~ /^[0-7]+$/ or die "$0: mode `$mode' is not octal\n";
      substr $mode, 0, 0, 'm';	# &mkdir -p gets prepended below
    } else {
      $mode = '';
    }
    my $group_subdir_chars;
    if( defined $extend ) {
      group $extend;
      for( @group ) {
	if( defined $group_subdir_chars ) {
	  $group_subdir_chars eq join ',', @{$_->{SUBDIR_CHARS}} or
	    die "$0: error: `$group[0]{DIRNAME}' and `$_->{DIRNAME}' have different --subdir-chars\n";
	} else {
	  $group_subdir_chars = join ',', @{$_->{SUBDIR_CHARS}};
	}
      }
    }
    if( defined $subdir_chars ) {
      $subdir_chars =~ tr/ \t//d;
      defined $group_subdir_chars and $group_subdir_chars ne $subdir_chars and
	die "$0: error: `$group[0]{DIRNAME}' has different --subdir-chars=$group_subdir_chars\n";

      my $last_len = 0;         # Do some quick validation:
      for( split ',', $subdir_chars ) {
        /^\d+$/ or
          die "$0: error: specify a list of numbers to --subdir-chars\n";
        $_ > $last_len or
          die "$0: error: parameters to --subdir-chars must be in increasing order\n";
	$last_len = $_;
      }
    } else {
      $subdir_chars = $extend ? $group_subdir_chars : '2,4';
				# Fall back to the default directory configuration.
    }

    for( @ARGV ) {
      if( -l or -e _ ) {
	die "$0: error: `$_' already exists\n" unless $force;
	unlink or die "$0: error: can't remove `$_'--$!\n"
	  unless -d;
      }
      if( $extend || @ARGV > 1 ) {
	$_ = { DIRNAME => $_, '..' => file_info $_ };
	undef $_->{PREFERRED} if $preferred;
      } else {
	$_ = { DIRNAME => $_ };
      }
    }
    Makecmds::c_mkdir '-p' . $mode, map "$_->{DIRNAME}/$BuildCache::incoming_subdir", @ARGV;
    push @ARGV, @group if $extend;
    for( @ARGV ) {
      my $str;
      if( @ARGV > 1 ) {
	my $self = $_->{'..'};
	$str .= "no warnings 'void'; # Scalar context skips next line.\n{ GROUP => [qw(" .
	  (Makesubs::f_sort # f_sort eliminates dups, which come from re-adding a lost group member.
	    join ' ', @unreachable, map { $_->{'..'} == $self ? () : absolute_filename $_->{'..'} } @ARGV ) .
	  ')]';
	$str .= ', PREFERRED => undef' if exists $_->{PREFERRED};
	$str .= " },\n{ ";
      } else {
	$str = '{ ';
      }
      $str .= "SUBDIR_CHARS => [$subdir_chars]";
      if( @ARGV > 1 ) {
	my( $y, $z ) = ("$_->{DIRNAME}/.y", "$_->{DIRNAME}/.z");
				# Prepend '.' which doesn't occur in bc keys, in
				# case we are forcing creation of an existing cache.
	my $symlink = eval { symlink 'x', $y };
	$symlink &&= link $y, $z; # Can we link to a stale symlink?  Stale, as some systems
				# link to the linked file, which only works on same fs.
	unlink $y, $z;
	$str .= ', SYMLINK => undef' if $symlink;
      }
      Makecmds::c_echo "$str }", -o => "$_->{DIRNAME}/$BuildCache::options_file";
    }
  } ['e', qr/extend(?:[-_]?group)?/, \$extend, 1],
    [qw(f force), \$force],
    ['m', qr/mode|access[-_]?permisssions/, \$mode, 1],
    [qw(p preferred), \$preferred],
    ['s', qr/subdir[-_]?chars/, \$subdir_chars, 1];
}


sub showtime($) {
  my @time = localtime $_[0];
  sprintf "%s %02d-%02d-%02d %02d:%02d:%02d",
    qw(Su Mo Tu We Th Fr Sa)[$time[6]],
    $time[5] % 100,
    $time[4] + 1,
    @time[3, 2, 1, 0];
}

sub showfull($$$@) {
  if( defined $_[0][MODE] ) {
    my $grfmt = exists $_[4] ? "    copies: %d    sym-links: %d\n" : '';
    printf "%s
  mode: %04o    ext-links: %d  $grfmt  uid: %s    bi-uid: %s    size: %d
  atime: %s
  mtime: %s
  ctime: %s\n", $_[1],
      $_[0][MODE] & 07777,
      $_[0][EXTLINK],
      exists $_[4] ? @_[3, 4] : (),
      @{$_[0]}[UID, BIUID, SIZE],
      map {
	my $res = showtime( $_ ) . '  (';
	$_ = $_[2] - $_;
	$res . int( $_ / (24 * 60 * 60) ) . 'd or ' .
	  int( $_ / (60 * 60) ) . 'h or ' .
	  int( $_ / 60 ) . 'm)';
      } @{$_[0]}[ATIME, MTIME, CTIME];
  } elsif( exists $_[4] ) {
    printf "%s
  ext-links: %d    sym-links: %d\n", $_[1], $_[0][EXTLINK], $_[4];
  } else {
    printf "%s
  ext-links: %d\n", $_[1], $_[0][EXTLINK];
  }
}

#
# This is a sort of recursive stat command, which takes into account that the
# owner of the cached file may have been changed, while the build info file
# retains the original owner.
#
sub c_show {
  local @ARGV = @_;
  my( $atime, $ctime, $deletable, $pattern, %user, $sep );
  my $time = time;
  my $sort;

  Makecmds::frame {
    warn "$0: ignoring --sort with --verbose\n" if defined $sort && $::verbose;
    for( $pattern ) {
      last unless defined;
      s/([?*])/.$1/g;
      s/\{/(?:/g and tr/,}/|)/;
      $_ = qr/_$_$/;
    }
    my @sort = split /[\s,]+/, defined $sort ? $sort : 'MEMBER,AGE';
    ARGVgroups {
      my( $grtitle, $grfmt, $grnone, $offset, @sortidxlen, %sort ) =
	@group > 1 ? ('C S ', '%d %d ', '- %d ', 4) :
	  ('', '', '', 0);
      my $timetype = $atime ? 'A' : $ctime ? 'C' : 'M';
      for my $key ( @sort ) {
	$key = uc $key;
	map {
	  if( $_->[0] eq $key ) {
	    push @sortidxlen, $_->[1], $_->[2];
	    next;
	  }
	} [MODE => 0, 4],
	  [EL => 5, 2],
	  [C => 8, 1],
	  [S => 10, 1],
	  [UID => 8 + $offset, 8],
	  ['BI-UID', 17 + $offset, 1],
	  [SIZE => 26 + $offset, 9],
	  ["${timetype}D" => 36 + $offset, 2],
	  [AGE => 39 + $offset, 17],
	  ["${timetype}DATE", 39 + $offset, 8],
	  ["${timetype}TIME", 48 + $offset, 8],
	  [MEMBER => -57 - $offset, -1];
      }
      $sep = "MODE EL ${grtitle}UID      BI-UID        SIZE ${timetype}D ${timetype}DATE    ${timetype}TIME    MEMBER\n"
	unless $::verbose;
      groupfind {
	return if $deletable && ($combined_lstat[EXTLINK] && defined $combined_lstat[BIUID])
	  or defined $pattern && !/$pattern/;
	$_ = defined() ? $user{$_} ||= getpwuid( $_ ) || $_ : '-'
	  for @combined_lstat[UID, BIUID];
	if( defined $sep ) {
	  print $sep;
	  undef $sep;
	}
	my @grinfo;
	if( @group > 1 ) {	# Count the copies and symlinks.
	  @grinfo = (0, 0);
	  for( @lstats ) {
	    $grinfo[ref() ? 0 : 1]++ if defined;
	  }
	}
	if( $::verbose ) {
	  showfull \@combined_lstat, $_[1], $time, @grinfo;
	  if( $::verbose > 1 ) { # Show each individual member.
	    for( my $i = 0; $i < @lstats; $i++ ) {
	      next unless defined $lstats[$i];
	      my $file = "$_[0][$i]/$_";
	      if( ref $lstats[$i] ) { # Normal file
		$_ = defined() ? $user{$_} ||= getpwuid( $_ ) || $_ : '-'
		  for @{$lstats[$i]}[UID, BIUID];
		showfull $lstats[$i], $file, $time;
	      } else {
		print "$file -> " . readlink( $file ) . "\n";
	      }
	    }
	    $sep = "\n";
	  }
	} else {
	  my $res;
	  if( defined $combined_lstat[MODE] ) { # A real file.
	    $res = sprintf "%04o %2d $grfmt%-8s %-8s %9d %s %s\n",
	      $combined_lstat[MODE] & 07777,
	      $combined_lstat[EXTLINK], @grinfo,
	      @combined_lstat[UID, BIUID, SIZE],
	      showtime $combined_lstat[$atime ? ATIME : $ctime ? CTIME : MTIME],
	      $_[1];
	  } else {		# Only stale symlink(s).
	    shift @grinfo;	# Doesn't have copies.
	    $res = sprintf "-    %2d $grnone-        -                - -  -        -        %s\n",
	      $combined_lstat[EXTLINK], @grinfo,
	      $_[1];
	  }
	  if( @sort ) {
	    my $key = '';
	    for( my $i = 0; $i < @sortidxlen; $i += 2 ) {
	      my( $idx, $len ) = @sortidxlen[$i, $i+1];
	      $idx = 1 + index $res, '_', $idx if $idx < 0; # Name starts after _
	      $key .= substr $res, $idx, $len;
	    }
	    if( exists $sort{$key} ) {
	      $sort{$key} .= $res;
	    } else {
	      $sort{$key} = $res;
	    }
	  } else {
	    print $res;
	  }
	}
      };
      if( @sort ) {
	print $sort{$_} for sort keys %sort;
      }
      $sep = "\f\n" if $::verbose;
    };
  } 'f', qw(o O),		# fails in 5.6: qw(f o O);
    ['a', qr/a(?:ccess[-_]?)?time/, \$atime],
    $blendopt,
    ['c', qr/c(?:hange[-_]?)?time/, \$ctime],
    [qw(d deletable), \$deletable],
    [qw(p pattern), \$pattern, 1],
    [qw(s sort), \$sort, 1];
}



sub cumul($\@\@) {
  my( $val, $asc, $desc ) = @_;
  my $last = @$val - 1;

  for my $i ( 0..$last ) {
    if( $i ) {
      $asc->[$i] = $asc->[$i - 1] + ($val->[$i] ||= 0);
      $desc->[$last - $i] = $desc->[$last - $i + 1] + ($val->[$last - $i] ||= 0);
    } else {
      $asc->[0] = $val->[0] ||= 0;
      $desc->[$last] = $val->[$last] ||= 0;
    }
  }
}
my $sep = '';
sub display($$$;$) {
  my( $size, $files, $title, $idx ) = @_;
  return unless @$size;
  my $last = @$size - 1;
  my( @size_asc, @size_desc, @files_asc, @files_desc );
  cumul $size, @size_asc, @size_desc;
  cumul $files, @files_asc, @files_desc;

  my $name_len = $idx ? length $idx->[$last] : 1 + int log( $last ) / log 10;
  $name_len = length $title->[0] if $name_len < length $title->[0];

  my $size_len = 1 + int log( $size_asc[$last] ) / log 10;
  $size_len = length $title->[-2] if $size_len < length $title->[-2];
  $size_len = length 'CUMUL' if $size_len < length 'CUMUL';
  my $size_fmt = "%${size_len}s      %%";

  my $files_len = 1 + int log( $files_asc[$last] ) / log 10;
  $files_len = length $title->[-1] if $files_len < length $title->[-1];
  $files_len = length 'CUMUL' if $files_len < length 'CUMUL';
  my $files_fmt = "%${files_len}s      %%";

  printf "$sep%${name_len}s | $size_fmt   $size_fmt   $size_fmt | $files_fmt   $files_fmt   $files_fmt\n",
    @{$title}[0..@$title-2], qw(CUMUL CUMUL), $title->[-1], qw(CUMUL CUMUL);

  $size_fmt = "%$size_len.0f %6.2f";
  $files_fmt = "%$files_len.0f %6.2f";
  my $fmt = "%${name_len}s | $size_fmt   $size_fmt   $size_fmt | $files_fmt   $files_fmt   $files_fmt\n";
  for my $i ( 0..$last ) {
    printf $fmt,
      $idx ? $idx->[$i] : $i,

      $size->[$i], 100 * $size->[$i] / $size_asc[$last],
      $size_asc[$i], 100 * $size_asc[$i] / $size_asc[$last],
      $size_desc[$i], 100 * $size_desc[$i] / $size_asc[$last],

      $files->[$i], 100 * $files->[$i] / $files_asc[$last],
      $files_asc[$i], 100 * $files_asc[$i] / $files_asc[$last],
      $files_desc[$i], 100 * $files_desc[$i] / $files_asc[$last]
      if $size->[$i];
  }
  $sep = "\n";
}

sub c_stats {
  my( $hours, $pattern );
  my $time = time;
  Makecmds::frame {
    for( $pattern ) {
      last unless defined;
      s/([?*])/.$1/g;
      s/\{/(?:/g and tr/,}/|)/;
      $_ = qr/_$_$/;
    }
    ARGVgroups {		# Might specify more than one group.
      my( @atime_size, @atime_count, @ctime_size, @ctime_count, @mtime_size, @mtime_count,
	  @el_size, @el_count, %cs_size, %cs_count );
      groupfind {
	return if defined $pattern && !/$pattern/;
	no warnings 'uninitialized';

	# Count and sum by atime hours.
	my $hour = $time - $combined_lstat[ATIME];
	$hour = $hour < 0 ? 0 : int $hour / 3600 / ($hours ? 1 : 24);
	$atime_size[$hour] += $combined_lstat[SIZE];
	$atime_count[$hour]++;

	# Count and sum by ctime hours.
	$hour = $time - $combined_lstat[CTIME];
	$hour = $hour < 0 ? 0 : int $hour / 3600 / ($hours ? 1 : 24);
	$ctime_size[$hour] += $combined_lstat[SIZE];
	$ctime_count[$hour]++;

	# Count and sum by mtime hours.
	$hour = $time - $combined_lstat[MTIME];
	$hour = $hour < 0 ? 0 : int $hour / 3600 / ($hours ? 1 : 24);
	$mtime_size[$hour] += $combined_lstat[SIZE];
	$mtime_count[$hour]++;

	# Count and sum by external links.
	$el_size[$combined_lstat[EXTLINK]] += $combined_lstat[SIZE];
	$el_count[$combined_lstat[EXTLINK]]++;

	if( @group > 1 ) {
	  # Count and sum by combination of number of copies and symlinks.
	  my( $copies, $symlinks ) = (0, 0);
	  defined and ref() ? $copies++ : $symlinks++ for @lstats;
	  $copies .= ":$symlinks";
	  $cs_size{$copies} += $combined_lstat[SIZE];
	  $cs_count{$copies}++;
	}
      };

      # Display by timestamps.
      display \@atime_size, \@atime_count, [$hours ? 'AH' : 'AD', qw(SIZE FILES)];
      display \@ctime_size, \@ctime_count, [$hours ? 'CH' : 'CD', qw(SIZE FILES)];
      display \@mtime_size, \@mtime_count, [$hours ? 'MH' : 'MD', qw(SIZE FILES)];

      # Display by external links.
      display \@el_size, \@el_count, [qw(EL SIZE FILES)];
      undef $el_size[0];
      undef $el_count[0];
      for( my $i = 1; $i < @el_size; $i++ ) {
	next unless defined $el_size[$i];
	$el_size[$i] *= $i;
	$el_count[$i] *= $i;
      }
      display \@el_size, \@el_count, [qw(EL *SIZE *FILES)];

      # Display by combination of number of copies and symlinks.
      my @cs_keys = sort keys %cs_size;
      display [@cs_size{@cs_keys}], [@cs_count{@cs_keys}], [qw(C:S SIZE FILES)], \@cs_keys;
    }
  } [qw(h hours), \$hours],
    [qw(p pattern), \$pattern, 1];
}


no warnings 'redefine';
sub ::metahelp { print STDERR <<EOF }
usage:	makepp_build_cache_control command [option ...] directory ...
	mppbcc command [option ...] directory ...
	makeppbuiltin -MBuildCacheControl command [option ...] directory ...
	mppb -MBuildCacheControl command [option ...] directory ...
  available commands:	clean, create, show
  to see options do:	makepp_build_cache_control command --help
EOF

sub ::helpfoot { die <<'EOF' }

Look at @htmldir@/makepp_build_cache.html for more details,
or at http://makepp.sourceforge.net/@BASEVERSION@/makepp_build_cache.html
or type "man makepp_build_cache".
EOF

1;
