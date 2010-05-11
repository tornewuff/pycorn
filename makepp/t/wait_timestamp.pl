# Wait till both the local clock and the timestamps of files are newer than
# those of the given files.  If no files are given, wait till the next second.

sub wait_timestamp(@) {
  my $ts = 0;
  my $tmp;
  # Pick out the newest file.
  for( @_ ) {
    $tmp = (stat)[9];
    $ts = $tmp if $tmp && $tmp > $ts;
  }
  # If none was given, default to now.
  $ts ||= time;
  my $hires = eval { require Time::HiRes };
  # Wait until the next second starts, if we can.
  if( $hires ) {
    $tmp = Time::HiRes::time();
    Time::HiRes::sleep( int( $tmp + 1 ) - $tmp );
    # Just in case something got rounded down, go forward in tiny steps:
    Time::HiRes::sleep( .01 ) while $ts >= time;
  } else {
    # More expensive than sleep, but we waste little real time.
    select undef, undef, undef, .1 while $ts >= time;
  }
  # If this is a remote file system, timestamps may not yet have changed.
  # So recheck in .1 second steps.
  while() {
    open my $fh, ">wait_timestamp.$$";
    # NOTE: Overwriting an existing empty file may not update its mod time,
    # so we also have to write to it.
    print $fh "x";
    last if (stat $fh)[9] > $ts;
    if( $hires ) {
      Time::HiRes::sleep( .1 );
    } else {
      select undef, undef, undef, .1;
    }
  }
  unlink "wait_timestamp.$$";
}

# If called as a standalone program or pseudo-standalone from run(), call the
# function.
wait_timestamp( @ARGV ) if !caller or caller eq 'Mpp::Subs';

1;
