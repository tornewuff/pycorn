# $Id: Rule.pm,v 1.92 2008/08/04 21:57:01 pfeiffer Exp $
use strict qw(vars subs);

package Rule;

use MakeEvent qw(when_done wait_for);
use FileInfo qw(file_info chdir absolute_filename relative_filename file_exists touched_filesystem);
use FileInfo_makepp;
use TextSubs;
use BuildCheck::exact_match;
use ActionParser;
use Makecmds;

our $unsafe;			# Perl code was performed, that might leave files open or chdir.

=head1	NAME

Rule -- Stores information about a build rule

=head1 USAGE

  my $rule = new Rule "targets", "dependencies",
    "command"[, $makefile, $makefile_line];

=head1 DESCRIPTION

A rule object contains all the information necessary to make a specific file.

$makefile is a pointer to the structure returned by load_makefile.
$makefile_line is an ASCII string that is used if an error message occurs
while expanding variables in this rule.

This does not actually place the rule into the FileInfo hierarchy.

=cut

sub new {
  my ($class, $targets, $dependencies, $command, $makefile, $source_line) = @_;
				# Name the arguments.

  bless { TARGET_STRING => $targets,
	  DEPENDENCY_STRING => $dependencies,
	  COMMAND_STRING => $command,
	  MAKEFILE => $makefile,
	  LOAD_IDX => $makefile->{LOAD_IDX},
	  RULE_SOURCE => $source_line }, $class;
				# Make the rule object.
}

#
# Return the directory that this rule should be executed in.
#
sub build_cwd { $_[0]{MAKEFILE}{CWD} }

#
# Return a build cache associated with this rule, if any.
#
# A build cache may be specified for each rule, or for a whole makefile,
# or for all makefiles (on the command line).
#
sub build_cache {
  exists $_[0]{BUILD_CACHE} ? $_[0]{BUILD_CACHE} :
  exists $_[0]{MAKEFILE}{BUILD_CACHE} ? $_[0]{MAKEFILE}{BUILD_CACHE} :
  $::global_build_cache;
}

#
# Set the build cache for this rule.
#
sub set_build_cache {
  $_[0]{BUILD_CACHE} = $_[1];
}

#
# Return the makefile that made this rule:
#
sub makefile { $_[0]{MAKEFILE} }

#
# This subroutine is called to find all the targets and dependencies of the
# rule.	 It does so by repeatedly expanding the dependency and target string,
# and repeatedly scanning the command, until there are no further changes.
#
# Usage:
#   ($all_targets, $all_dependencies, $action_string, $env_deps) =
#     $rule->find_all_targets_dependencies($oinfo);
#
# Where all_targets is a reference to a list of target object info structs,
# all_dependencies is a reference to a list of dependency object info structs,
# $action_string is the final build command after expanding all make
# variables, and $env_deps is a reference to a hash mapping environmental
# dependencies to current values. $oinfo is the requested target, which is
# used only to determine where to look for cached scan information.
#
sub find_all_targets_dependencies {
  my ($self, $oinfo, $dont_scan) = @_;

  my $build_cwd = $self->build_cwd; # Access the default directory.

  my %all_targets;		# A hash of all targets, so we can tell quickly
				# whether we already know about a given target.
  local $Makesubs::rule = $self; # Set this up so that subroutines can find the
				# rule that is currently being expanded.

  local $self->{ALL_TARGETS} = \%all_targets;
				# Store this so it can be found easily but
				# also goes away automatically.

  my %all_dependencies;		# A hash of all dependencies, so we can tell
				# whether we already know about a given
				# dependency.
  local $self->{ALL_DEPENDENCIES} = \%all_dependencies;

  my %env_dependencies;		# A hash of dependencies on the environment.
  local $self->{ENV_DEPENDENCIES} = \%env_dependencies;

  my @explicit_dependencies;	# The ones that were listed in the dependency
				# list or explicitly somewhere else in the
				# makefile.
  local $self->{EXPLICIT_DEPENDENCIES} = \@explicit_dependencies;

  my @explicit_targets;		# The ones that were listed and not inferred
				# from the command.

  local $self->{EXPLICIT_TARGETS} = \@explicit_targets;

  my @extra_dependencies;
  local $self->{EXTRA_DEPENDENCIES} = \@extra_dependencies;

  my $makefile = $self->{MAKEFILE};

#
# Get the full list of explicit targets:
#
  my $target_string =
    $makefile->expand_text($self->{TARGET_STRING}, $self->{RULE_SOURCE});


  for( split_on_whitespace $target_string ) {
    my $tinfo = file_info( unquote(), $build_cwd );
    push @explicit_targets, $tinfo;
    $self->add_target( $tinfo );
  }

# This is a good time to expand the dispatch rule option, if any:
  if($self->{DISPATCH}) {
    $self->{DISPATCH} = $makefile->expand_text(
      $self->{DISPATCH}, $self->{RULE_SOURCE}
    );
    $self->{DISPATCH} =~ s/^\s*//;
    $self->{DISPATCH} =~ s/\s*$//;
    delete $self->{DISPATCH} unless $self->{DISPATCH};
  }

#
# Get the full list of explicit dependencies:
#
  my $dependency_string =
    $makefile->expand_text($self->{DEPENDENCY_STRING}, $self->{RULE_SOURCE});
				# Get the list of files.

  for( split_on_whitespace $dependency_string ) {
    push @explicit_dependencies, /[\[\*\?]/ ?		# Is it a wildcard?
      Glob::zglob_fileinfo( unquote(), $build_cwd ) :
      file_info( unquote(), $build_cwd );
  }

#
# Get the explicit environmental dependencies:
#
  if(exists $self->{ENV_DEPENDENCY_STRING}) {
    my $env_dependency_string = $makefile->expand_text(
      $self->{ENV_DEPENDENCY_STRING}, $self->{RULE_SOURCE}
    );
    $self->add_env_dependency( unquote )
      for split_on_whitespace $env_dependency_string;
  }

  push @explicit_dependencies, @extra_dependencies;
				# Extra dependencies go at the end of the
				# list.

  foreach (@explicit_dependencies) {
    $self->add_dependency($_);	# Make sure we know about each dependency.
  }

#
# Now expand the command string.  This must be done last because it
# can depend on both the explicit targets and the explicit dependencies.
#
  my $perl = 0;	      # split-pattern always 2nd in list, even if 1st is empty
  my $command_string = join '', map {
    if (!($perl = !$perl))	# so this one is perl, and next won't be :-)
    {
      $_;
    }
    elsif (@explicit_targets && $explicit_targets[0]{PRIVATE_VARS})
    {
      local $Makefile::private = $explicit_targets[0];
				# Temporarily set up target-specific variables,
				# if there actually are any.
      $makefile->expand_text($_, $self->{RULE_SOURCE});
				# Get the text of the command.
    }				# Turn off the target-specific variables.
    else {
      $makefile->expand_text($_, $self->{RULE_SOURCE});
				# Get the text of the command.
    }
  } split /^((?:[\@-]|noecho\s+|ignore_error\s+)*perl\s*\{(?s:\{.*?\}\})?.*\n?)/m, $self->{COMMAND_STRING};

  $command_string =~ s/^\s+//;	# Strip out leading and trailing whitespace
  $command_string =~ s/\s+$//;	# so we don't trigger unnecessary rebuilds
				# quite as often.

  # In $dont_scan mode (--final-rule-only), don't try to build any
  # dependencies.  We assume that the user knows they're up to date.  The
  # build info won't show then either, so it will look out-of-date next time
  # around.
  if($dont_scan) {
    return (\@explicit_targets, [], $command_string, \%env_dependencies);
  }

  # Try to get the scanner results from the build info, and failing that
  # scan from scratch. (Well, not completely from scratch, because the cached
  # lists of include directives can still be used if they're up-to-date.)
  if(my $msg=$self->load_scaninfo(
      $oinfo, $command_string, \@explicit_dependencies
    )
  ) {
    ::log SCAN_RULE => $oinfo, $msg
      if $::log_level;
    unless(eval { $self->parser->parse_rule($command_string, $self) }) {
      die $@ if $@ && $@ !~ /^SCAN_FAILED\s/;
      $self->{SCAN_FAILED} ||= 1;
    }
				# Look for any additional dependencies (or
				# targets) that we didn't know about.
  }
  else {
    ::log SCAN_CACHED => $oinfo
      if $::log_level;
  }

#
# For some reason, the #@!$@#% linux kernel makefiles have a file that
# depends on itself.  This looks like a simple mistake, but I want this
# to work properly on the linux kernel, so we explicitly remove dependencies
# that are equal to the target.
#
  foreach (values %all_targets) {
    if( $all_dependencies{int()} ) {
      delete $all_dependencies{int()}; # Remove it from the dependency list.
      my $warn_flag = 0;
      for (my $idx = 0; $idx < @explicit_dependencies; ++$idx) {
	if ($explicit_dependencies[$idx] == $_) { # Was it an explicit dependency?
	  splice(@explicit_dependencies, $idx, 1); # Remove it.
	  $warn_flag++ or
	    warn FileInfo::absolute_filename( $_ ) . " depends on itself; circular dependency removed\n";
	}
      }

    }
  }

#
# Make a list of the targets and the dependencies, first listing the explicitly
# specified ones, and then listing the implicit ones later.  It's confusing
# (and breaks some makefiles) if dependencies are built in a different order
# than they are specified.
#
  delete @all_targets{int()} for @explicit_targets;
  delete $all_dependencies{int()} for @explicit_dependencies;

  ([ @explicit_targets, values %all_targets ],
   [ @explicit_dependencies, values %all_dependencies ],
   $command_string, \%env_dependencies);
}

=head2 get_tagname

   $real_tag = $rule->get_tagname($tag)

Returns a unique tag name for $tag.
See L</"load_scaninfo">.

Since a rule can have multiple commands with the same include tag, the
requested tag might not match the actual tag that gets returned.

=cut

sub get_tagname {
  my ($self, $tag) = @_;
  return undef unless defined $tag;
  die if $tag eq '';
  my $real_tag = $tag;
  my $suffix = 0;
  while(exists $self->{META_DEPS}{$real_tag}) {
    $real_tag = $tag . ++$suffix;
  }
  $self->{META_DEPS}{$real_tag}={};
  $real_tag;
}

=head2 add_include_dir

   $rule->add_include_dir($real_tag, $dirname);

Add an include directory for scanner caching.
See L</"load_scaninfo">.

$dirname is a name relative to build_cwd.

=cut

sub add_include_dir {
  my ($self, $tag, $dir, $front) = @_;
  die "unregistered tag $tag" unless $self->{META_DEPS}{$tag};
  die unless $tag;
  $self->{INCLUDE_PATHS}{$tag} ||= [];
  if($front) {
    unshift(@{$self->{INCLUDE_PATHS}{$tag}}, $dir);
  }
  else {
    push(@{$self->{INCLUDE_PATHS}{$tag}}, $dir);
  }
  $self->{INCLUDE_PATHS_REL}{$tag} = 1 unless defined $dir;
}

=head2 add_include_suffix

   $rule->add_include_suffix($real_tag, $suffix);

Add an include suffix list for scanner caching.
See L</"load_scaninfo">.

=cut

sub add_include_suffix_list {
  my( $self, $tag, $sfxs ) = @_;
  die "unregistered tag $tag" unless $self->{META_DEPS}{$tag};
  $self->{INCLUDE_SFXS}{$tag} = $sfxs;
}

=head2 add_meta_dependency

   $rule->add_meta_dependency($real_tag, $src, $name, $finfo);

Add an include file for scanner caching.
Return value is TRUE iff the meta dependency failed to build.
See L</"load_scaninfo">.

$name is the requested name relative to the directory named by $src
(relative to the build directory).
If $real_tag is undef, then it is treated as a path including only ".".
$src is ignored if $name is an absolute path or none of the directories in
the include path are undefined.
If $src is undef, then the build directory is used.

The return value is zero on success (including if the file could not be found),
nonzero on build failure.

=cut

sub fix_file_ {
  my $name = $_[0];
  die unless defined($name) && $name ne '' && $name !~ m@^/+$@;
  $name =~ s@/+$@@;
  $name;
}
sub add_any_dependency_ {
  my ($self, $key, $tag, $src, $name, $finfo) = @_;
  $tag='' unless defined $tag;
  # We won't care where the file was included from if it's not using a tag,
  # if the tag it's using doesn't care about the source directory, or if
  # the file was included with an absolute path.
  $src='' unless defined($src) && $tag ne '' &&
    $self->{INCLUDE_PATHS_REL}{$tag} && $name !~ m@^/@;
  $src=~s@/*$@/@ unless $src eq '';
  die "unregistered tag $tag" unless $tag eq '' || $self->{META_DEPS}{$tag};
  $self->{$key}{$tag}{$src}{fix_file_($name)} = 1;
  if($finfo) {
    $self->add_dependency($finfo);
    return wait_for ::build $finfo if $key eq 'META_DEPS';
  }
  0; # success
}
sub add_any_dependency_if_exists_ {
  my ($self, $key, $tag, $src, $name, $finfo, $tinfo) = @_;

  # TBD: We should return undef if work would have to be done in order
  # to build or re-build $finfo. This would require the equivalent of the
  # GNU make -q option, which we don't have (yet).

  # We need to recover from errors while figuring out where to build the
  # dependency, because we might need to build Makeppfile's while searching,
  # and we might fail to build one that we'll wind up not actually needing.
  local $::keep_going = 1;
  if($finfo && !FileInfo::exists_or_can_be_built_or_remove( $finfo )) {
    return undef;
  }
  ::log CACHED_DEP => $finfo, $tinfo
    if $::log_level && $finfo;
  !&add_any_dependency_;
}
sub add_meta_dependency {
  splice @_, 1, 0, 'META_DEPS';
  &add_any_dependency_;
}

=head2 add_implicit_dependency

   $rule->add_implicit_dependency($real_tag, $src, $name, $finfo);

Add a dependency file for scanner caching.
See L</"load_scaninfo">.

This works just like add_meta_dependency, except that the rule doesn't
have to be re-scanned by virtue of an implicit dependency being out of date.

=cut

sub add_implicit_dependency {
  splice @_, 1, 0, 'IMPLICIT_DEPS';
  &add_any_dependency_;
}

=head2 add_implicit_target

   $rule->add_implicit_target($name);

Add a target file for scanner caching.
See L</"load_scaninfo">.

=cut

sub add_implicit_target {
  my ($self, $name) = @_;
  $self->{IMPLICIT_TARGETS}{fix_file_($name)} = 1;
  my $finfo = file_info($name, $self->build_cwd);
  $self->add_target($finfo);
  $finfo;
}

=head2 add_implicit_env_dependency

   $rule->add_implicit_env_dependency($name);

Add an environmental dependency for scanner caching.
See L</"load_scaninfo">.

=cut

sub add_implicit_env_dependency {
  my ($self, $name) = @_;
  $self->{IMPLICIT_ENV_DEPS}{$name} = 1;
  $self->add_env_dependency($name);
}

=head2 set_signature_method_scanner

   $rule->set_signature_method_scanner($name)

Like set_signature_method_default, except that it takes a class name
instead of an object, and it caches how the signature method was set by a
scanner.

=cut

sub set_signature_method_scanner {
  my ($self, $name) = @_;
  eval "require Signature::$name" or
    die "failed to load class Signature::$name\n";
  my $signature = ${"Signature::${name}::$name"} or
    die "invalid signature class $name\n";
  $self->{SIG_METHOD_NAME} ||= $name;
  $self->set_signature_method_default($signature);
  $signature;
}

=head2 mark_scaninfo_uncacheable

   $rule->mark_scaninfo_uncacheable

Prohibit the rule from caching its scanner information.
This is useful when the information gathered by one of the scanners that
gets involved doesn't fit into the caching scheme, and that could cause
build inaccuracies.

=cut

sub mark_scaninfo_uncacheable {
  $_[0]{SCANINFO_UNCACHEABLE} = 1;
}

=head2 cache_scaninfo

   $rule->cache_scaninfo(\@targets);

Set build_info_string's for scanner caching for each of @targets.
See L</"load_scaninfo">.

Once transferred to the build_info cache, the information is deleted from
$rule, in order to save memory.

=cut

sub clear_scaninfo_ {
  delete @{$_[0]}{qw(SIG_METHOD_NAME INCLUDE_PATHS INCLUDE_PATHS_REL INCLUDE_SFXS
    META_DEPS IMPLICIT_DEPS IMPLICIT_TARGETS IMPLICIT_ENV_DEPS)};
}
sub cache_scaninfo {
  my ($self, $targets) = @_;

  die if exists $self->{SCAN_FAILED};
  unless($::nocache_scaninfo || $self->{SCANINFO_UNCACHEABLE}) {
    # Wipe out unnecessary info from IMPLICIT_DEPS:
    while(my ($tag, $th) = each %{$self->{IMPLICIT_DEPS}}) {
      $tag = $self->{META_DEPS}{$tag};
      while(my ($dir, $dh) = each %$th) {
	$dir = $tag->{$dir};
	for my $name (keys %$dh) {
	  delete $dh->{$name} if exists $dir->{$name};
	}
      }
    }

    # NOTE: Because the build info has to be valid at the next makepp run,
    # it is important that the directory/file paths do *not* have symbolic
    # links resolved, because otherwise there will be no hope of detecting
    # that we need to rebuild because a symbolic link was retargeted. The
    # exception to this rule is that the paths of directories containing the
    # file from which another file was included can be resolved, because the
    # dependency on the file that has the include directive will force a
    # rebuild if its path contains a retargeted symbolic link (and we are
    # able to detect that, which isn't the case yet).
    my @implicits = 'IMPLICIT_TARGETS';
    push @implicits, 'IMPLICIT_ENV_DEPS' if %{$self->{IMPLICIT_ENV_DEPS} || {}};
    for my $tinfo (@$targets) {
      save_build_info_( $self, $tinfo, 'SIG_METHOD_NAME', sub {$_[0] || ''} );
      save_build_info_tag_( $self, $tinfo, 'INCLUDE_PATHS', \&join1_ );
      save_build_info_tag_( $self, $tinfo, 'INCLUDE_SFXS', \&join1_ );
      save_build_info_tag_( $self, $tinfo, 'META_DEPS', \&join_dir_ );
      save_build_info_tag_( $self, $tinfo, 'IMPLICIT_DEPS', \&join_dir_ );
      save_build_info_( $self, $tinfo, $_,
	sub { join1_([sort keys %{$_[0] || {}}]) }
      ) for @implicits;
    }
    &FileInfo::update_build_infos;
  }
  &clear_scaninfo_;
}
sub join1_ {
  join("\01", map { defined($_) ? $_ : '' } @{$_[0]});
}
sub join_dir_ {
  my $hashref = $_[0] || {};
  my @result;
  if($hashref->{''}) {
    push(@result, sort keys %{$hashref->{''}});
  }
  for my $dir (sort keys %$hashref) {
    if( $dir ne '' and my @keys = keys %{$hashref->{$dir}} ) {
      push @result, $dir, sort @keys;
    }
  }
  join("\01", @result);
}
sub save_build_info_ {
  my ($self, $tinfo, $key, $sub) = @_;
  my $value = $self->{$key} if exists $self->{$key};
  FileInfo::set_build_info_string($tinfo,$key, &$sub($value));
}
sub save_build_info_tag_ {
  my ($self, $tinfo, $key, $sub) = @_;
  save_build_info_( $self, $tinfo, $key,
    sub {
      my $hashref = $_[0] || {};
      my @result;
      for(sort keys %$hashref) {
	my $item=&$sub($hashref->{$_});
	push(@result, join1_([$_, $item])) if $item ne '';
      }
      join("\02", @result);
    }
  );
}

=head2 load_scaninfo

   $rule->load_scaninfo($tinfo, $command_string, $explicit_dependencies);

Attempt to get all the scanned information about $rule from $tinfo's build
info file.
If this fails for whatever reason, including the scaninfo possibly being
out of date, then the reason is returned, and $rule is left in its original
state.
This typically means that it must be re-scanned from scratch.
Otherwise, 0 is returned.

=cut

require Scanner;
sub load_scaninfo_single {
  my (
    $self, $tinfo_version, $tinfo, $command_string, $explicit_dependencies
  ) = @_;

  # Fetch the info, or give up
  my( $build_cwd_name, $sig_method, $include_paths, $include_sfxs, $meta_deps,
      $implicit_deps, $implicit_targets, $implicit_env_deps, $command ) =
    FileInfo::build_info_string( $tinfo_version,
				 qw(CWD SIG_METHOD_NAME INCLUDE_PATHS INCLUDE_SFXS
				    META_DEPS IMPLICIT_DEPS IMPLICIT_TARGETS
				    IMPLICIT_ENV_DEPS COMMAND) );
  $include_sfxs ||= '';
  $implicit_env_deps ||= '';

  return 'info not cached' unless defined($build_cwd_name) &&
    defined($include_paths) &&
    defined($meta_deps) && defined($implicit_deps) &&
    defined($implicit_targets);

  # If the CWD changed, then we have to rescan in order to make sure that
  # path names in the scaninfo are relative to the correct directory.
  my $build_cwd = file_info($build_cwd_name, $tinfo->{'..'});
  return 'the build directory changed' unless $build_cwd == $self->build_cwd;

  # Early out for command changed. This is redundant, but it saves a lot of
  # work if it fails, especially because loading the cached dependencies might
  # entail building metadepedencies.
  return 'the build command changed' if $command_string ne $command;

  my $saved_signature_method = $self->{SIGNATURE_METHOD};
  $self->set_signature_method_scanner($sig_method)
    if $sig_method;

  # Trump up a dummy scanner object to help us look for files.
  my $scanner = Scanner->new($self, '.');

  for( split /\02/, $include_paths ) {
    my( $tag, @path ) = split /\01/, $_, -1;
    Scanner::add_include_dir( $scanner, $tag, $_ eq '' ? undef : $_)
      for @path;
  }
  for( split /\02/, $include_sfxs ) {
    my( $tag, @sfxs ) = split /\01/, $_, -1;
    Scanner::add_include_suffix_list( $scanner, $tag, \@sfxs );
  }

  # Replay the implicit environmental dependencies.
  my %saved_explicit_env_deps = %{$self->{ENV_DEPENDENCIES}};
  for(split(/\01/, $implicit_env_deps)) {
    $self->add_implicit_env_dependency($_);
  }

  # Save the existing list of dependencies.
  my %saved_all_dependencies = %{$self->{ALL_DEPENDENCIES}};

  my %meta_dep_finfos; # Not used, but may be used in the future.
  my $cant_build_deps;
  {
    # Update $self to reflect the cached dependencies:
    my $key = 'META_DEPS';
    TAG: for( split( /\02/, $meta_deps ), undef, split( /\02/, $implicit_deps )) {
      unless( defined ) {
	$key = 'IMPLICIT_DEPS';
	next TAG;
      }
      my( $tag, @incs ) = split /\01/, $_, -1;
      die if !defined $tag;
      Scanner::get_tagname( $scanner, $tag) if $tag ne ''; # make it a valid tag

      # TBD: Ideally, we should remember which tags are marked should_find
      # instead of assuming that 'sys' & 'lib' aren't and all others are, but
      # this will generate the correct warnings most of the time, and it's not
      # the end of the world if it doesn't:
      Scanner::should_find( $scanner, $tag ) if $tag ne 'sys' && $tag ne 'lib';

      my $dir = $build_cwd;
      for my $item (@incs) {
	if($item =~ s@/$@@) {
	  $dir = file_info($item, $build_cwd);
	}
	else {
	  my $finfo;
	  if($tag ne '') {
	    $finfo = Scanner::find( $scanner, undef, $tag, $item, $dir);
	    add_any_dependency_if_exists_( $self, $key,
	      $tag, relative_filename($dir,$build_cwd), $item, $finfo, $tinfo
	    ) or $cant_build_deps=1;
	  }
	  else {
	    $finfo = file_info($item, $build_cwd);
	    add_any_dependency_if_exists_( $self, $key,
	      undef, undef, $item, $finfo, $tinfo
	    ) or $cant_build_deps=1;
	  }
	  last TAG if $cant_build_deps;
	  $meta_dep_finfos{$finfo} = 1 if $finfo && $key eq 'META_DEPS';
	}
      }
    }
  }

  # TBD: If you have a explicit dependency in the target list, then it gets
  # removed after this method, but before the dependencies are stored in the
  # build info, and therefore the dependency list will look out of date
  # when it isn't.  This only slows things down, only happens when the
  # makefile is screwy, and we can fix it using $explicit_dependencies.
  delete @{$self->{ALL_DEPENDENCIES}}{keys %{$self->{ALL_TARGETS}}};

  my $flush_scaninfo = $cant_build_deps;
  unless($flush_scaninfo) {
    my $rebuild_needed = $self->build_check_method->build_check(
      $tinfo_version,
      $self->sorted_dependencies([values %{$self->{ALL_DEPENDENCIES}}]),
      $command_string, $build_cwd, $self->signature_method,
      $self->{ENV_DEPENDENCIES}
    );
    $flush_scaninfo ||= $rebuild_needed;
    if($rebuild_needed && $rebuild_needed eq 'DEPENDENCIES') {
      $flush_scaninfo = 0;

      # The target is out of date only with respect to dependencies.  If
      # none of them are meta dependencies, then we don't need to re-scan.
      my @outdated = $self->build_check_method->changed_dependencies(
	$tinfo_version, $self->signature_method, $build_cwd,
	values %{$self->{ALL_DEPENDENCIES}}
      );

      for(@outdated) {
	if($meta_dep_finfos{$_}) {
	  $flush_scaninfo = 1;
	  last;
	}
      }
    }
  }

  if($flush_scaninfo) {
    # Reverse the side effects of this method:
    $self->{SIGNATURE_METHOD} = $saved_signature_method;
    %{$self->{ALL_DEPENDENCIES}} = %saved_all_dependencies;
    %{$self->{ENV_DEPENDENCIES}} = %saved_explicit_env_deps;
    &clear_scaninfo_;

    return $cant_build_deps ? "couldn't build dependencies" :
      'meta dependencies are out of date';
  }

  # Now we know that this is going to succeed, so go ahead and add the implicit
  # targets without fear of having to backtrack.
  for(split(/\01/, $implicit_targets)) {
    $self->add_implicit_target($_);
  }

  0;
}
sub load_scaninfo {
  my $self = shift;		# Modify stack to pass on below
  my ($tinfo) = @_;

  return '--force_rescan option specified' if $::force_rescan;

  ::log SCAN_INFO => $tinfo
    if $::log_level;
  my $first_msg = $self->load_scaninfo_single( $tinfo, @_ );
  return 0 unless $first_msg;
  if( exists $tinfo->{ALTERNATE_VERSIONS} ) {
    for( @{$tinfo->{ALTERNATE_VERSIONS}} ) {
      if( my $msg = $self->load_scaninfo_single( $_, @_ )) {
	::log SCAN_INFO_NOT => $_, $msg
	  if $::log_level;
      } else {
	::log SCAN_INFO_FROM => $_
	  if $::log_level;
	return 0;
      }
    }
  }
  $first_msg;
}

sub sorted_dependencies {
  my ($self, $all_dependencies) = @_;
  my( %dependencies, %long_dependencies, %dups );
  my $build_cwd = $self->build_cwd;
  for( @$all_dependencies ) {
    my $name = $_->{NAME};
    if( $dups{$name} ) {
      $long_dependencies{relative_filename( $_, $build_cwd )} = $_;
    } elsif( $dependencies{$name} ) {
      $dups{$name} = 1;
      $long_dependencies{relative_filename( $dependencies{$name}, $build_cwd )} = $dependencies{$name};
      delete $dependencies{$name};
      $long_dependencies{relative_filename( $_, $build_cwd )} = $_;
    } else {
      $dependencies{$name} = $_;
    }
  }
  [@dependencies{sort keys %dependencies}, @long_dependencies{sort keys %long_dependencies}];
				# Get a sorted list of dependencies.  We need
				# to have these in a predictable order so
				# build info can be quickly compared.  It's
				# important to sort by the filename first
				# rather than the directories, because if we
				# use a repository, sometimes directories
				# change but filenames don't (if absolute
				# directory names are referenced in the build
				# procedure), and we want the system not to
				# force recompilation in this case.
}

=head2 $rule->execute($command_string, $all_targets, $all_dependencies)

   $handle = $rule->execute($command_string, $all_targets, $all_dependencies);

Executes the given command string, and returns a handle for the executing
process (which may not have finished by the time we return).  The command
string may contain several shell commands separated by newlines.

This is part of the Rule class so the default execute routine can be
overridden.  You can replace the execute routine with any complicated
operation you want, provided it returns the appropriate kind of handles.

=cut

sub execute {
  my ($self, $actions, $all_targets, $all_dependencies) = @_;	# Name the arguments.

  # Check for a symlink that doesn't need rebuilding, even though the linkee got built.
  if( @$all_targets == 1 && exists $all_targets->[0]{TEMP_BUILD_INFO} ) {
    if( $actions eq $all_targets->[0]{TEMP_BUILD_INFO}{COMMAND} &&
	$all_targets->[0]{TEMP_BUILD_INFO}{SYMLINK} eq
	  readlink( FileInfo::absolute_filename_nolink $all_targets->[0] ) || '' ) {
      $all_targets->[0]{BUILD_INFO} = delete $all_targets->[0]{TEMP_BUILD_INFO};
      $::n_files_changed--;	# Don't count this one, since we're not rebuilding it.
      ::log SYMLINK_KEEP => $all_targets->[0], $self->source
	if $::log_level;
      return;
    } else {
      delete $all_targets->[0]{TEMP_BUILD_INFO}; # Save some memory
    }
  }


  RecursiveMake::setup_socket() if # Do we need to listen for recursive make?
    defined $RecursiveMake::command && $actions =~ /\brecursive_makepp\b/;

  my $build_cwd = $self->build_cwd;
#
# If we're not building in parallel, then we don't change the standard output
# of the commands.  In this case, if there's a directory change, we print
# it out before executing the commands.
#
  &touched_filesystem;		# In case there are side-effects
  if( $::parallel_make ) {

#
# We're executing in parallel.	We can't print the directory name initially,
# and we have to redirect STDOUT, because we don't want to mix output from
# different commands.
#
# At present, we direct the output to a file, and then print it when we're
# done.
#

    my $tmpfile = "/tmp/makepp.$$" . substr rand, 1;
				# Make the name of a temporary file.
				# Don't use f_mktemp, which is more expensive.
				# TODO when discontinuing support of 5.6:
				# switch to: open my $fh, '+>', undef
				# and dup $fh to STDOUT/ERR
    my $proc_handle = new MakeEvent::Process sub {
#
# Child process.  Redirect our output to the temporary file and then start
# the build.
#
      open STDOUT, '>', $tmpfile or
	die "$::progname: can't redirect standard output to $tmpfile--$!\n";
      open STDERR, '>&STDOUT' or die "can't dup STDOUT\n";
				# Make stderr go to the same place.
      open STDIN, '< /dev/null' or
	die "$::progname: can't redirect standard input to /dev/null--$!\n";
      # Turns out we don't want to close STDIN, because then duping STDOUT
      # causes fd 0 to be opened, rather than 2, so STDERR is actually file
      # handle 0.  This caused a weird bug where error messages were not
      # displayed on parallel builds.
      $self->execute_command($actions, $build_cwd, $all_targets, $all_dependencies); # Execute the action(s).
    };

#
# Gets executed in the parent process when the child has finished.
#
    my $end_handler = sub {
      if( -s $tmpfile ) {	# Is there anything?
	print_build_cwd( $build_cwd ); # Display any directory change.
	local $| = 1;		# Must flush before copy.
	File::Copy::copy( $tmpfile, \*STDOUT );
      }
      unlink $tmpfile;		# Get rid of it.
      $_[0];			# Propagate the status.
    };
    when_done $proc_handle, $end_handler, ERROR => $end_handler;
  } else {			# Not parallel.
    print_build_cwd( $build_cwd ); # Display any directory change.
    wait_for new MakeEvent::Process sub {
      $self->execute_command($actions, $build_cwd, $all_targets, $all_dependencies); # Execute the command.
    };				# Wait for the process, because we don't want
				# to get ahead and start scanning files we
				# can't build yet.  That could mix up output
				# between make and the other process.
  }
}

=head2 $rule->print_command

Print out a command instead of executing it.  This is used only when we
are running with -n (dry run mode).

=cut

sub print_command {
  local $_ = $_[1];		# Access the command string.
  1 while s/^\s*([-\@]|noecho\s+|ignore_error\s+)//m;
				# Strip out any things that control printing,
				# because we always want to print with -n.
  print "$_\n";
  undef;			# No build handle.
}

=head2 $rule->append($rule)

Add the targets and commands from another rule to this rule.  This is used
only when parsing double colon rules like this:

   clean::
       $(RM) *.o

   # Later in the makefile:
   clean::
       $(RM) y.tab.c y.tab.h

=cut

sub append {
    my ($self, $other) = @_;	# Name the arguments.

    $self->{DEPENDENCY_STRING} .= " $other->{DEPENDENCY_STRING}";
    $self->{COMMAND_STRING} .= $other->{COMMAND_STRING};
    $self->{RULE_SOURCE} .= ",$other->{RULE_SOURCE}";
    if(exists $other->{ENV_DEPENDENCY_STRING}) {
      if(exists $self->{ENV_DEPENDENCY_STRING}) {
	$self->{ENV_DEPENDENCY_STRING} .= " $other->{ENV_DEPENDENCY_STRING}";
      }
      else {
	$self->{ENV_DEPENDENCY_STRING} = $other->{ENV_DEPENDENCY_STRING};
      }
    }
}

# Returns the action (2nd arg) split on newline, with prefixes (the stuff that
# doesn't get executed by the sub-shell) split off. The returned list repeatedly
# has the following 4 elements: (flags, perl, &, action, ..., (undef) x 4)
sub split_actions {
  pos( $_[1] ) = 0;
  $_[1] =~ /\G\s*/gc;
  $_[1] =~ /\G((?:[\@-]|noecho\s+|ignore_error\s+)*)(?:(?:make)?perl\s*(\{(?s:\{.*?\}\})?.*)|(&)?(.*))\s*/gcm;
  #		  1							   2			 3   4
}

=head2 $rule->setup_environment()

Sets up the environment for the rule's makefile.

=cut

sub setup_environment {
  my $makefile = $_[0]{MAKEFILE};
  shift;
  $makefile->setup_environment( @_ );
}

use POSIX ();
sub exec_or_die {
  my( $action, $silent ) = @_;
  my $result = 254 << 8;
  if( $silent || !$::profile ) {
    { exec format_exec_args $action }
				# Then execute it and don't return.
				# This discards the extra make process (and
				# probably saves lots of memory).
    print STDERR "exec $action failed--$!\n"; # Should never get here, braces eliminate warning.
  } else {
    $result = system format_exec_args $action;
    print STDERR "system $action failed--$!\n"
      if $result == -1;
    ::print_profile_end $action if $::profile;
  }
  close $_ for @::close_fhs;
  ::suicide ::signame $result & 0x7F
    if $result & 0x7F;
  POSIX::_exit $result ? ($result>>8 || 1) : 0;
}

#
# The following internal subroutine executes each action.  Arguments:
# a) The directory that we're supposed to execute this from.
# b) The lines of actions to execute.
# c) A hash of variables that must be exported to the environment, and their
#    values.
# d) The environment to use (not including the exports).
#
# At this point, STDOUT and STDERR should be set up to wherever the command
# is supposed to output to.
#
my $true = (-x '/bin/true') ? '/bin/true' : '/usr/bin/true';
sub execute_command {
  my( $self, undef, $build_cwd, $all_targets, $all_dependencies ) = @_; # Name the arguments.

  # On Windows, native actions don't fork, and so we have to continue to
  # defer signals so that we can reliably mark failed targets as such --
  # even though this is dangerous because there is no way to stop infinite
  # loops!  For system commands, we unblock 'INT' and 'QUIT' so that the
  # action will be stoppable, and we'll still be able to mark them because
  # while the system is running they are blocked by the makepp process
  # automatically.
  &::reset_signal_handlers if !::is_windows; # unless constant gives a warning in some variants of 5.6
  chdir( $build_cwd );		# Move to the correct directory.
  $self->{MAKEFILE}->setup_environment;

  $RecursiveMake::socket_name and	# Pass info about recursive make.
    $ENV{MAKEPP_SOCKET} = $RecursiveMake::socket_name;

  $::preexecute_rule_hook and &$::preexecute_rule_hook($self);

#
# Now execute each action.  We exec the action if it is the last in the series
# and we don't need to ignore the return code; otherwise, we call system()
# instead.  This means we start an extra process in those cases, but it's too
# tricky to avoid it in such comparatively rare situations.
#
  my( $flags, $perl, $command, $action, @actions ) = &split_actions; # Get the commands to execute with flags.
  $#actions -= 4;		# Eliminate bogus undefs.

  local $unsafe;
  my $maybe_open;
  while( 1 ) {
    next unless
      defined $action && $action =~ /\S/ || defined $perl && $perl =~ /\S/;
    if( $unsafe ) {
      $build_cwd = absolute_filename $build_cwd if ref $build_cwd;
      CORE::chdir $build_cwd;
      undef $unsafe;
      $maybe_open = 1;
    }

#
# Parse the @ and - in front of the command.
#
    my $silent_flag = $::silent_execution || $flags =~ /\@|noecho/;
    my $error_abort = $flags !~ /-|ignore_error/;

    if( defined $perl or defined $command ) {
      $File::chdir = 1;		# External command might change our dir.
      ::print_profile( defined $perl ? "perl $perl" : "&$action" ) unless $silent_flag;
				# This prints out makeperl as perl.  That is correct,
				# because it has already been expanded to plain perl code.
      local $Makesubs::rule = $self;
      local $self->{EXPLICIT_TARGETS} = $all_targets;
      local $self->{EXPLICIT_DEPENDENCIES} = $all_dependencies;
      if( defined $perl ) {
	$unsafe = 1;
	# NOTE: $self->{RULE_SOURCE} is the line at which the rule started,
	# which is not the same as the line at which the perl action started.
	# To get the real line number, you have to add the number of lines
	# into the rule in which the perl actions appears.  Although this is
	# misleading, it's better than nothing because it usually gets you
	# very close.  It's hard to fix.
	eval { Makesubs::eval_or_die $perl, $self->{MAKEFILE}, $self->{RULE_SOURCE} };
	if( $@ ) {
	  ::print_error( 'perl action: '.$@ );
	  return 1 if $error_abort;
	}
      } else {
	my $comment = index_ignoring_quotes $action, '#';
	my( $cmd, @args ) =
	  unquote_split_on_whitespace $comment > -1 ? substr $action, 0, $comment : $action;
	eval {
	  if( defined &{$self->{MAKEFILE}{PACKAGE} . "::c_$cmd"} ) { # Function from makefile?
	    $unsafe = 1;
	    local $0 = $cmd;
	    &{$self->{MAKEFILE}{PACKAGE} . "::c_$0"}( @args );
	  } elsif( defined &{"Makecmds::c_$cmd"} ) { # Builtin Function?
	    local $0 = $cmd;
	    &{"Makecmds::c_$0"}( @args );
	  } else {
	    $unsafe = 1;
	    Makesubs::run( $cmd, @args );
	  }
	};
	if( $@ ) {
	  for( my $error = $@ ) { # Make a modifiable copy.
	    s/\(eval \d+\)(?:\[.*?\])? line \d+/\`$self->{RULE_SOURCE}\'/g;
	    s/^$cmd: //;
	    print STDERR "makepp: &$cmd: $_";
	  }
	  &::flush_log;
	  return 1 if $error_abort;
	}
      }
      ::print_profile_end( defined $perl ? "perl $perl" : "&$action" ) if $::profile && !$silent_flag;
      next;			# Process the next action.
    }

    if($self->{DISPATCH}) {
      $action =~ s/\'/\'\\\'\'/g;
      $action = $self->{DISPATCH} . " sh -c '$action'";
    }
    ::print_profile( $action ) unless $silent_flag;
    if( ::is_windows ) {	# Can't fork / exec on windows.
      {
	# NOTE: In Cygwin, TERM and HUP are automatically unblocked in the
	# system process, but INT and QUIT are not -- they are just ignored
	# in the calling process.  There is a race here where an INT or
	# QUIT can sneak in, but it's quite unlikely.
	local @SIG{'INT', 'QUIT'} = ( 'DEFAULT' ) x 2;
	system format_exec_args $action;
      }
      $error_abort and $? and return ($?/256) || 1; # Quit if an error.
      ::print_profile_end( $action ) if $::profile && !$silent_flag;
      next;			# Process the next action.
    }

    undef $unsafe;		# Because exec() closes and fork() flushes
                                # filehandles, and external cmd can't chdir.
    my $nop = \&::is_windows;	# Reuse any old function that does nothing, to
				# save allocating a new sub.
    if ($error_abort && !@actions) { # Is this the last action?
      $SIG{__WARN__} = $nop;	# Suppress annoying warning message here if the
				# exec fails.
      exec_or_die $action, $silent_flag;
				# Then execute it and don't return.
				# This discards the extra make process (and
				# probably saves lots of memory).
    } else {			# We have more to do after the process
				# finishes, so we need to keep running
				# after the process starts.
#
# We used to do with with system(), but system has two serious drawbacks:
# 1) A nasty message is printed if the first word is not a valid shell
#    command, and the perl process aborts, apparently without setting the
#    correct exit code.
# 2) On linux, system() blocks signals, which means that makepp wouldn't
#    respond to ^C or other things like that.  (See the man page.)
#
      &::flush_log if ::is_perl_5_6;
      my $pid = fork;
      unless ($pid) {		# Executed in child process:
	$SIG{__WARN__} = $nop;	# Suppress annoying warning message here if
				# the exec fails.
	exec_or_die $action, $silent_flag;
      }

      $pid == -1 and die "fork failed--$!\n";
      wait;			# Wait for it to finish.
      $error_abort && $? and exit( int( $? / 256 ) || 255 );
				# Quit on an error.  $status/256 is the exit
				# status of the process, but if the system()
				# call failed for some reason, that may be 0.
    }
  } continue {
    last unless @actions;
    ($flags, $perl, $command, $action) = splice @actions, 0, 4;
  }

  ::is_windows > 0 and return 0; # If we didn't fork, we'll always get here.

  # Close filehandles that may have been left opened by a perl command.
  if( $maybe_open || $unsafe ) {
    if( ::is_perl_5_6 ) {
      close $_ for @::close_fhs, values %{$self->{MAKEFILE}{PACKAGE}.'::'};
    }
    exec $true;			# Close open filehandles w/o doing garbage collection
    warn "failed to exec `$true'--$!";
    # If that didn't work, then do a close on every symbol in the makefile's
    # package. This is usually good enough, but not if the makefile used an
    # IO handle in another package.
    close $_ for values %{$self->{MAKEFILE}{PACKAGE}.'::'};
  }

  close $_ for @::close_fhs;
  POSIX::_exit 0;		# If we get here, it means that the last
				# command in the series was executed with
				# ignore_error.
}

#
# This subroutine prints out a message if we've changed directories.  This
# message is important for programs like emacs that look through the compiler
# output--it helps them find the directory that the file containing the errors
# is located in.
#
our $last_build_cwd = 0;	# Comparable to a ref.
sub print_build_cwd {
  my $build_cwd = $_[0];
  if( $last_build_cwd != $build_cwd ) { # Different from previous or no previous?
    print "$::progname: Leaving directory `$last_build_cwd->{FULLNAME}'\n"
      if $last_build_cwd;	# Don't let the directory stack fill up.
    print "$::progname: Entering directory `" . &absolute_filename . "'\n";
    $last_build_cwd = $build_cwd;
  }
}

=head2 build_check_method

  $rule->build_check_method;

Gets the build check method for this particular rule.

=cut

sub build_check_method { $_[0]{BUILD_CHECK_METHOD} || $::default_build_check_method }

=head2 set_build_check_method

  $rule->set_build_check_method(method);

Sets the build check method to be the given method.

=cut

sub set_build_check_method {
  $_[0]{BUILD_CHECK_METHOD} = $_[1];
}


=head2 set_build_check_method_default

  $rule->set_build_check_method_default(method);

Sets the build check method, unless it has already been set.

=cut

sub set_build_check_method_default {
  $_[0]{BUILD_CHECK_METHOD} ||= $_[1];
}

=head2 $rule->signature_method

  my $sig_method = $rule->signature_method;

Returns the signature method to use to calculate signatures for files.

The default method is the file time + file size (see Signature.pm for
details).

=cut

sub signature_method {
  $_[0]{SIGNATURE_METHOD} ||
    $::default_signature; # Use the default method.
}

=head2 set_signature_method

  $rule->set_signature_method($sigmethod);

Sets the signature method to use for this particular rule.

=cut

sub set_signature_method {
  $_[0]{SIGNATURE_METHOD} = $_[1];
}

#
# This subroutine sets the signature method if one hasn't already been
# specified.
#
sub set_signature_method_default {
  $_[0]{SIGNATURE_METHOD} ||= $_[1];
}

=head2 $rule->scan_action

  $rule->scan_action($action_string);

Scans the given actions, looking for additional dependencies (such as include
files) or targets (such as .libs/xyz.lo) that weren't explicitly listed.  Make
variables in the action string have already been expanded.

Calls $rule->add_dependency for new dependencies, and $rule->add_target for
new targets.

=cut

sub scan_action { $_[0]->parser->parse_rule($_[1], $_[0]) }

=head2 $rule->parser

  $rule->parser()

Returns the rule scanner object, creating it if it doesn't already exist.

=cut

sub parser {
  my $self = $_[0];
  my $parser_obj = $self->{PARSER_OBJ}; # Is the scanner already cached?
  unless($parser_obj) {
    my $scanner = $self->{ACTION_SCANNER}; # Was one explicitly set?
    $parser_obj = $scanner ?
      &$scanner() : # Yes: generate it
      new ActionParser; # No: Use the default
    $self->{PARSER_OBJ}=$parser_obj;
  }
  $parser_obj;
}

=head2 $rule->source

  $source_string = $rule->source;

Returns a description of where this rule was encountered, suitable for error
messages.

=cut

sub source { $_[0]{RULE_SOURCE} }
*name = \&source;		# Alias for ::log

#
# This subroutine is called to add a possibly new dependency to the list of
# all dependencies for the current rule.
#
# Arguments:
#	$rule->add_dependency($object_info);
#
# This should only be called from subroutines which are called by
# find_all_targets_dependencies.  This basically means it should only be
# called by the command scanners.
#
sub add_dependency {
  $_[0]{ALL_DEPENDENCIES}{int $_[1]} ||= $_[1];
				# Store it if we didn't already know about it.
}

# This subroutine is called to add a possibly new environmental dependency,
# and to store the value as if it were rebuilt in the rule.
#
#	$rule->add_env_dependency($var_name);
#
# $var_name can also be "$name in $var", in which case we split $var on /:/
# and look for a file called $name in each directory, taking the first
# directory name we find as the "current value".
#
sub add_env_dependency {
  my ($rule, $var_name) = @_;
  return if $rule->{ENV_DEPENDENCIES}{$var_name};
  if( my( $name, $val ) = $var_name =~ /^(.+) in (\S+)$/) {
    for( split /:/, $rule->{MAKEFILE}->get_env( $val ) || '' ) {
      next unless $_;
      return $rule->{ENV_DEPENDENCIES}{$var_name} = $_
	if FileInfo::exists_or_can_be_built
	  file_info $name, file_info $_, $rule->{MAKEFILE}{CWD};
    }
    return $rule->{ENV_DEPENDENCIES}{$var_name} = '';
  }
  my $val = $rule->{MAKEFILE}->get_env($var_name);
  $val = 'defined' if defined($val) && $val eq '';
  $val = '' unless defined($val);
  $rule->{ENV_DEPENDENCIES}{$var_name} = $val;
}

sub expand_additional_deps {
  my ($self, $oinfo) = @_;
  if ($oinfo->{ADDITIONAL_DEPENDENCIES}) { # Any additional explicit dependencies?
    my @dep_infos;
    foreach (@{$oinfo->{ADDITIONAL_DEPENDENCIES}}) {
      my ($dep_str, $makefile, $makefile_line) = @$_;
				# Name the components of the stored info.
      for( split_on_whitespace $makefile->expand_text( $dep_str, $makefile_line ) ) {
	push @dep_infos, /[\[\*\?]/ ?		# Is it a wildcard?
	  Glob::zglob_fileinfo( unquote(), $makefile->{CWD} ) :
				# Get which files this is referring to.
	  file_info( unquote(), $makefile->{CWD} );
      }
    }
    return @dep_infos;
  }
  ();
}

#
# This subroutine is called to add a possibly new target to the list of targets
# for the current rule.
#
# Arguments:
#     $rule->add_target($object_info);
#
# This should only be called from subroutines which are called by
# find_all_targets_dependencies.  This basically means it should only be
# called by the command scanners.
#
sub add_target {
  my ($self, $oinfo) = @_;

  return if $self->{ALL_TARGETS}{int $oinfo}; # Quit if we already knew about this.

#
# This target may have dependencies which were explicitly listed in some other
# part of the makefile, as used to be common for specifying include files.
# For example, we might have a rule like:
#
#   %.o: %.c
#     compilation commnad
#
#   xyz.o : abc.h def.h ghi.h
#
  my @dep_infos = $self->expand_additional_deps($oinfo);
  for( @dep_infos ) { $self->add_dependency($_) }
				# Add them to the dependency list.
  push @{$self->{EXTRA_DEPENDENCIES}}, @dep_infos;

  $self->{ALL_TARGETS}{int $oinfo} = $oinfo;
}



###############################################################################
#
# This is a replacement for the Rule when there is no actual rule specified
# in the file.	Most of the subroutines in this package are dummies to
# replace the functionality in the Rule class.
#
package DefaultRule;

#
# Establish a default rule for a new file:
#
sub new { bless { TARGET => $_[1] }, $_[0] }
				# Make the rule object.	 Declare it local
				# so it's accessible to any subroutines that
				# Makefile::expand_text calls. (TODO: What is local here?)

sub build_cwd { $FileInfo::CWD_INFO }

*expand_additional_deps = \&Rule::expand_additional_deps;

sub find_all_targets_dependencies {
  my $self = $_[0];
  my $target = $self->{TARGET};	# Get the target object info.
  if ($target->{ADDITIONAL_DEPENDENCIES}) {
				# Are there dependencies for this file even
				# though there's no rule?
				# We have to expand the dependency list now.

    my @addl_deps = $self->expand_additional_deps($target);
    return ([$target], \@addl_deps, '');
  }

  ([ $target ], [], '', {});
}

#
# If we ever get to this subroutine, it means there's no way to build this
# file.
#
sub execute {
  MakeEvent::when_done sub {
    my $target = $_[0];
    if( $target->{ADDITIONAL_DEPENDENCIES} ) {
				# If it has additional dependencies, then
				# there was a rule, or at least we have to
				# pretend there was.
      #FileInfo::file_exists( $target ) and return 0;
				# If the file doesn't exist yet, we may
				# have to link it from a repository.
      # ::build handles linking targets in from a repository after the rule
      # runs, so we don't have to do it here even if there are
      # ALTERNATE_VERSIONS.
      return 0;			# Return success even if the file doesn't exist.
				# This is to handle things like
				# FORCE:
				# which are really just dummy phony targets.
    }
    ::print_error( 'No rule to make ', $target );
    -1;				# Return nonzero to indicate error.
  }, [$_[0]{TARGET}];
}

#
# This gets called whenever we access a file for which there is no rule
# when we are running with -n.	In this case we don't want to do anything.
#
sub print_command {
  my $target = $_[0]{TARGET};
  if ($target->{ADDITIONAL_DEPENDENCIES}) {
    return 0;			# Handle case like
				# all: xyz
				# where there is no rule for all.
  }
  ::print_error( 'No rule to make ', $target );

  -1;				# Return nonzero to indicate error.
}

#
# The signature method that we use causes the file to be remade if it doesn't
# exist, and leaves it alone if it does.
#
sub signature_method { $Signature::signature }

sub build_check_method { $DefaultRule::BuildCheck::build_check_method_object }

sub build_cache { undef }	# Build cache disabled for objects with
				# no actions.  Reused below where we also need a sub
				# that returns undef.

sub source { 'default rule' }
*name = \&source;		# Alias for ::log

*sorted_dependencies = \&Rule::sorted_dependencies;

# If there is no action, then there is nothing to scan, so presumably nothing
# was affected by scanning.  Reuse any nop function.
*cache_scaninfo = \&TextSubs::CONST0;



package DefaultRule::BuildCheck;

our @ISA = 'BuildCheck';

#
# This package implements signature checking for files for which there is no
# rule.	 We "rebuild" only if the file does not exist.	This will cause the
# file to be linked in from a repository if we can find it somewhere.
#

$DefaultRule::BuildCheck::build_check_method_object = bless \@ISA;
				# Make a singleton object that defines our
				# special build_check method.

sub build_check {
  my $tinfo = $_[1];		# Ignore self

  warn FileInfo::absolute_filename( $tinfo ) . " is a stale file generated by makepp,\n" .
    "but there is no rule to build it any more.	 Makepp assumes that it\n" .
    "is now a source file.  If this is correct, then you should add it\n" .
    "to your source control system (if any), and touch the file to make\n" .
    "this warning go away.  Otherwise, you should remove the file,\n" .
    "because the targets that depend on it will not be reproducible.\n"
      if FileInfo::is_stale( $tinfo );

  # It would be faster to check for existence first, but that doesn't work,
  # because then if the file were from a repository but the repository version
  # changed, then the file would originally exist, and then it gets deleted
  # by build_info_string(), which returns undef, making it look as though the
  # file is up-to-date, when in fact it doesn't even exist!
  defined( FileInfo::build_info_string( $tinfo, 'FROM_REPOSITORY' )) ||
    !FileInfo::file_exists( $tinfo ) ||
    (
     # If it used to be a generated file, but now we need to get it from a
     # repository, then it needs to be linked in, unless we're treating old
     # generated files as source.
     $::rm_stale_files &&
     !$tinfo->{ADDITIONAL_DEPENDENCIES} &&
     defined( FileInfo::build_info_string( $tinfo, 'BUILD_SIGNATURE' ))
    );
}

#
# This subroutine is called when we're deciding whether to import the file
# from a repository.  (It can be called when deciding whether to use a build
# cache, too, except that we've disabled build caches by not providing a
# build_cache_key function.)
#
*build_check_from_build_info = \&DefaultRule::build_cache;
				# If a file exists in a repository, there's
				# no reason not to import it since there's
				# no build rule for this target.

1;
