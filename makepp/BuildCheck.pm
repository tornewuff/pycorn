# $Id: BuildCheck.pm,v 1.7 2006/06/28 19:01:53 pfeiffer Exp $
package BuildCheck;

=head1 NAME

BuildCheck -- Interface definition for various signature classes

=head1 USAGE

Derive a package from this package.

=head1 DESCRIPTION

The BuildCheck package specifies how makepp uses the file signatures and other
information in order to compute whether a file needs to be rebuilt or not.
Each rule can have a different build check associated with it, if necessary.
In the makefile, the signature class is specified by using the :build_check
modifier, like this:

   %.o : %.c
	   : build_check special_build
	   $(CC) $(CFLAGS) -c $(FIRST_DEPENDENCY) -o $(TARGET)

This causes the signature class C<BuildCheck::special_build> to be used for
this particular rule.

Only one object from each different buildcheck class is actually created; the
object has no data, and its only purpose is to contain a blessed reference to
the package that actually implements the functions.  Each rule contains a
reference to the build check object that is appropriate for it.  The object is
found by the name of the build check class.  For example, the above rule uses
the object referenced by C<$BuildCeck::special_build::special_build>.  (The
purpose of this naming scheme is to make it impossible to inherit accidently a
singleton object, which would cause the wrong Signature class to be used.)

The build check class is independent of the signature class.  In general, any
signature method may be used with any build check.  (An exception is the
target_newer build check class, which by definition works only with file
dates.)

=head2 build_check

  if ($build_check_method->build_check($tinfo, \@all_dependencies,
             $build_command, $build_cwd, $signature_method, \%env)) {
    ...
  }

Returns undef if the given target does not need rebuilding, or the true
if a rebuild is needed.
If the I<only> reason a rebuild is needed is that dependencies are changed,
then the special value "DEPENDENCIES" should be returned, as this permits
optimizations related to scanning.

$tinfo is the FileInfo structure for one of the targets that this rule
can make.  build_check should be called for each target.

@all_dependencies is an array that contains all dependencies (including ones
that we detected automatically, such as include files).  The dependencies
should already have been built.  The list should be sorted in the final
order we'll put them into the build information, and duplicates should be
removed.

$build_command is the command string that we are going to execute (after
all variables have been expanded) if any files are out of date.

$build_cwd is the directory that the command should be executed in if it
needs to be executed.

$signature_method is a reference to the Signature class object that should be
used to compute signatures.  See F<Signature.pm> for details.

%env is a hash mapping names of environment variables which affect the
target to values.  If the name is of the form /(.+) in (\S+)/, then $1 is
taken as a filename to search for in the value of the environment variable
named by $2 (directories separated by colons), and the value is the name of
the first directory in which it is found.

The build_check subroutine should pay attention to the ASSUME_CHANGED and
ASSUME_UNCHANGED members of the FileInfo structure.  Implemented by classes
which inherit from this.

=cut

=head2 build_check_from_build_info($build_info_hash, \@all_dependencies,
                                   $build_command, $build_cwd,
                                   $signature_method, \%env)

  if ($build_check_method->build_check_from_build_info($bc_entry,
        \@all_dependencies, $build_command, $build_cwd,
        $signature_method, \%env)) {
    ...
  }

This subroutine does exactly the same thing as C<build_check()> except that it
accepts either a repository entry or a build cache entry.  It may only look at
the build info (the stuff that's stored in the .makepp file) to see if the
file can be used.  Since the $bc_entry argument may be either a build cache
entry or a fileinfo describing a file in the repository, you should access the
build info by calling
C<<$bc_entry->build_info_string("BUILD_INFO_ENTRY_NAME")>>.  You should not call
any other methods of the object.  Implemented by classes which inherit from this.

=cut

=head2 changed_dependencies

   @changed_files = $sigobj->
      changed_dependencies($tinfo, $signature_method, $build_cwd,
                           @all_dependencies);

Returns a list of the dependencies which have been changed since the last time
the target was updated.  "Changed" means changed so that the signature method
cares.  For example, if your signature method simply compares the dates to see
if any of the dependencies are newer (the C<target_newer> method), then this
is a list of dependencies whose file date is newer than the target.

This is only used to avoid rescanning for include files unnecessarily, and
also for the $(changed_inputs) variable.  It is not used for the decision of
whether to build a file.

=cut

sub changed_dependencies {
  return @_[3 .. $#_];               # You'll want to override this....
}

=head2 build_cache_key

   $build_cache_key = $self->build_cache_key($tinfo, \@all_dependencies,
                                             $build_command, $build_cwd,
                                             $signature_method, \%env);

Returns a key for looking up this target in the build cache.  The arguments
are the same as for C<build_check>.

If undef is returned, then we do not look at the file in the build cache.
This is the default behavior; you must override it if you want to enable the
build cache.

NOTE: Build cache key equality must imply substitutability (barring a
vanishingly improbable event such as an accidental MD5 alias), because we
don't do a build_check after getting a cache hit.  In addition to improving
efficiency, this is necessary to combat the fact that it is impossible to
guarantee that a build cache target and its build info actually go together
when the build cache is on an NFS volume.

=cut

sub build_cache_key {
  return undef;                 # Disables the build cache.
}

=head2 update_dep_sigs

    $self->update_dep_sigs($tinfo, $rule);

If build_cache_key returns a defined value, then this is called after
importing from the build cache to make the DEP_SIGS in the target's build info
match the signatures of its dependencies (rather than that of the dependencies
in the build cache), so that its build check will pass on the next makepp run
unless something has actually changed.  Implemented by classes which inherit
from this.

=cut

1;
