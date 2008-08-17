Summary: compatible, more powerful reimplementation of make
Name: makepp
Version: 1.19
Release: 1
License: GPL
Group: Development/Tools
Packager: Gary Holt <holt-makepp@gholt.net>
URL: http://makepp.sourceforge.net
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-buildroot

%description
makepp is a syntax-compatible reimplementation of make designed for more
reliable builds and simpler makefiles. It scans automatically for include
files, handles builds involving multiple directories in a much simpler and
more reliable fashion, supports transparently putting object files in a
different directory, can automatically import files from a central
repository, won't rebuild if only comments and indentation have changed,
knows to rebuild if the build commands change, has wildcards which match
files which don't exist yet but can be built, and can be extended by
embedding Perl code in the makefile.

%prep
%setup -q

%build
./configure --prefix=$RPM_BUILD_ROOT/usr --htmldir=none

%install
rm -rf $RPM_BUILD_ROOT
make install
cd $RPM_BUILD_ROOT/usr/bin
perl -i~ -pe "s@$RPM_BUILD_ROOT/usr@/usr@; s@none/index\.html@http://makepp.sourceforge.net/1.19@" makepp
rm makepp~

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
/usr/bin
/usr/man
/usr/share

%changelog
* Fri Jul 25 2003 Gary Holt <holt@gholt.net> 
- Initial build.


