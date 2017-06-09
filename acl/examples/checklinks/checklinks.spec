Summary: A program to check for broken links in HTML documents.
Name: checklinks
Version: %{version}
Release: %{release}
License: Commercial
Group: Applications/Internet
#URL: 
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

# Don't attempt to make the debuginfo package
%define debug_package %{nil}

%description
A program to check for broken links in HTML documents.  It recursively
descends into locally referenced file documents, too.  It keeps a table
of visited documents, so it won't get into a loop.


%prep
%setup -q

%build
LISP="%{mlisp}" make

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT{/usr/bin,/usr/lib/checklinks}
cp -rp checklinks/* $RPM_BUILD_ROOT/usr/lib/checklinks
ln -s /usr/lib/checklinks/checklinks $RPM_BUILD_ROOT/usr/bin/checklinks

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
/usr/lib/checklinks/*
/usr/bin/checklinks
%doc


%changelog
* Tue Jun 20 2006 Ahmon Dancy <dancy@dancy> - 
- Initial build.

