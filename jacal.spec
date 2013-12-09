%define name jacal
%define version 1b7
%define release 1

Name:         %{name}
Release:      %{release}
Version:      %{version}
Packager:     Aubrey Jaffer <agj @ alum.mit.edu>

License:      GPL
Vendor:       Aubrey Jaffer <agj @ alum.mit.edu>
Group:        Sciences/Mathematics
Provides:     jacal
BuildArch:    noarch
Requires:     slib

Summary:      The JACAL Symbolic Math System
Source:       ftp://swiss.csail.mit.edu/pub/scm/jacal%{version}.zip
URL:          http://swiss.csail.mit.edu/~jaffer/JACAL.html
BuildRoot:    %{_tmppath}/%{name}%{version}
Prefix:       %{_prefix}

%description
JACAL is an interactive symbolic mathematics program. JACAL can
manipulate and simplify equations, scalars, vectors, and matrices of
single and multiple valued algebraic expressions containing numbers,
variables, radicals, and algebraic differential, and holonomic
functions.

#%define __os_install_post /usr/lib/rpm/brp-compress

%prep
%setup -n jacal -c -T
cd ..
unzip ${RPM_SOURCE_DIR}/jacal%{version}.zip

%build
gzip -f jacal.info

%install
mkdir -p ${RPM_BUILD_ROOT}%{_infodir}/
cp jacal.info.gz ${RPM_BUILD_ROOT}%{_infodir}/

make	prefix=${RPM_BUILD_ROOT}%{prefix}/ \
	mandir=${RPM_BUILD_ROOT}%{_mandir}/ \
	infodir=${RPM_BUILD_ROOT}%{_infodir}/ \
	pinstall

echo "(use-modules (ice-9 slib))">${RPM_BUILD_ROOT}%{prefix}/lib/jacal/guile.scm
echo "(slib:load \"%{prefix}/lib/jacal/math\")">>${RPM_BUILD_ROOT}%{prefix}/lib/jacal/guile.scm
echo "(math)"			>>${RPM_BUILD_ROOT}%{prefix}/lib/jacal/guile.scm
echo "(slib:load \"%{prefix}/lib/jacal/math\")" >${RPM_BUILD_ROOT}%{prefix}/lib/jacal/go.scm
echo "(math)"			>>${RPM_BUILD_ROOT}%{prefix}/lib/jacal/go.scm
mkdir -p ${RPM_BUILD_ROOT}%{prefix}/bin
echo '#! /bin/sh'		> ${RPM_BUILD_ROOT}%{prefix}/bin/jacal
echo JACALDIR=%{prefix}/lib/jacal/>> ${RPM_BUILD_ROOT}%{prefix}/bin/jacal
echo VERSION=%{version}		>> ${RPM_BUILD_ROOT}%{prefix}/bin/jacal
cat jacal.sh 			>> ${RPM_BUILD_ROOT}%{prefix}/bin/jacal
chmod +x ${RPM_BUILD_ROOT}%{prefix}/bin/jacal

%clean
rm -rf $RPM_BUILD_ROOT

%post
/sbin/install-info %{_infodir}/jacal.info.* %{_infodir}/dir

%files
%defattr(-, root, root)
%{prefix}/bin/jacal
%dir %{prefix}/lib/jacal
%{prefix}/lib/jacal/*.scm
%{prefix}/lib/jacal/jacalcat
%{prefix}/lib/jacal/COPYING
%{prefix}/lib/jacal/HELP
%{prefix}/lib/jacal/Makefile
%{_mandir}/man1/jacal.1.*
%{_infodir}/jacal.info.*

%doc ANNOUNCE ChangeLog README COPYING demo test.math rw.math DOC/algdenom DOC/grammar DOC/history DOC/lambda DOC/ratint.pdf


%changelog
* Thu Aug 30 2001     Bo Forslund  <bo.forslund @ abc.se>
- Make more use of macros.
- Install COPYING and HELP file.
