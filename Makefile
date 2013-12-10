# Makefile for JACAL: Symbolic Mathematics System.
# Copyright (C) 1990 - 2005, 2010 Aubrey Jaffer.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# These are normally set in "config.status"; defaults are here so that
# "make" won't complain about target redefinitions.
snapdir=$(HOME)/pub/
infodir=$(HOME)/info/
htmldir=$(HOME)/public_html/

SHELL = /bin/sh
INSTALL = install
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_INFO = ginstall-info

CHPAT = $(HOME)/bin/chpat
MAKEDEV = $(MAKE) -f $(HOME)/makefile.dev
TEXI2HTML = /usr/local/bin/texi2html -split -verbose
TEXI2PDF = texi2pdf
RSYNC = rsync -av
Uploadee = csail

intro:	config.status
	@echo
	@echo "Welcome to JACAL.  To install, unpack this directory"
	@echo "where it will eventually reside.  You will also need"
	@echo "to install SLIB and have a Scheme Implementation"
	@echo "(both of which are available from the same vendor or"
	@echo "site as JACAL)."
	@echo
	@echo "Read \"README\" and \"jacal.info\" (or \"jacal.texi\")"
	@echo "to learn how to run and use JACAL."
	@echo

VERSION = 1c3
RELEASE = 1

# ./configure --distdir=${HOME}/dist/ --snapdir=${HOME}/pub/ --docdir=${HOME}/public_html/

config.status:
	./configure
Makefile: config.status
include config.status

prevdocsdir = prevdocs/
jacallibdir = $(libdir)jacal/
S48IMAGE = $(jacallibdir)scheme48.image
windistdir = /c/Voluntocracy/dist/
rpm_prefix = $(HOME)/rpmbuild/

cfiles = math.scm modeinit.scm debug.scm view.scm toploads.scm
sfiles = types.scm func.scm poly.scm elim.scm vect.scm ext.scm		\
	norm.scm sqfree.scm hist.scm sexp.scm grammar.scm unparse.scm	\
	builtin.scm info.scm tensor.scm combin.scm ff.scm factors.scm	\
	uv-hensel.scm hensel.scm interpolate.scm decompose.scm
gfiles = English.scm
mfiles = ANNOUNCE COPYING HELP configure Makefile jacalcat jacal.texi	\
	fdl.texi jacal.1 demo rw.math jacal.spec jacal.sh elk.scm	\
	jacal.nsi
tfiles = test.math interp_test.scm
allfiles = README ChangeLog $(mfiles) $(sfiles) $(cfiles) $(gfiles)	\
	$(tfiles) jacal.info jacal.doc version.txi
#dfiles Document internals of Jacal.
dfiles = algdenom grammar history lambda ratint.tex eqalign.sty
# Common Lisp not currently supported.
#lfiles = scl.lisp math.lisp compilem.lisp
libfiles = $(sfiles) $(cfiles) $(gfiles) jacalcat Makefile COPYING HELP
tagfiles = $(sfiles) $(cfiles) $(gfiles) $(mfiles) $(tfiles)

installdirs:
	mkdir -p $(DESTDIR)$(mandir)man1/
	mkdir -p $(DESTDIR)$(jacallibdir)
	mkdir -p $(DESTDIR)$(bindir)
	mkdir -p $(DESTDIR)$(infodir)
	mkdir -p $(DESTDIR)$(htmldir)
	mkdir -p $(DESTDIR)$(pdfdir)
	mkdir -p $(DESTDIR)$(dvidir)

jacal.dvi: jacal.texi version.txi
	$(TEXI2DVI) -b -c $<
dvi:	jacal.dvi
xdvi:	jacal.dvi
	xdvi $<
install-dvi: jacal.dvi installdirs
	$(INSTALL_DATA) $< $(DESTDIR)$(dvidir)

jacal.pdf: jacal.texi version.txi
	$(TEXI2PDF) -b -c $<
pdf:	jacal.pdf
xpdf:	jacal.pdf
	xpdf $<
install-pdf: jacal.pdf installdirs
	$(INSTALL_DATA) $< $(DESTDIR)$(pdfdir)

# jacal_toc.html: jacal.texi version.txi
# 	$(TEXI2HTML) $<
# html: jacal_toc.html
# $(DESTDIR)$(htmldir)jacal_toc.html: jacal_toc.html
# 	-rm -f jacal_stoc.html
# 	if [ -f $(prevdocsdir)jacal_toc.html ]; \
# 	  then hitch $(prevdocsdir)jacal_\*.html jacal_\*.html \
# 		$(DESTDIR)$(htmldir); \
# 	  else $(INSTALL_DATA) jacal_*.html $(DESTDIR)$(htmldir);fi
# install-html: $(DESTDIR)$(htmldir)jacal_toc.html

html/jacal: jacal.texi version.txi $(txifiles) $(texifiles)
	mkdir -p html
	rm -rf html/jacal
	makeinfo --html $< -o html/jacal
	if type icoize>/dev/null; then icoize ../Logo/equal.ico html/jacal/*.html; fi
html:	html/jacal
$(DESTDIR)$(htmldir)jacal: html/jacal
	-rm -rf $(DESTDIR)$(htmldir)jacal
	mkdir -p $(DESTDIR)$(htmldir)jacal
	$(INSTALL_DATA) html/jacal/*.html $(DESTDIR)$(htmldir)jacal
install-html: $(DESTDIR)$(htmldir)jacal

# Used by w32install
jacal.html:	jacal.texi
	$(MAKEINFO) --html --no-split --no-warn --force $<

DOC/ratint.aux: DOC/ratint.tex DOC/eqalign.sty
	cd DOC/; latex ratint.tex
DOC/ratint.dvi: DOC/ratint.tex DOC/eqalign.sty DOC/ratint.aux
	cd DOC/; latex ratint.tex

$(DESTDIR)$(pdfdir)ratint.pdf: DOC/ratint.tex DOC/eqalign.sty
	cd DOC/; pdflatex ratint.tex
	$(INSTALL_DATA) DOC/ratint.pdf $(DESTDIR)$(pdfdir)

jacal-$(VERSION).info: jacal.texi version.txi
	$(MAKEINFO) $< --no-split -o $@
jacal.info: jacal-$(VERSION).info
	if [ -f $(prevdocsdir)jacal.info ];\
	  then infobar $(prevdocsdir)jacal.info $< $@;\
	  else cp jacal-$(VERSION).info jacal.info;fi
$(DESTDIR)$(infodir)jacal.info:	jacal.info
	$(INSTALL_DATA) $< $@
	-$(INSTALL_INFO) $@ $(DESTDIR)$(infodir)dir
	-rm $(DESTDIR)$(infodir)-jacal.info.gz
install-info:	$(DESTDIR)$(infodir)jacal.info
info:	install-info
$(DESTDIR)$(infodir)jacal.info.gz: $(DESTDIR)$(infodir)jacal.info
	gzip -f $<
install-infoz:	$(DESTDIR)$(infodir)jacal.info.gz
infoz:	install-infoz

jacal.doc: jacal.1
	nroff -man $< | ul -tunknown >$@
install-man: jacal.1 installdirs
	-$(INSTALL_DATA) $< $(DESTDIR)$(mandir)man1/

pdfs:	$(DESTDIR)$(pdfdir)jacal.pdf $(DESTDIR)$(pdfdir)ratint.pdf
dvis:	jacal.dvi DOC/ratint.dvi

docs: $(DESTDIR)$(infodir)jacal.info.gz \
      $(DESTDIR)$(htmldir)jacal_toc.html \
      $(DESTDIR)$(htmldir)DOC/ratint.dvi \
      $(DESTDIR)$(pdfdir)jacal.pdf \
      jacal.doc

jacal48:
	(echo ",batch on"; \
	 echo "(require 'fluid-let)"; \
	 echo "(slib:load \"$(jacallibdir)math\")"; \
	 echo "(require 'hensel)"; \
	 echo ",collect"; \
	 echo ",batch off"; \
	 echo ",dump $(DESTDIR)$(S48IMAGE) \"(jacal $(VERSION))\""; \
	 echo ",exit") | slib48 -h 5100000
unjacal48:
	-rm -f $(DESTDIR)$(S48IMAGE)

install-lib: $(libfiles) installdirs
	-$(INSTALL_DATA) $(libfiles) $(DESTDIR)$(jacallibdir)

install-script: jacal.sh installdirs
#	echo ";;(use-modules (ice-9 slib))"	   > go-scm
	echo "(slib:load \"$(jacallibdir)math\")"  > go-scm
	echo "(math)"				  >> go-scm
	$(INSTALL_DATA) go-scm $(DESTDIR)$(jacallibdir)go.scm
	rm go-scm
	echo '#! /bin/sh'			   > jacal-script
	grep -h ^SCHEME_LIBRARY_PATH= `which slib`>> jacal-script
	echo export SCHEME_LIBRARY_PATH		  >> jacal-script
	echo JACALDIR=$(DESTDIR)$(jacallibdir)	  >> jacal-script
	echo VERSION=$(VERSION)			  >> jacal-script
	cat $<					  >> jacal-script
	$(INSTALL_PROGRAM) jacal-script $(DESTDIR)$(bindir)jacal
	rm jacal-script

install: install-script install-lib install-infoz install-man

uninstall: unjacal48
	$(PRE_UNINSTALL)     # Pre-uninstall commands follow.
	-$(INSTALL_INFO) --delete $(DESTDIR)$(infodir)jacal.info \
	  $(DESTDIR)$(infodir)dir
	$(NORMAL_UNINSTALL)  # Normal commands follow.
	-rm $(DESTDIR)$(infodir)jacal.info*
	-rm $(DESTDIR)$(mandir)man1/jacal.1
	-rm $(DESTDIR)$(bindir)jacal
	cd $(DESTDIR)$(jacallibdir); rm $(libfiles)
#	$(POST_UNINSTALL)     # Post-uninstall commands follow.
	-rmdir $(DESTDIR)$(jacallibdir)

## to build a windows installer
## make sure makeinfo and NSIS are available on the commandline
w32install: jacal.nsi jacal.html
	makensis $<

check:
	scm -l math -e'(batch "test.math")'
t:
	scm -l math -e'(batch "t.math")'

#### Stuff for maintaining JACAL below ####

ver = $(VERSION)

lint:	$(cfiles) $(sfiles) $(gfiles)
	sclint $(cfiles) $(sfiles) $(gfiles)

temp/jacal/: $(DESTDIR)$(pdfdir)ratint.pdf $(allfiles)
	-rm -rf temp
	mkdir -p $@
	ln  $(allfiles) $@
	mkdir -p $@DOC/
	ln $< $@DOC/
	cd DOC; ln $(dfiles) ../temp/jacal/DOC/

#For change-barred HTML.
prevdocs:	$(prevdocsdir)jacal_toc.html $(prevdocsdir)jacal.info
$(prevdocsdir)jacal_toc.html:
$(prevdocsdir)jacal.info: Makefile
	cd $(prevdocsdir); unzip -a $(distdir)jacal*.zip
	rm $(prevdocsdir)jacal/jacal*.info
	cd $(prevdocsdir)jacal; make jacal.info; make jacal_toc.html
	cd $(prevdocsdir); mv -f jacal/jacal*.info ./
	cd $(prevdocsdir); mv -f jacal/*.html ./
	rm -rf $(prevdocsdir)jacal
	-rm -f jacal-$(VERSION).info

README: jacal-$(VERSION).info Makefile
	echo "This directory contains the distribution of jacal-$(VERSION).  Jacal is a" > $@
	echo "symbolic mathematics system written in the programming language Scheme." >> $@
	echo "" >> $@
	echo "	     http://people.csail.mit.edu/jaffer/JACAL.html" >> $@
	echo "" >> $@
	info -f jacal-$(VERSION).info -n 'Installation' -o - >> $@

release: dist README pdfs # rpm
	cvs tag -F jacal-$(VERSION)
	cp ANNOUNCE $(htmldir)JACAL_ANNOUNCE.txt
	$(RSYNC) $(htmldir)JACAL.html $(htmldir)JACAL_ANNOUNCE.txt \
	  $(pdfdir)ratint.pdf \
	  $(Uploadee):public_html/
	$(RSYNC) $(distdir)README $(distdir)jacal-$(VERSION).zip \
	  $(distdir)jacal-$(VERSION)-$(RELEASE).noarch.rpm \
	  $(distdir)jacal-$(VERSION)-$(RELEASE).src.rpm \
	  $(Uploadee):dist/
#	upload $(distdir)README $(distdir)jacal-$(VERSION).zip ftp.gnu.org:gnu/jacal/

upzip:	$(snapdir)jacal.zip
	$(RSYNC) $(snapdir)jacal.zip $(Uploadee):pub/

gnupzip:
	gnupload --to alpha.gnu.org:jacal $(snapdir)jacal.zip
## ??does this end up in http://www.artfiles.org/gnu.org/alpha/gnu/jacal/

dist:	$(distdir)jacal-$(VERSION).zip
$(distdir)jacal-$(VERSION).zip:	temp/jacal/
	$(MAKEDEV) DEST=$(distdir) PROD=jacal ver=-$(VERSION) zip

upgnu:	$(distdir)jacal-$(VERSION).tar.gz
	cd $(distdir); gnupload --to ftp.gnu.org:jacal jacal-$(VERSION).tar.gz
tar.gz:	$(distdir)jacal-$(VERSION).tar.gz
$(distdir)jacal-$(VERSION).tar.gz:	temp/jacal/
	$(MAKEDEV) DEST=$(distdir) PROD=jacal ver=-$(VERSION) tar.gz

rpm:	pubzip
# $(distdir)jacal-$(VERSION)-$(RELEASE).noarch.rpm:	$(distdir)jacal-$(VERSION).zip
	cp -f $(snapdir)jacal.zip $(rpm_prefix)SOURCES/jacal-$(VERSION).zip
	rpmbuild -ba --clean jacal.spec
	rm $(rpm_prefix)SOURCES/jacal-$(VERSION).zip
	mv $(rpm_prefix)RPMS/noarch/jacal-$(VERSION)-$(RELEASE).noarch.rpm \
	   $(rpm_prefix)SRPMS/jacal-$(VERSION)-$(RELEASE).src.rpm $(distdir)

shar:	jacal.shar
jacal.shar:	temp/jacal/
	$(MAKEDEV) PROD=jacal shar
dclshar:	jacal.com
com:	jacal.com
jacal.com:	temp/jacal/
	$(MAKEDEV) PROD=jacal com
zip:	jacal.zip
jacal.zip:	temp/jacal/
	$(MAKEDEV) PROD=jacal zip
doszip:	$(windistdir)jacal-$(VERSION).zip
$(windistdir)jacal-$(VERSION).zip: temp/jacal/ jacal.html equal.ico
	$(MAKEDEV) DEST=$(windistdir) PROD=jacal ver=-$(VERSION) zip
	-cd ..; zip -9ur $(windistdir)jacal-$(VERSION).zip \
		jacal/jacal.html jacal/equal.ico
	zip -d $(windistdir)jacal-$(VERSION).zip jacal/jacal.info
pubzip:	temp/jacal/
	$(MAKEDEV) DEST=$(snapdir) PROD=jacal zip

diffs:	pubdiffs
pubdiffs:	temp/jacal/
	$(MAKEDEV) DEST=$(snapdir) PROD=jacal pubdiffs
distdiffs:	temp/jacal/
	$(MAKEDEV) DEST=$(distdir) PROD=jacal ver=$(ver) distdiffs

CITERS = ANNOUNCE ../scm/ANNOUNCE $(htmldir)README.html $(distdir)README \
	$(windistdir)unzipall.bat $(windistdir)buildall
CITES = toploads.scm Makefile jacal.spec jacal.texi jacal.nsi \
	$(htmldir)JACAL.html

updates:
	$(CHPAT) jacal-$(VERSION) jacal-$(ver) $(CITERS)
	$(CHPAT) $(VERSION) $(ver) $(CITES)
	$(MAKE) README

new:	updates
	echo @set JACALVERSION $(ver) > version.txi
	echo @set JACALDATE `date +"%B %Y"` >> version.txi
	echo `date -I` \ Aubrey Jaffer \ \<`whoami`@`hostname`\>> change
	echo>> change
	echo \	\* toploads.scm \(*jacal-version*\): Bumped from $(VERSION) to $(ver).>>change
	echo>> change
	cat ChangeLog >> change
	mv -f change ChangeLog
	cvs commit -m '(*jacal-version*): Bumped from $(VERSION) to $(ver).'
	cvs tag -F jacal-$(ver)

tags:	$(tagfiles)
	etags $(tagfiles)

clean:
	-rm -f *~ *.bak *.orig *.rej core a.out *.o \#*
	-rm -rf *temp
distclean:	realclean
realclean:
	-rm -f *~ *.bak *.orig *.rej TAGS core a.out *.o \#*
	-rm -f jacal.info*
	-rm -rf *temp jacal.html JACAL-*.exe
realempty:	temp/jacal/
	-rm -f $(allfiles)
	cd DOC; rm -f $(dfiles) ratint.pdf
