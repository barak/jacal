# Makefile for JACAL: Symbolic Mathematics System.
# Copyright (C) 1990 - 2005 Aubrey Jaffer.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
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

SHELL = /bin/sh
intro:
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

srcdir.mk: Makefile
	echo "srcdir = `pwd`/" > srcdir.mk
include srcdir.mk

VERSION = 1b7
RELEASE = 1

rpm_prefix=$(HOME)/rpmbuild/
prefix = /usr/local/
exec_prefix = $(prefix)
# directory where `make install' will put executable.
bindir = $(exec_prefix)bin/
libdir = $(exec_prefix)lib/
JACALLIB = $(prefix)lib/jacal/
# directory where `make install' will put manual page.
mandir = $(prefix)man/
man1dir = $(mandir)man1/
infodir = $(prefix)info/
S48IMAGE = $(JACALLIB)scheme48.image

PREVDOCS = prevdocs/

jacal$(VERSION).info:	version.txi jacal.texi
	-mv jacal.info jacaltmp.info
	makeinfo jacal.texi --no-split -o jacal.info
	mv jacal.info jacal$(VERSION).info
	-mv jacaltmp.info jacal.info
jacal.info:	jacal$(VERSION).info
	if [ -f $(PREVDOCS)jacal.info ];\
		then infobar $(PREVDOCS)jacal.info jacal$(VERSION).info jacal.info;\
		else cp jacal$(VERSION).info jacal.info;fi
info:	installinfo
installinfo:	$(infodir)jacal.info
$(infodir)jacal.info:	jacal.info
	cp -p jacal.info $(infodir)jacal.info
	-install-info $(infodir)jacal.info $(infodir)dir
	-rm $(infodir)jacal.info.gz
infoz:	installinfoz
installinfoz:	$(infodir)jacal.info.gz
$(infodir)jacal.info.gz:	$(infodir)jacal.info
	gzip -f $(infodir)jacal.info

pinstall:	jacal.1
	test -d $(mandir) || mkdir $(mandir)
	test -d $(man1dir) || mkdir $(man1dir)
	-cp jacal.1 $(man1dir)
	test -d $(libdir) || mkdir $(libdir)
	test -d $(JACALLIB) || mkdir $(JACALLIB)
	-cp $(sfiles) $(cfiles) $(gfiles) jacalcat Makefile COPYING HELP $(JACALLIB)

install:	pinstall
	echo ";;(use-modules (ice-9 slib))"	 > $(JACALLIB)go.scm
	echo "(slib:load \"$(JACALLIB)math\")"	 > $(JACALLIB)go.scm
	echo "(math)"				>> $(JACALLIB)go.scm
	test -d $(bindir) || mkdir $(bindir)
	echo '#! /bin/sh'			 > $(bindir)jacal
	echo JACALDIR=$(JACALLIB)		>> $(bindir)jacal
	echo VERSION=$(VERSION)			>> $(bindir)jacal
	cat jacal.sh				>> $(bindir)jacal
	chmod +x $(bindir)jacal

uninstall:
	-rm $(bindir)jacal
	-rm $(man1dir)jacal.1

htmldir=../public_html/
dvi:	jacal.dvi
jacal.dvi:	version.txi jacal.texi
	texi2dvi -b -c $(srcdir)jacal.texi
xdvi:	jacal.dvi
	xdvi jacal.dvi

pdfs:	$(htmldir)jacal.pdf $(htmldir)ratint.pdf
pdf:	$(htmldir)jacal.pdf
$(htmldir)jacal.pdf:	version.txi jacal.texi
	texi2pdf -b -c $(srcdir)jacal.texi
	mv jacal.pdf $(htmldir)
xpdf:	$(htmldir)jacal.pdf
	xpdf $(htmldir)jacal.pdf

TEXI2HTML = /usr/local/bin/texi2html -split -verbose
jacal_toc.html:	version.txi jacal.texi
	${TEXI2HTML} jacal.texi
html:	$(htmldir)jacal_toc.html
$(htmldir)jacal_toc.html:	jacal_toc.html Makefile
	-rm -f jacal_stoc.html
	if [ -f $(PREVDOCS)jacal_toc.html ]; \
		then hitch $(PREVDOCS)jacal_\*.html jacal_\*.html $(htmldir); \
		else cp jacal_*.html $(htmldir);fi

DOCdir=$(srcdir)DOC/
dvis:	dvi $(DOCdir)ratint.dvi
$(DOCdir)ratint.aux:	$(DOCdir)ratint.tex $(DOCdir)eqalign.sty
	cd $(DOCdir); latex ratint.tex
$(DOCdir)ratint.dvi:	$(DOCdir)ratint.tex $(DOCdir)eqalign.sty \
		$(DOCdir)ratint.aux
	cd $(DOCdir); latex ratint.tex
$(htmldir)ratint.pdf:	$(DOCdir)ratint.tex $(DOCdir)eqalign.sty
#		$(DOCdir)ratint.aux
	cd $(DOCdir); pdflatex ratint.tex
	mv -f $(DOCdir)ratint.pdf $(htmldir)

jacal.doc:	jacal.1
	nroff -man $< | ul -tunknown >$@

#### Stuff for maintaining JACAL below ####

ver = $(VERSION)
version.txi:	Makefile
	echo @set JACALVERSION $(ver) > version.txi
	echo @set JACALDATE `date +"%B %Y"` >> version.txi

cfiles = math.scm modeinit.scm debug.scm view.scm toploads.scm
sfiles = types.scm func.scm poly.scm elim.scm \
	vect.scm ext.scm norm.scm sqfree.scm hist.scm sexp.scm \
	grammar.scm unparse.scm builtin.scm info.scm \
	tensor.scm combin.scm ff.scm factors.scm uv-hensel.scm hensel.scm
gfiles = English.scm
mfiles = ANNOUNCE COPYING HELP Makefile jacalcat jacal.texi fdl.texi jacal.1 \
	demo test.math rw.math jacal.spec jacal.sh
allfiles = README ChangeLog $(mfiles) $(sfiles) $(cfiles) $(gfiles) \
	jacal.info jacal.doc
#dfiles Document internals of Jacal.
dfiles = algdenom grammar history lambda ratint.tex eqalign.sty
# Common Lisp not currently supported.
lfiles = scl.lisp math.lisp compilem.lisp

jacal48:
	(echo ",batch on"; \
	 echo "(require 'fluid-let)"; \
	 echo "(slib:load \"$(JACALLIB)math\")"; \
	 echo "(require 'hensel)"; \
	 echo ",collect"; \
	 echo ",batch off"; \
	 echo ",dump $(S48IMAGE) \"(jacal $(VERSION))\""; \
	 echo ",exit") | slib48 -h 4500000
unjacal48:
	rm -f $(S48IMAGE)

lint:	$(cfiles) $(sfiles) $(gfiles)
	sclint $(cfiles) $(sfiles) $(gfiles)

docs:	$(infodir)jacal.info.gz $(htmldir)jacal_toc.html ratint.dvi \
	$(htmldir)jacal.pdf jacal.doc
	xdvi -s 4 jacal.dvi

makedev = make -f $(HOME)/makefile.dev
CHPAT=$(HOME)/bin/chpat
RSYNC=rsync --rsync-path=bin/rsync -bav
UPLOADEE=swissnet_upload
dest = $(HOME)/dist/
DOSCM = /misc/usb1/scm/

temp/jacal:	$(allfiles) $(htmldir)ratint.pdf
	-rm -rf temp
	mkdir temp
	mkdir temp/jacal
	ln  $(allfiles) temp/jacal
	mkdir temp/jacal/DOC
	ln $(htmldir)ratint.pdf temp/jacal/DOC
	cd DOC; ln $(dfiles) ../temp/jacal/DOC

infotemp/jacal:	jacal.info
	-rm -rf infotemp
	mkdir infotemp
	mkdir infotemp/jacal
	ln jacal.info jacal.info-* infotemp/jacal

#For change-barred HTML.
$(PREVDOCS)jacal_toc.html:
$(PREVDOCS)jacal.info:	srcdir.mk Makefile
	cd $(PREVDOCS); unzip -a $(dest)jacal*.zip
	rm $(PREVDOCS)jacal/jacal*.info
	cd $(PREVDOCS)jacal; make jacal.info; make jacal_toc.html
	cd $(PREVDOCS); mv -f jacal/jacal*.info ./
	cd $(PREVDOCS); mv -f jacal/*.html ./
	rm -rf $(PREVDOCS)jacal

distinfo:	$(dest)jacal.info.zip
$(dest)jacal.info.zip:	infotemp/jacal
	$(makedev) TEMP=infotemp/ DEST=$(dest) PROD=jacal ver=.info zip
	rm -rf infotemp

README: jacal$(VERSION).info Makefile
	echo "This directory contains the distribution of jacal$(VERSION).  Jacal is a" > README
	echo "symbolic mathematics system written in the programming language Scheme." >> README
	echo "" >> README
	echo "	     http://swiss.csail.mit.edu/~jaffer/JACAL.html" >> README
	echo "" >> README
	info -f jacal$(VERSION).info -n 'Installation' -o - >> README

release:	dist README pdfs # rpm
	cvs tag -F jacal$(VERSION)
	cp $(srcdir)ANNOUNCE $(htmldir)JACAL_ANNOUNCE.txt
	$(RSYNC) $(htmldir)JACAL.html $(htmldir)JACAL_ANNOUNCE.txt $(UPLOADEE):public_html/
	$(RSYNC) $(dest)README $(dest)jacal$(VERSION).zip\
	$(dest)jacal-$(VERSION)-$(RELEASE).noarch.rpm\
	$(htmldir)ratint.pdf $(htmldir)jacal.pdf\
	$(dest)jacal-$(VERSION)-$(RELEASE).src.rpm $(UPLOADEE):dist/
#	upload $(dest)README $(dest)jacal$(VERSION).zip ftp.gnu.org:gnu/jacal/
#	$(MAKE) indiana
indiana:
	upload $(dest)jacal$(VERSION).zip ftp@ftp.cs.indiana.edu:/pub/scheme-repository/incoming
	echo -e \
	'I have uploaded jacal$(VERSION).zip to ftp.cs.indiana.edu:/pub/scheme-repository/incoming\n' \
	'for placement into ftp.cs.indiana.edu:/pub/scheme-repository/code/num/' \
	 | mail -s 'JACAL upload' -b jaffer scheme-repository-request@cs.indiana.edu

postnews:
	echo -e "Newsgroups: sci.math.symbolic,comp.lang.scheme\n" \
	| cat - ANNOUNCE | inews -h -O -S \
	-f "announce@voluntocracy.org (Aubrey Jaffer)" \
	 -t "JACAL$(VERSION) Released" -d world

upzip:	$(HOME)/pub/jacal.zip
	$(RSYNC) $(HOME)/pub/jacal.zip $(UPLOADEE):pub/

dist:	$(dest)jacal$(VERSION).zip
$(dest)jacal$(VERSION).zip:	temp/jacal
	$(makedev) DEST=$(dest) PROD=jacal ver=$(VERSION) zip

rpm:	pubzip
# $(dest)jacal-$(VERSION)-$(RELEASE).noarch.rpm:	$(dest)jacal$(VERSION).zip
	cp -f $(HOME)/pub/jacal.zip $(rpm_prefix)SOURCES/jacal$(VERSION).zip
	rpmbuild -ba --clean jacal.spec
	rm $(rpm_prefix)SOURCES/jacal$(VERSION).zip
	mv $(rpm_prefix)RPMS/noarch/jacal-$(VERSION)-$(RELEASE).noarch.rpm \
	   $(rpm_prefix)SRPMS/jacal-$(VERSION)-$(RELEASE).src.rpm $(dest)

shar:	jacal.shar
jacal.shar:	temp/jacal
	$(makedev) PROD=jacal shar
dclshar:	jacal.com
com:	jacal.com
jacal.com:	temp/jacal
	$(makedev) PROD=jacal com
zip:	jacal.zip
jacal.zip:	temp/jacal
	$(makedev) PROD=jacal zip
doszip:	$(DOSCM)dist/jacal$(VERSION).zip
$(DOSCM)dist/jacal$(VERSION).zip:	temp/jacal
	$(makedev) DEST=$(DOSCM)dist/ PROD=jacal ver=$(VERSION) zip
	zip -d $(DOSCM)dist/jacal$(VERSION).zip jacal/jacal.info
pubzip:	temp/jacal
	$(makedev) DEST=$(HOME)/pub/ PROD=jacal zip

diffs:	pubdiffs
pubdiffs:	temp/jacal
	$(makedev) DEST=$(HOME)/pub/ PROD=jacal pubdiffs
distdiffs:	temp/jacal
	$(makedev) DEST=$(dest) PROD=jacal ver=$(ver) distdiffs

CITERS = ANNOUNCE ../scm/ANNOUNCE $(htmldir)README.html ../dist/README \
	$(DOSCM)dist/install.bat $(DOSCM)dist/makefile $(DOSCM)dist/mkdisk.bat
CITES = toploads.scm Makefile $(htmldir)JACAL.html jacal.spec jacal.texi

updates:
	$(CHPAT) jacal$(VERSION) jacal$(ver) $(CITERS)
	$(CHPAT) jacal-$(VERSION) jacal-$(ver) $(CITERS)
	$(CHPAT) $(VERSION) $(ver) $(CITES)
	make README

new:	updates
	echo `date -I` \ Aubrey Jaffer \ \<`whoami`@`hostname`\>> change
	echo>> change
	echo \	\* toploads.scm \(*jacal-version*\): Bumped from $(VERSION) to $(ver).>>change
	echo>> change
	cat ChangeLog >> change
	mv -f change ChangeLog
	cvs commit -m '(*jacal-version*): Bumped from $(VERSION) to $(ver).'
	cvs tag -F jacal$(ver)

tags:	$(sfiles) $(cfiles) $(gfiles) $(mfiles)
	etags $(sfiles) $(cfiles) $(gfiles) $(mfiles)

clean:
	-rm -f *~ *.bak *.orig *.rej core a.out *.o \#*
	-rm -rf *temp
distclean:	realclean
realclean:
	-rm -f *~ *.bak *.orig *.rej TAGS core a.out *.o \#*
	-rm -f jacal.info*
	-rm -rf *temp
realempty:	temp/jacal
	-rm -f $(allfiles)
	cd DOC; rm -f $(dfiles) ratint.pdf
