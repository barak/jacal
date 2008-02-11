#!/bin/bash
##"jacal" sh script; Find a Scheme implementation and run JACAL in it.
# Copyright (C) 2001, 2003, 2004 Aubrey Jaffer
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

usage="Usage: jacal [--version | -v]

  Display version information and exit successfully.

Usage: jacal SCHEME

  Start JACAL using Scheme implementation SCHEME.

Usage: jacal

  Run JACAL using (MIT) 'scheme', 'scm', 'gsi', 'mzscheme', 'guile-1.6',
  'slib48', 'larceny', 'scmlit', 'elk', 'sisc', or 'kawa'."
# in preceding notice, change guile to guile-1.6, by tb.

case "$1" in
    -v | --ver*) echo jacal "$VERSION"; exit 0;;
    "") if type scheme>/dev/null 2>&1; then
	  command=scheme
	fi;;
    -*) echo "$usage"; exit 1;;
    *) command="$1"
    shift
esac

if [ -z "$command" ]; then
    if type scm>/dev/null 2>&1; then
	command=scm; implementation=scm
    elif type gsi>/dev/null 2>&1; then
	command=gsi; implementation=gam
    elif type mzscheme>/dev/null 2>&1; then
	command=mzscheme; implementation=plt
# Changed from looking for 'guile' to 'guile-1.6' by tb.
    elif type guile-1.6>/dev/null 2>&1; then
	command=guile-1.6; implementation=gui
    elif type slib48>/dev/null 2>&1; then
	echo "do 'cd ${JACALDIR}; make jacal48'"
    elif type scheme48>/dev/null 2>&1; then
	echo "do 'cd ${SCHEME_LIBRARY_PATH}; make slib48'"
	echo "do 'cd ${JACALDIR}; make jacal48'"
    elif type larceny>/dev/null 2>&1; then
	command=larceny; implementation=lar
    elif type scmlit>/dev/null 2>&1; then
	command=scmlit; implementation=scm
    elif type elk>/dev/null 2>&1; then
	command=elk; implementation=elk
    elif type sisc>/dev/null 2>&1; then
	command=sisc; implementation=ssc
    elif type kawa>/dev/null 2>&1; then
	command=kawa; implementation=kwa
    else
	echo No Scheme implementation found.
	exit 1
    fi
# Gambit 4.0 doesn't allow input redirection; foils --version test.
elif [ "$command" == "gsi" ]; then implementation=gam
elif type $command>/dev/null 2>&1; then
  SPEW="`$command --version < /dev/null 2>&1`"
  if   echo ${SPEW} | grep 'Initialize load-path (colon-list of directories)'\
				       >/dev/null 2>&1; then implementation=elk
  elif echo ${SPEW} | grep 'MIT'       >/dev/null 2>&1; then implementation=mit
  elif echo ${SPEW} | grep 'UMB Scheme'>/dev/null 2>&1; then implementation=umb
  elif echo ${SPEW} | grep 'scheme48'  >/dev/null 2>&1; then implementation=s48
  elif echo ${SPEW} | grep 'MzScheme'  >/dev/null 2>&1; then implementation=plt
  elif echo ${SPEW} | grep 'larceny'   >/dev/null 2>&1; then implementation=lar
  elif echo ${SPEW} | grep 'Guile'     >/dev/null 2>&1; then implementation=gui
  elif echo ${SPEW} | grep 'SCM'       >/dev/null 2>&1; then implementation=scm
  elif echo ${SPEW} | grep 'SISC'      >/dev/null 2>&1; then implementation=ssc
  elif echo ${SPEW} | grep 'Kawa'      >/dev/null 2>&1; then implementation=kwa
  else implementation=
  fi
else
  echo "Program '$command' not found."
  exit 1
fi

case $implementation in
  scm);;
  s48);;
  *) if [ -z "${SCHEME_LIBRARY_PATH}" ]; then
	if type rpm>/dev/null 2>&1; then
	  SCHEME_LIBRARY_PATH=`rpm -ql slib 2>/dev/null \
	     | grep require.scm | sed 's%require.scm%%'`
	fi
     fi
     if [ -z "${SCHEME_LIBRARY_PATH}" ]; then
       if [ -d /usr/local/lib/slib/ ]; then
	  SCHEME_LIBRARY_PATH=/usr/local/lib/slib/
       elif [ -d /usr/share/slib/ ]; then
	  SCHEME_LIBRARY_PATH=/usr/share/slib/
       fi
     export SCHEME_LIBRARY_PATH
     fi;;
esac

# for gambit
case $implementation in
  gam) if [ -z "${LD_LIBRARY_PATH}" ]; then
	LD_LIBRARY_PATH=/usr/local/lib
	export LD_LIBRARY_PATH
	fi;;
esac

case $implementation in
  scm) echo $command -ip1 -l ${JACALDIR}go "$@"
       exec $command -ip1 -l ${JACALDIR}go "$@";;
  elk) echo $command -i -l ${JACALDIR}elk.scm "$@"
       exec $command -i -l ${JACALDIR}elk.scm "$@";;
  gam) echo $command -:s ${SCHEME_LIBRARY_PATH}gambit.init ${JACALDIR}go.scm "$@"
       exec $command -:s ${SCHEME_LIBRARY_PATH}gambit.init ${JACALDIR}go.scm "$@";;
  gui) echo $command -l ${SCHEME_LIBRARY_PATH}guile.init -l ${JACALDIR}go.scm "$@"
       exec $command -l ${SCHEME_LIBRARY_PATH}guile.init -l ${JACALDIR}go.scm "$@";;
  ssc) echo $command -e "(begin (load \"${SCHEME_LIBRARY_PATH}sisc.init\") (load \"${JACALDIR}go.scm\"))" -- "$@"
       exec $command -e "(begin (load \"${SCHEME_LIBRARY_PATH}sisc.init\") (load \"${JACALDIR}go.scm\"))" -- "$@";;
  kwa) echo $command -f ${SCHEME_LIBRARY_PATH}kawa.init -f ${JACALDIR}go.scm -- "$@"
       exec $command -f ${SCHEME_LIBRARY_PATH}kawa.init -f ${JACALDIR}go.scm -- "$@";;
  plt) echo $command -f ${SCHEME_LIBRARY_PATH}mzscheme.init -f ${JACALDIR}go.scm "$@"
       exec $command -f ${SCHEME_LIBRARY_PATH}mzscheme.init -f ${JACALDIR}go.scm "$@";;
  lar) echo $command -- -e "(require 'srfi-96)" ${JACALDIR}go.scm "$@"
       exec $command -- -e "(require 'srfi-96)" ${JACALDIR}go.scm "$@";;
  mit) echo $command -load ${SCHEME_LIBRARY_PATH}mitscheme.init -load ${JACALDIR}go "$@"
       exec $command -load ${SCHEME_LIBRARY_PATH}mitscheme.init -load ${JACALDIR}go "$@";;
  umb) echo umb-scheme does not run jacal; exit 1;;
  s48) if [ -f "${JACALDIR}scheme48.image" ]; then
	 echo ";;; Type (math) to begin."
	 echo scheme48 -h 4500000 -i ${JACALDIR}scheme48.image "$@"
	 exec scheme48 -h 4500000 -i ${JACALDIR}scheme48.image "$@"
       else
	 echo "scheme48 found; do: 'cd ${JACALDIR}; make jacal48'";
       fi
       exit 1;;
  *)   echo Program '$command' ??
       exit 1;;
esac
