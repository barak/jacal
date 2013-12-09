;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993 Aubrey Jaffer.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


(set! Language "English")	;grammar <Language>.scm should exist

(set! math:debug #f)		; #t to break on soft errors, #f to continue.
				;Does extra checking.

(set! math:phases #f)		; #t to show phases of normaization.

(set! math:trace #f)		; #t to trace variable eliminations, #f not to.

(set! linkradicals #t)		;Relate radicals in the traditional manner.

(set! horner #f)		;Horner's rule on expression output

(set! page-height #t)		;#t for pagination on, #f off

(set! page-width #t)		;Number or #t to use SLIB's width. #f for wide.

(set! newextstr (string-standard-case "EXT0")) ;extension template

(set! newlabelstr (string-standard-case "E0")) ;prompt template

(set! newlabelsym (string->symbol newlabelstr))
(set! % novalue)

(let ((init (in-vicinity (user-vicinity) "mathinit")))
  (if (file-exists? init)
      (slib:load init)))			;User initialization file

(if (and Language (= 3 (length (list-of-grammars)))) ;none loaded yet.
    (slib:load (in-vicinity (program-vicinity) Language)))
