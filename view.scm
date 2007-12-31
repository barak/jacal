;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1997 Aubrey Jaffer.
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


;;; See Template.scm in the Scheme Library for how to set up
;;; vicinities and require.

(require 'with-file)

;(slib:load (in-vicinity (program-vicinity) "scl"))
			;Common Lisp/Scheme compatability definitions.
;(slib:load (in-vicinity (program-vicinity) "toploads"))

(slib:load (in-vicinity (program-vicinity) "grammar"))

(define page-height #f)
(define page-width #f)
(define *input-grammar* (get-grammar 'scheme))
			;grammar to use if none is loaded.
(define *output-grammar* (get-grammar 'scheme))
			;grammar to use if none is loaded.
(define *echo-grammar* (get-grammar 'null))

(slib:load (in-vicinity (library-vicinity) "prec"))
;;(prec:trace)
(slib:load (in-vicinity (program-vicinity) "unparse"))
(slib:load (in-vicinity (program-vicinity) "English"))

(define *input-grammar* (get-grammar 'standard)) ;tex

(define (view . file)
  (define obj #f)
  (define math:prompt ":")
  ((lambda (doit)
     (cond ((not (null? file)) (with-input-from-file (car file) doit))
	   (else (doit))))
   (lambda ()
     (define cip (current-input-port))
     (let loop ()
       (cond ((output-port? cip)
	      (display math:prompt cip)
	      (force-output cip)
	      (tok:bump-column (string-length math:prompt) cip))
	     (else (display math:prompt)
		   (force-output)
		   (tok:bump-column (string-length math:prompt) cip)))
       (set! obj (read-sexp *input-grammar*))
       (tok:bump-column 0 cip)
       (cond ((and (null? file) (not obj)))
	     ((eof-object? obj))
	     ((not obj) (display "got #f") (newline))
	     (else (write-sexp obj *output-grammar*)
		   (newline)
		   (loop)))))))
;;(pretty-print (grammar-read-tab *input-grammar*))
(view)
;; scm -ip1 -lmath -e(batch"demo")
