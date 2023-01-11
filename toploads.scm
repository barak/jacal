;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1995, 1997, 2002, 2008 Aubrey Jaffer.
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

;;; If BYTE requires ARRAY, then it will redefine EQUAL?, which will
;;; screw up HASH-TABLE.  So require BYTE first.
(require 'byte)
(require 'hash-table)
(require-if 'compiling 'info)
(require-if 'compiling 'precedence-parse)

(define *jacal-version* "1c8")

(define (jacal:dot) (display ".") (force-output))
(jacal:dot)
(slib:load (in-vicinity (program-vicinity) "types"))
(jacal:dot)		;Variables and type conversions.
(define (math:exit b) (cleanup-handlers!) (slib:error "error in math system"))
	;error handling when not running (math) [read-eval-print loop]
;(define *diagnostic-output* (current-output-port))
(slib:load (in-vicinity (program-vicinity) "grammar"))
(jacal:dot)		;grammar, I/O, test, and error routines.
;;; Dynamic Variables -- These get set in "modeinit.scm"
		;grammars to use if none is loaded.
(define *input-grammar* (get-grammar 'scheme))
(define *output-grammar* (get-grammar 'scheme))
(define *echo-grammar* (get-grammar 'null))
(define tran:translations '())
(define Language #f)
(define math:debug #f)
(define math:phases #f)
(define math:trace #f)
(define linkradicals #f)
(define horner #f)
(define page-height #f)
(define page-width #f)
(define newextstr #f)
(define newlabelstr #f)
(define newlabelsym #f)
(define % #f)
(define *modulus* 0)

;(define *symdefs* '())			;":" environment.
(define *symdefs* (make-hash-table 37))	;":" environment.
(slib:load (in-vicinity (program-vicinity) "sexp"))
(jacal:dot)		;read-eval-print loop.  Conversion from
			;sexpression to internal form and back.
(slib:load (in-vicinity (program-vicinity) "poly"))
(jacal:dot)		;Routines which operate on internal data type POLY.
(slib:load (in-vicinity (program-vicinity) "elim"))
(jacal:dot)		;Routines which eliminate variables.
(slib:load (in-vicinity (program-vicinity) "vect"))
(jacal:dot)		;Routines which operate on lists of POLY (mtrx).
(slib:load (in-vicinity (program-vicinity) "norm"))
(jacal:dot)		;Differentials; logical operations.
(slib:load (in-vicinity (program-vicinity) "sqfree"))
(jacal:dot)		;square-free.
(slib:load (in-vicinity (program-vicinity) "builtin"))
(jacal:dot)		;Routines defined for sexpressions.
(define (info:describe obj)		;autoload for info
  (require 'info)
  (info:describe obj))
(define (info:example obj)		;autoload for info
  (require 'info)
  (info:example obj))
(define (definfo . args)		;autoload for info
  (require 'info)
  (apply definfo args))
(slib:load (in-vicinity (program-vicinity) "ext"))
(jacal:dot)		;Field extension creation and simplification.
(require 'precedence-parse)		;Fixup prec:warn
(define (prec:warn dyn . msgs)
  (do ((j (+ -1 (car (cddddr dyn))) (+ -8 j)))
      ((> 8 j)
       (do ((i j (+ -1 i)))
	   ((>= 0 i))
	 (display-diag #\space)))
    (display-diag slib:tab))
  (display-diag "^ ")
  (for-each (lambda (x)
	      (write-diag (tran:translate x))
	      (display-diag #\space))
	    msgs)
  (newline-diag))
(jacal:dot)		;General parser
(slib:load (in-vicinity (program-vicinity) "unparse"))
(jacal:dot)		;infix printer.
(catalog:read jacal-vicinity "jacalcat")
;;; These routines test the core mathematical routines;
;;; Beware if they produce warnings or errors.
(poly:test)		;Test for routines in "poly"
(elim:test)		;Test for routines in "bunch"
;(factor:test)
;(mtrx:test)
;;(slib:load (in-vicinity (program-vicinity) "debug"))

(newline)
(display "JACAL version ") (display *jacal-version*)
(display ", Copyright 1989-2020 Aubrey Jaffer
JACAL comes with ABSOLUTELY NO WARRANTY; for details type `(terms)'.
This is free software, and you are welcome to redistribute it
under certain conditions; type `(terms)' for details.
")
(display ";;; Type (math); to begin.")
(newline)
(force-output)
