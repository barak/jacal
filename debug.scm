;;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;;; Copyright 1989, 1990, 1991, 1992, 1993, 1995, 1996, 1997, 2002 Aubrey Jaffer.
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


;;; The following are useful for debugging representational and other
;;; problems.

;;; (POLY:VALID? obj) returns #t if the variable ordering in obj is
;;; consistent with respect to var:>.

;;; SCM has (STACK-TRACE), which prints evaluation pending expressions.

;;; To print out an expression in the current output format, do:
;;; (write-sexp (cano->sexp exp #f) *output-grammar*) (force-output)

;;; (MATH:BREAK-WITH-OBJECT MSG OBJ) prints MSG, sets the jacal
;;; variable errobj to OBJ, and breaks to the jacal prompt.

;;; (MATH:CONTINUABLE-BREAK-WITH-OBJECT MSG OBJ) prints MSG, sets the
;;; jacal variable errobj to OBJ, saves its continuation and breaks to
;;; the jacal prompt.

(require 'polynomial-factors)
(require 'debug)

(define math:break-continuation-stack '())

(define (math:break-with-object msg obj)
  (display-diag msg) (newline-diag)
  (display-diag " Offending object is now value of errobj.") (newline-diag)
  (defsym 'errobj obj)
  (math:exit #f))

(define (math:continuable-break-with-object msg obj)
  (display-diag msg) (newline-diag)
  (display-diag " Offending object is now value of errobj.") (newline-diag)
  (display-diag " To continue type continue(RETURN-VALUE);") (newline-diag)
  (defsym 'errobj obj)
  (call-with-current-continuation
   (lambda (cont)
     (set! math:break-continuation-stack
	   (cons cont math:break-continuation-stack))
     (math:exit #f))))

(define (qpn . args)
  (force-output)
  (for-each (lambda (x) (math:print x)
		    (if (symbol? x) (display-diag #\space) (newline-diag)))
	    args))

(define (print . args)
  (define result #f)
  (for-each (lambda (x) (set! result x) (math:print x)
		    (display-diag #\space))
	    args)
  (newline-diag)
  result)

(define (debug:check proc1 proc2 . opts)
  (lambda args
    (let ((result1 (apply proc1 args))
	  (result2 (apply proc2 args)))
      (cond ((not (equal? result1 result2))
	     (newline-diag)
	     (display-diag "WARN:")
	     (for-each (lambda (x) (display-diag #\space) (write-diag x)) opts)
	     (display-diag " results not equal?")
	     (newline-diag)
	     (display-diag " args:")
	     (for-each (lambda (x) (display-diag #\space) (math:print x)) args)
	     (newline-diag)
	     (display-diag (if (null? opts) "{1}" (car opts)))
	     (display-diag " ==> ")
	     (math:print result1)
	     (newline-diag)
	     (display-diag (if (or (null? opts) (null? (cdr opts))) "{1}"
			       (cadr opts)))
	     (display-diag " ==> ")
	     (math:print result2)
	     (newline-diag)))
      result1)))

;;; Wrap POLY:FACTOR< with check for well-ordering.
(define poly:factor<
  (let ((factor< poly:factor<))
    (lambda (x y)
      (define x<y (factor< x y))
      (define y<x (factor< y x))
      (cond ((and x<y y<x)
	     (slib:error 'poly:factor< 'failed x y))
	    ((and (not x<y) (not y<x)
		  (not (equal? x y)))
	     (slib:error 'poly:factor< 'failed= x y))
	    (else x<y)))))

(provide 'qp)
