;; "factors.scm" Polynomial factors.		-*-scheme-*-
;; Copyright 1994, 1995 Mike Thomas
;; Copyright 1995, 1997, 1998, 1999, 2001, 2002, 2003, 2005, 2007, 2009, 2020, 2021, 2024 Aubrey Jaffer
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

(require 'finite-fields)
(require 'sort)
(require 'common-list-functions)

;;; NUMCONT is the integer numeric content.
(define (prepend-integer-factor numcont factors)
  (cond ((one? numcont) factors)
	((eqv? -1 numcont) (cons (list (list numcont) 1) factors))
	((number? (caaar factors))
	 (cons (list (list (* numcont (caaar factors))) 1)
	       (cdr factors)))
	(else (cons (list (list numcont) 1) factors))))

(define (expand-integer-factors factors)
  (cond ((number? (caar factors))
	 (append (int:factors (caar factors)) (cdr factors)))
	((number? (caaar factors))
	 (append (int:factors (caaar factors)) (cdr factors)))
	(else factors)))

;;; Special Var Power Factors (of polynomial)
(define (svpf poly)
  (if (number? poly)
      1
      (let loop ((p (cdr poly)) (n 0))
	(if (eqv? 0 (car p))
	    (if (null? (cdr p))
		(+ 1 n)
		(loop (cdr p) (+ 1 n)))
	    n))))

(define (poly:sort-merge-factors fs)
  (define (doit facts exp factors-list)
    (cond ((null? factors-list) (list (list (poly:sort-factors facts) exp)))
	  ((math:equal? exp (cadar factors-list))
	   (doit (append facts (caar factors-list)) exp (cdr factors-list)))
	  (else (cons (list (poly:sort-factors facts) exp)
		      (doit (caar factors-list)
			    (cadar factors-list)
			    (cdr factors-list))))))
  (cond
   ((null? fs) fs)
   (else
    (let ((factors-list (poly:sort-factors fs)))
      (doit (caar factors-list) (cadar factors-list) (cdr factors-list))))))

;;; ================================================================

;;; FACTORS-LIST is a list of lists of a list of factors and exponent.
;;; FACT-EXPS is a list of lists of factor and exponent.
(define (factors->sexp factors-list)
  (apply sexp:*
	 (map (lambda (fact-exp)
		(sexp:^
		 (if (number? (car fact-exp))
		     (int:factor (car fact-exp))
		     (cano->sexp (car fact-exp) #f))
		 (cadr fact-exp)))
	      (poly:sort-factors (factors-list->fact-exps factors-list)))))

(define (nuf-def poly)
  (define den (denom poly))
  (define nufa (poly:factorz (num poly)))
  (define defa (if (number? den)
		   (list (list (list den) 1))
		   (poly:factorz den)))
  (cond ((negative? (sign (cond ((number? defa) defa)
				((number? (caaar defa)) (caaar defa))
				(else 1))))
	 (list (expand-integer-factors (prepend-integer-factor -1 nufa))
	       (expand-integer-factors (prepend-integer-factor -1 defa))))
	(else (list (expand-integer-factors nufa)
		    (expand-integer-factors defa)))))

;;; return an sexp product of sorted factors of the polynomial POLY
;;; over the integers (Z)
(define (rat:factor->sexp poly)
  (cond ((rat? poly)
	 (let ((nufa-defa (nuf-def poly)))
	   (sexp:over (factors->sexp (car nufa-defa))
		      (factors->sexp (cadr nufa-defa)))))
	(else (factors->sexp (expand-integer-factors (poly:factorz poly))))))
(define (rat:factors poly)
  (poly:sort-merge-factors
   (cond ((rat? poly)
	  (let ((nufa-defa (nuf-def poly)))
	    (append (car nufa-defa) (negate-factors-exps (cadr nufa-defa)))))
	 (else (expand-integer-factors (poly:factorz poly))))))

;; (require 'debug-jacal) (trace RAT:FACTORS RAT:FACTOR->SEXP EXPAND-INTEGER-FACTORS RAT:CONT-CAN)
