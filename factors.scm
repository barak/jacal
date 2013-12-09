;; "factors.scm" Polynomial factors.		-*-scheme-*-
;; Copyright 1994, 1995 Mike Thomas
;; Copyright 1995, 1997, 1998, 1999, 2001, 2002 Aubrey Jaffer
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
(require 'fluid-let)
(require 'sort)
(require 'common-list-functions)

;;; NUMCONT is the integer numeric content.
(define (prepend-integer-factor numcont factors)
  (cond ((one? numcont) factors)
	(*sort-int-factors* (append (int:factors numcont) factors))
	(else (cons (list (list numcont) 1) factors))))

(define (negate-factors-exps fact-exps)
  (reverse
   (map (lambda (fact-exp) (list (car fact-exp) (- (cadr fact-exp))))
	fact-exps)))

;;; Special Var Power Factors (of polynomial)
(define (svpf poly)
  (let loop ((p (cdr poly)) (n 0))
    (if (eqv? 0 (car p))
	(if (null? (cdr p))
	    (+ 1 n)
	    (loop (cdr p) (+ 1 n)))
	n)))

;;;==================== Sort polynomial factors ====================
(define (poly:factor< x y)
  (define (lnumber? x)
    (cond ((number? x) #t)
	  ((list? x) (and (= 1 (length x)) (number? (car x))))
	  (else #f)))
  (cond ((and (number? x) (number? y)) (< x y))
	((and (lnumber? x) (lnumber? y)) (< (car x) (car y)))
	((lnumber? x) #t)
	((lnumber? y) #f)
	((null? x) #t)
	((null? y) #f)
	((and (symbol? x) (symbol? y)) (string<? (symbol->string x)
						 (symbol->string y)))
	((vector? (car x))
	 (cond ((string<? (vector-ref (car x) 1) (vector-ref (car y) 1))
		#t)
	       ((string=? (vector-ref (car x) 1) (vector-ref (car y) 1))
		(poly:factor< (cdr x) (cdr y)))
	       (else #f)))
	((> (length x) (length y)) #f)
	((< (length x) (length y)) #t)
	((and (list? x)
	      (list? y))
	 (cond
	  ((poly:factor< (univ:lc x) (univ:lc y)) #t)
	  ((poly:factor< (univ:lc y) (univ:lc x)) #f)
	  (else (poly:factor< (butlast x 1) (butlast y 1)))))
	((list? x)
	 (poly:factor< (but-last x 1) y))
	((list? y)
	 (poly:factor< x (but-last y 1)))
	(else
	 (slib:error "poly:factor<: unknown type" x y))))

(define (poly:sort-factors fs) (sort! fs poly:factor<))

(define (poly:sort-merge-factors fs)
  (define factors-list (poly:sort-factors fs))
  (define (doit facts exp factors-list)
    (cond ((null? factors-list) (list (list (poly:sort-factors facts) exp)))
	  ((equal? exp (cadar factors-list))
	   (doit (append facts (caar factors-list)) exp (cdr factors-list)))
	  (else (cons (list (poly:sort-factors facts) exp)
		      (doit (caar factors-list)
			    (cadar factors-list)
			    (cdr factors-list))))))
  (doit (caar factors-list) (cadar factors-list) (cdr factors-list)))
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

(define *sort-int-factors* #f)

;;; Factorise over the Rationals (Q)
;;; return an sexp product of sorted factors of the polynomial POLY
;;; over the integers (Z)
(define (rat:factor->sexp poly)
  (fluid-let ((*sort-int-factors* #f))
    (cond ((rat? poly)
	   (let ((nu (num poly))
		 (de (denom poly)))
	     (sexp:over (if (number? nu)
			    (int:factor nu)
			    (factors->sexp (poly:factorz nu)))
			(if (number? de)
			    (int:factor de)
			    (factors->sexp (poly:factorz de))))))
	  (else (factors->sexp (poly:factorz poly))))))
(define (rat:factors poly)
  (fluid-let ((*sort-int-factors* #t))
    (poly:sort-merge-factors
     (cond ((rat? poly)
	    (append (poly:factorz (num poly))
		    (negate-factors-exps (poly:factorz (denom poly)))))
	   (else (poly:factorz poly))))))
