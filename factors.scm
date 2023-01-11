;; "factors.scm" Polynomial factors.		-*-scheme-*-
;; Copyright 1994, 1995 Mike Thomas
;; Copyright 1995, 1997, 1998, 1999, 2001, 2002, 2020, 2021 Aubrey Jaffer
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

(define (rat:cont-can nufa defa)
  (cond ((negative? (sign (cond ((number? defa) defa)
				((number? (caaar defa)) (caaar defa))
				(else 1))))
	 (list (prepend-integer-factor -1 nufa)
	       (prepend-integer-factor -1 defa)))
	(else (list nufa defa))))

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

(define (poly:sort-factors fs)
  (cond ((null? fs) fs)
	(else (sort! fs poly:factor<))))

(define (poly:sort-merge-factors fs)
  (define (doit facts exp factors-list)
    (cond ((null? factors-list) (list (list (poly:sort-factors facts) exp)))
	  ((equal? exp (cadar factors-list))
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

;;; Factorise over the Rationals (Q)
;;; return an sexp product of sorted factors of the polynomial POLY
;;; over the integers (Z)
(define (rat:factor->sexp poly)
  (cond ((rat? poly)
	 (let ((nufa (poly:factorz (num poly)))
	       (defa (poly:factorz (denom poly))))
	   (define nufa-defa (rat:cont-can nufa defa))
	   (sexp:over (factors->sexp (expand-integer-factors (car nufa-defa)))
		      (factors->sexp (expand-integer-factors (cadr nufa-defa))))))
	(else (factors->sexp (expand-integer-factors (poly:factorz poly))))))
(define (rat:factors poly)
  (poly:sort-merge-factors
   (cond ((rat? poly)
	  (let ((nufa (poly:factorz (num poly)))
		(defa (poly:factorz (denom poly))))
	    (define nufa-defa (rat:cont-can nufa defa))
	    (append (expand-integer-factors (car nufa-defa))
		    (negate-factors-exps
		     (expand-integer-factors (cadr nufa-defa))))))
	 (else (expand-integer-factors (poly:factorz poly))))))

;; (require 'debug-jacal) (trace rat:factors rat:factor->sexp expand-integer-factors rat:cont-can)
