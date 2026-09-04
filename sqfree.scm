;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1997, 2002, 2005, 2007, 2020, 2021, 2024, 2026 Aubrey Jaffer.
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

(require 'common-list-functions)
(require 'sort)

;;; Functions involved with square-freeness.
(define (poly:square-free-var p var)
  (poly:/ p (poly:gcd p (poly:diff p var))))

(define (poly:square-and-num-cont-free p)
  (if (number? p) (if (zero? p) p 1)
      (poly:* (poly:square-and-num-cont-free (univ:cont p))
	      (poly:square-free-var p (car p)))))

(define (negate-factors-exps fact-exps)
  (reverse
   (map (lambda (fact-exp) (list (car fact-exp) (- (cadr fact-exp))))
	fact-exps)))

;;;==================== Sort polynomial factors ====================
(define (poly:factor< x y)
  (define (lnumber? x)
    (cond ((number? x) #t)
	  ((list? x) (and (= 1 (length x)) (number? (car x))))
	  (else #f)))
  (cond ((eqv? x y) #f)
	((math:equal? x y) #f)
	((and (number? x) (number? y)) (< x y))
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

;;; Wrap POLY:FACTOR< with check for well-ordering.
;; (define poly:factor<
;;   (let ((factor< poly:factor<))
;;     (lambda (x y)
;;       (define x<y (factor< x y))
;;       (define y<x (factor< y x))
;;       (cond ((and x<y y<x)
;; 	     (slib:warn 'poly:factor< 'failed x y))
;; 	    ((and (not x<y) (not y<x)
;; 		  (not (math:equal? x y)))
;; 	     (slib:warn 'poly:factor< 'failed= x y))
;; 	    (else x<y)))))

(define (poly:sort-factors fs)
  (cond ((null? fs) fs)
	(else (sort fs poly:factor<))))

;;; Functions used to square-free factorize multivariate radicands.

(define (rat:sqfr-factors-list e1)
  (cond ((rat? e1)
	 (append (poly:sqfr-factors (num e1))
		 (negate-factors-exps (poly:sqfr-factors (denom e1)))))
	(else (poly:sqfr-factors e1))))

;;; Returns an alist of ((<factor>) <exp>) lists.
(define (poly:sqfr-factors e1)
  (define nc 1)
  (define (psf e1)
    (cond
     ((one? e1) '())
     ((number? e1) (int:factors e1))
     (else
      (let ((splitter (poly:gcd e1 (poly:diff e1 (car e1)))))
	(define fct0 (poly:/ e1 splitter))
	(define lc (leading-number fct0))
	(define fct (cond ((positive? lc) fct0)
			  (else (set! nc (* -1 nc))
				(poly:negate fct0))))
	(define lst (psf splitter))
	(cond
	 ((null? lst) (cons (list (list fct) 1) lst))
	 (else
	  (let ((quo (poly:/? fct (caaar lst))))
	    (cond ((not quo) (cons (list (list fct) 1) lst))
		  ((number? quo)
		   ;; (cons (list (caar lst) (+ 1 (cadar lst))) (cdr lst))
		   (cons (list (list (poly:/ fct quo)) 1) lst))
		  (else
		   (cons (list (list (poly:/ fct quo)) 1)
			 (cons (list (list quo) 1) lst)))))))))))
  ;; (require 'debug-jacal) (trace psf)
  (let ((fct1s (psf e1)))
    (define fcts '())
    (for-each (lambda (f)
		(set! fcts
		      (cond ((null? fcts) (list f))
			    ((math:equal? (car f) (caar fcts))
			     (cons (list (caar fcts) (+ (cadr f) (cadar fcts)))
				   (cdr fcts)))
			    (else (cons f fcts)))))
	      (poly:sort-factors
	       (if (one? nc) fct1s (cons (list (list -1) 1) fct1s))))
    fcts))

;;; This algorithm is due to:
;;; Multivariate Polynomial Factorization
;;; David R. Musser
;;; Journal of the Association for Computing Machinery
;;; Vol 22, No. 2, April 1975
;; (define (poly:sqfr-split c splitter)
;;   (let ((d '()) (aj '()) (b (poly:gcd c splitter))) ; changed #f's to ()
;;     (do ((b b (poly:/ b d))
;;          (a (poly:/ c b) d))
;;         ((number? b)
;;          (if (one? b)
;;              (cons a aj)                ; nreverse removed
;;              (cons b (cons a aj))))     ; nreverse removed
;;       (set! d (poly:gcd a b))
;;       (set! a (poly:/ a d))             ; 'a' is used as a temporary variable
;;       (if (not (eqv? a 1))              ; keeps extraneous `1's
;;         (set! aj (cons a aj))))))       ;  out of the results list

;;;; functions used in "ff.scm" and "hensel.scm"

;;;; GCL Algorithm 8.1 Square-Free Factorization (p. 340)
;;;
;;; Given a primitive polynomial a(v) which is an element of a
;;; Unique Factorisation Domain in v, calculate the square free
;;; factorisation of a(v).  A primitive polynomial has the content
;;; removed from its coefficients.  The content is the gcd of the
;;; coefficients.
;;;    - MJT
;;;
;;; Returns an alist of (<factor> <exp>) lists.
;; (define (univ:square-free-factorization a v)
;;   (define b (poly:diff a v))
;;   (define output '())
;;   (define y 0)
;;   (define z 0)
;;   (let* ((c (unitcan (poly:gcd a b)))
;; 	 (w (poly:/ a c)))
;;     (let loop ((i 1))
;;       (cond ((eqv? 1 c) (cons (list w i) output))
;; 	    (else (set! y (unitcan (poly:gcd w c)))
;; 		  (set! z (poly:/ w y))
;; ;;;		  (math:print "y = " y " w = " w " c = " c " z = " z)
;; 		  (if (not (number? z))
;; 		      (set! output (cons (list z i) output)))
;; 		  (set! w y)
;; 		  (set! c (poly:/ c y))
;; ;;;		  (math:print "c = " c)
;; 		  (loop (+ 1 i)))))))

;;;; Yun's Square-Free Factorization
;;;; GCL Algorithm 8.2 Square-Free Factorization (p. 342) or
;;;; http://www.inf.ethz.ch/personal/bernardi/publications/thesis.ps.gz
;;;
;;; Given a primitive polynomial a(x) element R[x], R is a UFD of
;;; characteristic zero, calculate the square-free factorization of a(x)
;;; using Yun's algorithm.
;;;
;;; Returns an alist of (<factor> <exp>) lists.
(define (yuniv:square-free-factorization a v)
  (define b (poly:diff a v))
  (cond (math:trace
	 (math:print 'yuniv:square-free-factorization a)
	 ;; (math:print a)
	 ))
  (let ((c (unitcan (poly:gcd a b))))
    (cond ((eqv? 1 c)
	   (if math:trace
	       (math:print 'yielding1 (list (list a 1)))
	       (list (list a 1))))
	  (else
	   (let ((w (poly:/ a c))
		 (y (poly:/ b c))
		 (output '()))
	     (define z (poly:- y (poly:diff w v)))
	     (do ((i 1 (+ 1 i)))
		 ((poly:0? z)
		  (set! output (cons (list w i) output))
		  (if math:trace
		      (math:print 'yielding* output)
		      output))
	       (let ((g (poly:gcd w z)))
		 (if (not (number? g))
		     (set! output (cons (list g i) output)))
		 (set! w (poly:/ w g))
		 (set! y (poly:/ z g))
		 (set! z (poly:- y (poly:diff w v))))))))))

(define (poly:square-free-factorization poly)
  (define (psff poly exp vexcludes)
    (define vars (set-difference (poly:vars poly) vexcludes))
    (if (null? vars)
	(list (list poly exp))
	(apply append
	       (map (lambda (factor-exp)
		      (psff (car factor-exp) (cadr factor-exp)
			    (append vexcludes (car vars))))
		    ))))
  (psff poly 1 '()))

;;; the following algorithm attempts to separate factors in a multivariate
;;; polynomial with major variable.  It substitues 0 for each variable
;;; that it finds in turn and takes GCD against the original expression.
;;; It assumes that it's argument is squarefree and contentfree in the
;;; major variable.
(define (univ:split pe varlist)
  (cond ((unit? pe) (list))
	((null? varlist) (list pe))
	((let ((p0 (unitcan
		    (poly:gcd pe (poly:subst0 (car varlist) pe))))
	       (cvl (cdr varlist)))
	   (if (unit? p0)
	       (univ:split pe cvl)
	       (nconc (univ:split (poly:/ pe p0) cvl)
		      (univ:split p0 cvl)))))))

;;;; But algorithm is variable ordering sensitive!
(define (univ:split-all poly) (univ:split poly (poly:vars poly)))
 
;; (define (poly:sqfr-all c)
;;   (poly:sqfr-split c (poly:diff-all c)))

;; (define (sqfr:test)
;;   (define x 'x)
;;   (math:test (list (list x -1 -2) (list x -1 0 1))
;;         poly:sqfr-all
;;         (list x -1 -4 -3 4 4)))

;;(require 'debug-jacal) (trace YUNIV:SQUARE-FREE-FACTORIZATION)
;;(trace POLY:SQFR-ALL POLY:DIFF-ALL POLY:SQFR-SPLIT)
