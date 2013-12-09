;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1997 Aubrey Jaffer.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
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

(require 'sort)
(require 'common-list-functions)

;;; An algebraic extension is the root of a polynomial with more than
;;; one distinct value.  These values are not linked;  the difference
;;; between two algebraic extensions which are roots of identical
;;; polynomials is not 0.  Radicals have an additional rule that
;;; exponents of "positive" radicands commute.  For instance:
;;; (x^2)^(1/2) ==> x.  Notice that ((-x)^2)^(1/2) ==> x also.
;;; (-x^2)^(1/2) ==> (-1)^(1/2)*x.

;;; algebraic extensions
;;; we want to find all extensions used by this poly except this poly.
(define (alg:exts poly)
  (let ((elts '()))
    (poly:for-each-var
     (lambda (v)
       (let ((er (extrule v)))
	 (if (and er (not (eq? er poly)))
	     (set! elts (adjoin v elts)))))
     poly)
    elts))

(define (application? v)
  (and (not (extrule v)) (pair? (var:sexp v))
       (not (eq? 'differential (car (var:sexp v))))))

;;; we want to find all functionals used by this poly except.
(define (var:funcs poly)
  (let ((elts '()))
    (poly:for-each-var
     (lambda (v)
       (if (application? v)
	   (set! elts (adjoin v elts))))
     poly)
    elts))

;;; algebraic and applications
(define (chainables poly)
  (let ((elts '()))
    (poly:for-each-var
     (lambda (v)
       (let ((er (extrule v)))
	 (if (or (and er (not (eq? er poly))) (application? v))
	     (set! elts (adjoin v elts)))))
     poly)
    elts))

;;;alg:vars returns a list of all terminal vars used in this or in extensions
;;;used in this.
(define (alg:vars poly)
  (let ((deps '()))
    (poly:for-each-var
     (lambda (v)
       (if (and (not (extrule v)) (null? (var:depends v)))
	   (set! deps (adjoin v deps)))
       (set! deps (union (var:depends v) deps)))
     poly)
    deps))

(define (alg:square-free-var p var)
  (alg:/ p (alg:gcd p (alg:diff p var))))

;;; This is for equations
;;; Don't simplify a rule with itself
(define (alg:simplify p)
  (let* ((vars (sort (alg:exts p) var:>))
	 (exrls (map extrule vars)))
    (if (memv p exrls)
	p
	(let ((ans p))
	  (for-each (lambda (r v) (set! ans (poly:prem ans r v)))
		    exrls vars)
	  ans))))

(define (alg:clear-denoms poly)
  (define p poly)
  (cond (math:trace
	 (display-diag 'clear-denoms:) (newline-diag)))
  (do ((v (poly:find-var-if? (rat:denom p) potent-extrule)
	  (poly:find-var-if? (rat:denom p) potent-extrule))
       (oldv "foo" (car v)))
      ((not v) p)
    (if (eq? (car v) oldv)
	(eval-error 'could-not-clear-denominator-of:- poly))
    (set! p (alg:simplify (poly:* p (alg:conjugate (rat:denom p) v))))))

;;; This generates conjugates for any algebraic by a wonderful theorem of mine.
;;; 4/30/90 jaffer
(define (alg:conjugate poly extpoly)
  (let* ((var (car extpoly))
	 (poly (poly:promote var poly))
	 (pdiv (univ:pdiv extpoly (if (shorter? poly extpoly)
				      poly
				      (univ:prem poly extpoly))))
	 (pquo (car pdiv))
	 (prem (cadr pdiv)))
    (if (zero? (univ:degree prem var))
	(univ:demote pquo)
	(poly:* (univ:demote pquo) (alg:conjugate prem extpoly)))))

;;; This section attempts to implement an incremental version of
;;; Caviness, B.F., Fateman, R.:
;;; Simplification of Radical Expressions.
;;; SYMSAC 1976, 329-338
;;; as described in
;;; Buchberger, B., Collins, G.E., Loos, R.:
;;; Computer Algebra, Symbolic and Algebraic Computation. Second Edition
;;; Springer-Verlag/Wein 1983, 20-22
;;; This algorithm for canonical simplification of UNNESTED radical expressions
;;; also has the convention that (s * t)^r = s^r * t^r.
;;; If the variable LINKRADICALS is #f then a new multiple value expression
;;; is returned for each radical.

;;; this is actually alg:depth
;(define (rad:depth imp)
;  (let ((exts (alg:exts imp)))
;    (if (null? exts)
;	0
;      (+ 1 (apply max (map (lambda (x) (rad:depth (extrule x))) exts))))))

;;; Integer power of EXPR
(define (ipow a pow)
  (if (not (integer? pow)) (math:error 'non-integer-power?- pow))
  (cond ((expl? a) (if (< pow 0)
		       (make-rat 1 (poly:^ a (- pow)))
		       (poly:^ a pow)))
	((rat? a) (if (< pow 0)
		      (make-rat (ipow (rat:denom a) (- pow))
				(ipow (rat:num a) (- pow)))
		      (make-rat (ipow (rat:num a) pow)
				(ipow (rat:denom a) pow))))
	(else (if (< pow 0)
		  (app* (list $ 1 (univ:monomial -1 (- pow) $1)) a)
		  (app* (univ:monomial 1 pow $1) a)))))

(define (^ a pow)
  (cond
   ((not (rat:number? pow)) (deferop _^ a pow))
   ((eqn? a) (math:error 'expt-of-equation?:- a))
   (else
    (set! pow (expr:normalize pow))
    (let ((expnum (num pow))
	  (expdenom (denom pow)))
      (cond
       ((eqv? 1 expdenom) (ipow a expnum))
       (linkradicals
	(set! a (expr:normalize a))
	(cond ((expl? a) (ipow (make-radical-exts a expdenom) expnum))
	      ((not (rat? a)) (math:error 'non-rational-radicand:- a))
	      ((rat:unit-denom? a)
	       (ipow (make-radical-exts (poly:* (denom a) (num a)) expdenom)
		     expnum))
	      (else (ipow (make-rat (make-radical-exts (rat:num a) expdenom)
				    (make-radical-exts (rat:denom a) expdenom))
			  expnum))))
       ((> expnum 0)
	(let ((tmp (univ:monomial -1 expdenom $)))
	  (set-car! (cdr tmp) (univ:monomial 1 expnum $1))
	  (app* tmp a)))
       (else
	(let ((tmp (univ:monomial (univ:monomial -1 (- expnum) $1) expdenom $)))
	  (set-car! (cdr tmp) 1)
	  (app* tmp a))))))))

;;; Generate extensions for radicals of polynomials
;;; Currently this does not split previously defined radicands.
;;; It will as soon as expression rework is added.
(define (make-radical-exts p r)
  (reduce-init poly:* 1 (map (lambda (fact-exp)
			       (ipow (make-radical-ext (car fact-exp) r)
				     (cadr fact-exp)))
			     (factors-list->fact-exps (rat:factors-list p)))))

(define (make-radical-ext p r)
  (set! p (licit->polxpr p))
  (let ((e (member-if (lambda (e) (equal? p (cadr e))) radical-defs)))
    (cond (e (if (divides? r (length (cddr (car e))))
		 (radpow (car e) r)
		 (var->expl (make-rad-var p r))))
	  (else (var->expl (make-rad-var p r))))))

(define (radpow radrule r)
  (univ:monomial 1 (quotient (length (cddr radrule)) r) (car radrule)))

;;;	Copyright 1989, 1990, 1991, 1992, 1993 Aubrey Jaffer.
