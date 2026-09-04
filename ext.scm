;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1997, 1998, 1999, 2002, 2005, 2007, 2019, 2020, 2024, 2026 Aubrey Jaffer.
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

(require 'sort)
(require 'tsort)			; topological sort
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
(define (poly:exts poly)
  (define elts '())
  (poly:for-each-var
   (lambda (v)
     (let ((er (var:algrule v)))
       (if (and er (not (math:equal? er poly)))
	   (set! elts (cons v elts)))))
   poly)
  elts)

(define (poly:aexts poly)
  (define elts '())
  (poly:for-each-var
   (lambda (v)
     (let ((er (var:algrule v)))
       (if (and er
		(not (math:equal? er poly))
		(not (poly:find-var? er (var:differential v))))
	   (set! elts (cons v elts)))))
   poly)
  elts)

;;;alg:vars returns a list of all terminal vars used in this or in extensions
;;;used only by listofvars
(define (alg:vars poly)
  (define deps '())
  (poly:for-each-var
   (lambda (v)
     (if (and (not (var:algrule v)) (null? (var:depends v)))
	 (set! deps (cons v deps)))
     (set! deps (union (var:depends v) deps)))
   poly)
  deps)

(define (var:application? v)
  (and (not (var:algrule v))
       (pair? (var:sexp v))))

;;; used only in chain-rule
(define (poly:funcs poly)
  (define elts '())
  (poly:for-each-var
   (lambda (v)
     (if (var:application? v)
	 (set! elts (cons v elts))))
   poly)
  elts)

;;; used only in EXT:ELIM and CHAINABLES
(define (extensions licit)
  (define deps (licit:depends licit))
  (remove-if (lambda (dp) (null? (var:depends dp))) deps))

;;; returns a list of algebraic, differential, and application vars.
;;; used only in TOTAL-DIFFERENTIAL
(define (chainables licit)
  (remove-if var:constant? (licit:depends licit)))

;;; This is for poleqn
;;; Don't simplify a rule with itself
;;; Don't simplify differential rules
(define (alg:simplify p)
  (phases-diag
   'alg:simplify
   (lambda (p)
     (define vars (sort (poly:aexts p) var:>))
     (define exrls (map var:algrule vars))
     (define ans p)
     (for-each (lambda (r v) (set! ans (poly:prem ans r v))) exrls vars)
     ans)
   p))

(define (alg:clear-leading-exts poly)
  (phases-diag
   'alg:clear-leading-exts
   (lambda (poly)
     (define p poly)
     (cond ((number? p) p)
	   (else
	    (let loop ((lc (poly:leading-coeff p (car p))))
	      (define v (poly:find-var-if? lc var:algrule))
	      (cond ((not v) p)
		    (else
		     (set! p (alg:simplify (poly:* p (alg:conjugate lc v))))
		     (cond ((number? p)
			    (math:warn 'wta 'ALG:CLEAR-LEADING-EXTS p)
			    poly)	; give up
			   (else
			    (loop (poly:leading-coeff p (car p)))))))))))
   poly))

;;; This generates conjugates for any algebraic by a wonderful theorem of mine.
;;; 4/30/90 jaffer
(define (alg:conjugate poly extpoly)
  (let* ((var (car extpoly))
	 (poly (poly:promote var poly))
	 (pdiv (if (univ:shorter? poly extpoly)
		   (univ:pdiv extpoly poly)
		   '(1 0)))
	 (pquo (car pdiv))
	 (prem (cadr pdiv)))
    (if (zero? (univ:degree prem var))
	(univ:demote pquo)
	(poly:* (univ:demote pquo) (alg:conjugate prem extpoly)))))
;; (trace ALG:SIMPLIFY ALG:CLEAR-LEADING-EXTS ALG:CONJUGATE POLY:AEXTS)

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
		  (app* (list $ -1 (univ:monomial 1 (- pow) $1)) a)
		  (app* (univ:monomial 1 pow $1) a)))))

(define (rref rvarl idx)
  (let ((var (expl->var rvarl)))
    ;; (set! idx (normalize idx))
    (cond ((not (number? idx)) (deferop _rref rvarl idx))
	  ((negative? idx) (math:error 'rref 'negative-index idx)
	   novalue)
	  ((not (var:recurrence? var))
	   (math:error 'rref 'wta var))
	  ((assv idx (var:instances var)) => cdr)
	  (else
	   (let ((val (canonicalize (rapply (var:recrule var) (list idx)))))
	     (var:set-instances! var (cons (cons idx val) (var:instances var)))
	     val)))))

(define %expt #f)
(define (register-%expt!)
  (cond (%expt)
	(else
	 (set! %expt (cdr (symdef-lookup (string->symbol "%expt") '()))))))

;;; function for handling non-trivial cases.
(define (^ a0 pow0)
  (define a (expr:canonicalize a0))
  (define pow (normalize pow0))
  (cond
   ((eqn? a) (math:error 'expt-of-equation?:- a))
   ((and (not (rat:number? pow)) (not (eqv? 0 a)))
    (register-%expt!)
    (app* %expt a pow))
   (else
    (let ((expnum (num pow))
	  (expdenom (denom pow)))
      (cond
       ((and (eqv? 0 a) (eqv? 0 expnum))
	(math:error 'undefined '(^ 0 0)))
       ((eqv? 0 a) 0)
       ((eqv? 1 expdenom) (ipow a expnum))
       (linkradicals
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
(define (make-radical-exts p r)
  (reduce-init
   poly:* 1 (map (lambda (fact-exp)
		   (cond 
		    ;; ((licit:variable? (car fact-exp))
		    ;;  ;; (register-%expt!)
		    ;;  (app* %expt (car fact-exp) (make-rat (cadr fact-exp) r)))
		    (else
		     (ipow (make-radical-ext (car fact-exp) r)
			   (cadr fact-exp)))))
		 ;; (factors-list->fact-exps (rat:factors-list p))
		 (factors-list->fact-exps (rat:sqfr-factors-list p))
		 )))

;; radical-defs is the list of radical extension defining poleqns
(define (make-radical-ext p r)
  (set! p (licit->polxpr p))
  (let ((e (member-if (lambda (e) (math:equal? p (cadr e))) radical-defs)))
    (cond (e (if (divides? r (length (cddr (car e))))
		 (radpow (car e) r)
		 (var->expl (make-rad-var p r))))
	  (else (var->expl (make-rad-var p r))))))

(define (radpow radrule r)
  (univ:monomial 1 (quotient (length (cddr radrule)) r) (car radrule)))
