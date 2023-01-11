;; "anti-diff.scm" rational-function anti-derivative.	-*-scheme-*-
;; Copyright 2020, 2023 Aubrey Jaffer
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

;; returns a list of unsquared factors of increasing power.
(define (sqfr-splits c var)
  (define splitter (poly:diff c var))
  (cond ((not (number? splitter))
	 (let ((d '())
	       (aj '())
	       (bi (poly:gcd c splitter)))
	   (do ((b bi (poly:/ b d))
		(a (poly:/ c bi) d))
	       ((number? b)
		(reverse (cons a aj)))
	     (set! d (poly:gcd a b))
	     (set! aj (cons (poly:/ a d) aj)))))
	((number? c) (list c))
	(else (list (univ:demote c)))))

;; The inverse of sqfr-splits.  Returns the product of increasing
;; powers of list of factors fcts.
(define (powfacts fcts)
  (let lp ((fcts fcts) (n 1) (acc 1))
    (cond ((null? fcts) acc)
	  (else
	   (lp (cdr fcts)
	       (+ 1 n)
	       (poly:* (ipow-by-squaring (car fcts) n 1 poly:*)
		       acc))))))

(define (rat:integrate dnmf L var ver)
  (define dY (normalize (diff (var->expl ver) var)))
  (define Q (denom dY))
  (define Qxd (poly:degree Q var))
  (define N (reduce-init poly:* 1 dnmf))
  (define M
    (let ((M 0) (j 0))
      (if math:debug (math:print 'dnmf= dnmf))
      (for-each (lambda (p)
		  (set! M (poly:+ (poly:*
				   (reduce-init poly:* 1 (butnth j dnmf))
				   (poly:* (- j) (poly:diff p var)))
				  M))
		  (set! j (+ 1 j)))
		dnmf)
      M))
  (let ((A 0) (R L) (Nyd (poly:degree N ver)))
    (define NyC (poly:coeff N ver Nyd))
    (define Nxd (poly:degree NyC var))
    (define (fnl) (app* $1/$2 A (powfacts (cdr dnmf))))
    (define (fail . args)
      (math:warn 'could-not-find-algebraic-anti-derivative)
      (apply math:print args)
      novalue)
    (if math:debug (math:print 'M= M 'N= N 'NyC= NyC 'Nxd= Nxd))
    (let lp ((Ryd (poly:degree (num R) ver)))
      (define RyC (poly:coeff (num R) ver Ryd))
      (define Rxd (poly:degree RyC var))
      (define g (+ 1 (- Rxd Nxd)))
      (define h (- Ryd Nyd))
      (define T
	(cond ((negative? g) (math:warn 'g= g '<0) 1)
	      ((number? Q) (univ:monomial 1 g var))
	      ((<= Qxd g)
	       (poly:* (ipow-by-squaring Q (quotient g Qxd) 1 poly:*)
		       (poly:* (univ:monomial 1 (remainder g Qxd) var)
			       (univ:monomial 1 h ver))))
	      (else (poly:* (univ:monomial 1 g var)
			    (univ:monomial 1 h ver)))))
      (define dT (normalize (diff T var)))
      (define B (normalize (app* $1*$2+$3 N dT (poly:* M T))))
      (define C (expr:normalize
		 (app* $1/$2
		       (poly:* (denom B) (poly:coeff RyC var Rxd))
		       (poly:* (poly:coeff (poly:coeff (num B) ver Ryd) var Rxd)
			       (denom R)))))
      (cond (math:debug
	     (math:print 'Ryd= Ryd 'Rxd= Rxd
			 'Byd= (poly:degree (num B) ver)
			 'ByC= (poly:coeff (num B) ver (poly:degree (num B) ver))
			 'Bxd= (poly:degree (poly:coeff (num B) ver (poly:degree (num B) ver)) var)
			 'Qxd= Qxd 'g= g 'h= h)
	     (math:print 'R= R)
	     (math:print 'T= T)
	     (math:print 'dT= dT)
	     (math:print 'B= B)
	     (math:print 'C= C)))
      (set! A (expr:normalize (app* $1*$2+$3 C T A)))
      (set! R (expr:normalize (app* $1-$2*$3 R C B)))
      (cond ((and math:debug (number? R) (not (zero? R)))
	     (math:print 'nonzero-number-R= R)))
      (cond ((if (number? R) (zero? R) (univ:zero? R))
	     (fnl))
	    ;; ((number? R) (math:warn 'non-zero-constant-part R) (fnl))
	    ((> (poly:degree (num R) ver) Ryd)
	     (fail 'increasing-Ryd (poly:degree (num R) ver) 'vs Ryd))
	    ((and (= (poly:degree (num R) ver) Ryd)
		  (>= (poly:degree (poly:coeff (num R) ver (poly:degree (num R) ver)) var)
		      Rxd))
	     (fail 'non-decreasing-Rxd
		   (poly:degree (poly:coeff (num R) ver (poly:degree (num R) ver)) var)
		   'vs Rxd))
	    (else (lp (poly:degree (num R) ver)))))))

(define (poly:cont2 ve v p)
  (apply poly:gcd*
	 (map (lambda (c) (univ:cont (promote v c)))
	      (cdr (promote ve p)))))

(define (independent-of-var? poly var)
  (and (zero? (poly:degree poly var))
       (null? (poly:find-var-exts poly var))))

(define (indef-integrate p v)
  (define nm (num p))
  (define dnm (denom p))
  (define verlst (poly:find-var-exts nm v))
  (define ver (if (null? verlst) _$ (car verlst)))
  (define cnm (poly:cont2 ver v nm))
  (define cdnm (univ:cont (promote v dnm)))
  (define nm/cnm (poly:/ nm cnm))
  (define dnm/cdnm (poly:/ dnm cdnm))
  (cond ((> (length verlst) 1)
	 (math:warn 'too-many-extensions-involving v ': verlst)
	 novalue)
	(else
	 (let ((ans (normalize
		     (app* $1*$2
			   (rat:integrate (sqfr-splits dnm/cdnm v) nm/cnm v ver)
			   (app* $1/$2 cnm cdnm)))))
	   (let ((chk (expr:normalize (diff ans v))))
	     (cond ((novalue? ans) ans)
		   ((independent-of-var? (normalize (app* $1-$2 chk p)) v)
		    ans)
		   ((independent-of-var? (normalize (app* $1+$2 chk p)) v)
		    (if math:debug (math:warn 'integration-was-negated))
		    (app* _-$1 ans))
		   (else
		    (math:warn 'diff-of-integral-mismatch chk)
		    (math:print 'mr-diff-of-integral-mismatch p)
		    novalue)))))))

(define (integrate . args)
  (if (not (<= 2 (length args) 4)) (bltn:error 'integrate 'wna args))
  (let ((expr (normalize (car args)))
	(var (expl->var (cadr args)))
	(lo (if (null? (cddr args)) #f (caddr args)))
	(hi (and (= 4 (length args)) (cadddr args))))
    (cond ((= 2 (length args))
	   (indef-integrate expr var))
	  (else
	   (let ((sexp (sexp:alpha-convert (list (var:sexp var))
					   (cano->sexp expr horner))))
	     (define ifun (indef-integrate (sexp->math sexp) $1))
	     (cond ((novalue? ifun) ifun)
		   ((case (length args)
		      ((3) (app* ifun lo))
		      ((4) (app* $1-$2 (app* ifun hi) (app* ifun lo)))))))))))

(defbltn 'integrate 2 4 integrate)
