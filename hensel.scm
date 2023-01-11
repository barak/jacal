;; "hensel.scm" Multivariate Hensel Lifting.	-*-scheme-*-
;; Copyright 1994, 1995 Mike Thomas
;; Copyright 1996, 1997, 1998, 1999, 2002, 2009, 2020 Aubrey Jaffer
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

(require 'univariate-hensel)
(require 'random)
(require 'factor)
(require 'combinatorics)
(require 'common-list-functions)
(require 'rev4-optional-procedures)

;;; Same but multivariate, uses poly:modularize in poly.scm and using
;;; sym:sym instead of modulo.
(define (ff:mnorm modulus poly)
  (poly:modularize (symmetric:modulus modulus) poly))

;;; m-th Taylor series coefficient in expansion about var = n
(define (taylor-coefficient poly var n m)
  ;;(if (negative? m) (math:error 'taylor-coefficient 'of 'negative 'index m))
  (poly:/ (poly:evaln
	   (let loop ((i 1) (fn poly))
	     (if (> i m) fn (loop (+ 1 i) (poly:diff fn var))))
	   var n)
	  (factorial m)))

;;; Substitute an updated leading coefficient into each member of u.
;;; The member of the correct leading coefficients in lcu corresponding
;;; to a factor in u, is reduced mod the ideal id.
(define (lcsubst u lcu id pl)
  (map (lambda (x y)
	 (set! x (append (butlast x 1) (list y)))
	 (let loop ((i id) (y x))
	   (if (null? i)
	       (number->univ y (car x))
	       (loop (cdr i)
		     (ff:mnorm pl
			       (poly:evaln y (caar i) (cadar i)))))))
       u
       lcu))
	
(define (poly:largest-coeff poly)
  (define mc 0)
  (define (plc p)
    (if (number? p)
	(set! mc (max mc (abs p)))
	(for-each plc (cdr p))))
  (plc poly)
  mc)

;;;; GCL Algorithm 6.4 (p. 272)
;;;
;;; a: A mutivariate polynomial a(x_1, ..., x_v) element Z[x_1, ..., x_v]
;;;    which is primitive as a polynomial in the special variable x_1.
;;;
;;; I: a list of equations [x_2 = al_2, ..., x_v = al_v]
;;;    representing the evaluation homomorphism used;
;;;    lcoeff(a, x_1) /= 0 (mod I).
;;;
;;; p: prime integer which does not divide lcoeff(a mod I).
;;;
;;; l: a positive integer such that (p^l)/2 bounds the magnitudes of all
;;;    integers appearing in a and in any of its factors to be computed.
;;;
;;; u: a list of n>1 univariate polynomials in Z_p^l[x_1] which are
;;;    pairwise relatively prime in the Euclidean domain Z_p[x_1],
;;;    such that a = u_1 * ... * u_n (mod <I, p^l>).
;;;
;;; lcU: a list of the n correct multivariate leading coefficients
;;;      corresponding to the univariate factors u.

;;; MJT: Assumes that the leading coefficient of a given multivariate
;;; polynomial is the last one.
(define (hen:multivariate-hensel a x_1 I p l u lcU)
  (define nu-1 (length I))
  (define p^l   (expt p l))
  (define a-mod-I (poly:eval-ideal a I))
  (define lc-a-mod-I (univ:lc a-mod-I))
  (define abnd (poly:largest-coeff a))
  (define a* (ff:unorm p^l a-mod-I))
  (define u_i*
    (ff:unorm
     p^l
     (reduce-init poly:*
		  1
		  (map (lambda (u_i)
			 (ff:unorm p^l (poly:eval-ideal u_i I)))
		       u))))
  ;;(apply math:print 'u '==> u)
  ;;(math:print 'hen:multivariate-hensel x_1 (poly:eval-ideal a I))
  (cond
   ((not (poly:primitive? a x_1))
    (math:print 'polynomial-not-primitive-in-special-variable x_1 a) '()) ; 3
   ((zero? lc-a-mod-I)
    (math:print 'lc-0-ideal a) '())
   ((divides? p lc-a-mod-I)
    (math:print 'p-divides-lc-ideal p lc-a-mod-I) '())
   ((< (quotient (expt p l) 2) abnd)
    (math:print 'p^l/2<a-coeff-max (quotient (expt p l) 2) '< abnd) '())
   ((<= (length u) 1)
    (math:print 'u-too-short u) '())
   ((not (pairwise-relatively-prime? u p x_1))
    (math:print 'not-pairwise-relatively-prime-in x_1 u p) '())
   ((not (equal? a* u_i*))
    ;; many occurrences; all are negations.
    (math:print 'u-product-mod-p^l-ideal-not-equal-to-a u_i* a*) '())
   (else
    (let ((nu (+ nu-1 1)))
      (define av (make-vector nu a))	;Partly substituted targets
      (define iv (make-vector nu '())) ;Partial ideals for leading coefficients
      (do ((j (+ -2 nu) (+ -1 j)))
	  ((negative? j))
	(let ((xj     (car (list-ref I j)))
	      (alphaj (cadr (list-ref I j))))
	  (vector-set! iv j (list-tail I j))
	  (vector-set! av j (poly:evaln (vector-ref av (+ 1 j)) xj alphaj))))
      (let ((maxdeg (apply max (map (lambda (x) (poly:degree a x_1)) I)))
	    (bU u)
	    (n (length u)))
	(let loop0 ((j 0))
	  (define bU* (reduce-init poly:* 1 bU))
	  (cond
	   ((>= j nu-1) (if (equal? a bU*) bU #f))
	   (else
	    (let ((bU1 bU)
		  (monomial 1)
		  (xj     (car (list-ref I j)))
		  (alphaj (cadr (list-ref I j))))
	      (define ij (list xj (- alphaj) 1))
	      (set! bU (lcsubst bU lcU (vector-ref iv (+ 1 j)) p^l))
	      (let loop1 ((k 1)
			  (monomial ij)
			  (e (poly:- (vector-ref av (+ 1 j)) (reduce poly:* bU))))
		(cond
		 ((and (not (eqv? 0 e))
		       (<= k (poly:degree (vector-ref av (+ 1 j)) xj)))
		  (let ((c (taylor-coefficient e xj alphaj k)))
		    (cond
		     ((poly:0? c) (loop1 (+ 1 k) (poly:* monomial ij) e))
		     (else
		      (let ((du1 (hen:multivariate-diophant
				  bU1 (number->univ c x_1)
				  (butlast I (- nu-1 j)) maxdeg p l x_1)))
			(cond
			 ((null? du1) '())
			 (else
			  (let ((du (map (lambda (x)
					   (poly:* x monomial)) du1)))
			    (set! bU (map (lambda (x y)
					    (ff:mnorm p^l (poly:+ x y)))
					  bU du))
			    (loop1 (+ 1 k)
				   (poly:* monomial ij)
				   (ff:mnorm
				    p^l (poly:- (vector-ref av (+ 1 j))
						(reduce poly:* bU))))))))))))
		 (else (loop0 (+ 1 j))))))))))))))

;;;; GCL Algorithm 6.2 (p. 268) partly tested by MJT
;;;
;;; Solve in the domain Z_p^k[x_1, ..., x_v] the multiariate polynomial
;;; diophantine equation:
;;;    S_1 * b_1 + ... + S_r * b_r = c (mod <I^d+1, p^k>).
;;; where, in terms of the given list of polynomials a_1, ..., a_r,
;;; the polynomials b_i, i = 1, ..., r, are defined by:
;;;   b_i = a_1 * ... * a_i-1 * a_i+1 * ... a_r.
;;; The unique solution S_1, ..., S_r, will be computed such that
;;;   deg(S_i, x_1) < deg(a_i, x_1).
;;;
;;; CONDITIONS:
;;;   mod using symmetric representation.
;;;
;;;   p must not divide lcoeff(a_i mod I), i = 1, ..., r;
;;;
;;;   a_i mod <I, p>, i = 1, ..., r, must be pairwise relatively prime
;;;   in Z_p[x_1]; and
;;;
;;;   degree(c, x_1) < sum(degree(a_i, x_1), i = 1, ..., r.
;;;
;;; INPUTS:
;;; a: A list of r > 1 polynomials in the domain Z_p^k[x_1, ..., x_v]
;;;
;;; c: A polynomial element of Z[x_1, ..., x_v]
;;;
;;; I: a list of equations [x_2 = al_2, ..., x_v = al_v]
;;;    representing an evaluation homomorphism.
;;;    Empty for univariate problems.
;;;
;;; d: nonnegative integer specifying the maximum total degree
;;;    with respect to x_2, ..., x_v of the desired result.
;;;
;;; p: a prime integer.
;;;
;;; k: a positive integer specifying that the coefficient arithmetic
;;;    is to be performed modulo p^k
;;;
;;; OUTPUT: Returns the list [S_1, ..., S_r]
(define (hen:multivariate-diophant a c I d p k x1)
  (define r (length a))
  (define nu (+ (length I) 1))
  (define p^k (expt p k))
  (cond
   ((>= (poly:degree c x1)
	(apply + (map (lambda (a_i) (poly:degree a_i x1)) a)))
    (math:warn 'c-degree-too-large
	       (poly:degree c x1) '>=
	       (apply + (map (lambda (a_i) (poly:degree a_i x1)) a)))
    '())
   ((not (pairwise-relatively-prime?
	  (map (lambda (a_i)
		 (set! a_i (ff:unorm p (poly:eval-ideal a_i I)))
		 (cond ((zero? (modulo (univ:lc a_i) p))
			(math:error 'hen:multivariate-diophant 'lc-0-divider-ideal a_i)))
		 a_i)
	       a)
	  p
	  x1))
    (math:warn 'not-relatively-prime 'mod p) '())
   ((> nu 1)
    (let* ((ap (reduce-init poly:* 1 a))
	   (b (map (lambda (x) (poly:/ ap x)) a))
	   (xnu (car (list-ref I (- nu 2))))
	   (alphanu (cadr (list-ref I (- nu 2))))
	   (anew (map (lambda (x)
			(number->univ (poly:evaln x xnu alphanu) (car x)))
		      a))
	   (cnew (poly:evaln c xnu alphanu))
	   (inew (butlast I 1))		;(remove (list xnu alphanu) I)
	   (monomial 1)
	   (sigma (hen:multivariate-diophant anew cnew inew d p k x1))
	   (e (if (null? sigma)
		  0
		  (ff:mnorm
		   p^k (poly:- c (reduce-init poly:+ 0 (map poly:* sigma b)))))))
      (if (null? sigma)
	  '()
	  (let loop ((m 1))
	    (cond
	     ((and (<= m d) (not (poly:0? e)))
	      (set! monomial (poly:* monomial (poly:- (var->expl xnu) alphanu)))
	      (let ((cm (taylor-coefficient e xnu alphanu m)))
		(if (not (poly:0? cm))
		    (let ((ds (hen:multivariate-diophant anew cm inew d p k x1)))
		      (set! ds (map (lambda (x1) (poly:* x1 monomial)) ds))
		      (set! sigma (map poly:+ sigma ds))
		      (set! e (ff:mnorm
			       p^k (poly:- e (reduce-init
					      poly:+ 0 (map poly:* ds b))))))))
	      (loop (+ 1 m)))
	     (else
	      (map (lambda (x) (ff:mnorm p^k x)) sigma)))))))
   (else
    (let* ((sigma (make-list r 0))
	   (c1 (number->univ c x1)) ;So we can deconstruct consistently
	   (dc (poly:degree c1 x1)))
      (let loop ((m 0))
	(if (<= m dc)
	    (let* ((cm (poly:leading-coeff (list-ref c1 (+ 1 m)) x1))
;;; IF cm = 0 then we don't need to do the diophantine solution.
		   (ds (hen:univariate-diophant a x1 m p k)))
	      (if (null? (check-hd1 a x1 m p k ds))
		  (math:error 'hen:multivariate-diophant 'check-hd1))
	      (set! ds (map (lambda (x) (poly:* x cm)) ds))
	      (set! sigma (map (lambda (x y)
				 (number->univ (poly:+ x y) x1))
			       sigma ds))
	      (loop (+ 1 m)))))
      (map (lambda (x) (ff:unorm p^k x)) sigma)))))

;;; ppoly is primitive polynomial
(define (map-svpf-factors ppoly spec-var)
  (let loop ((ins (yuniv:square-free-factorization ppoly spec-var))
	     (outs '()))
    (define pfszpp (cond ((null? ins) #f)
			 ((number? (caar ins)) (caar ins))
			 (else (poly:factorszpp (caar ins)))))
    (cond ((not pfszpp) outs)
	  ;; ((eqv? 0 pfszpp) '())
	  (else (loop (cdr ins)
		      (cons (cons pfszpp (cdar ins)) outs))))))

;;; Factorise multivariate polynomial over Z
(define (poly:factorz poly)
  (define vars (sort (poly:vars poly) var:>))
  (define nvars (length vars))
  (case nvars
    ((0) (list (list poly 1)))		; number
    ((1) (prepend-integer-factor	; univariate
	  (* (u:unitz poly) (univ:cont poly))
	  (let ((primz (u:primz poly)))
	    (map (lambda (x) (list (u:factorsz (car x)) (cadr x)))
		 (yuniv:square-free-factorization primz (car primz))))))
    (else				; multivariate
     (let varloop ((vloopcnt 0))
       (cond (math:trace
	      (display-diag 'factoring:) (newline-diag)
	      (math:write (poleqns->licits poly) *output-grammar*)))
       (let* ((spec-var (list-ref vars (modulo vloopcnt nvars)))
	      (spec-var-poly (var->expl spec-var))
	      (psign (poly:unitz poly spec-var))
	      (pcont (unitcan (univ:cont (promote spec-var poly))))
	      (ppoly1 (unitcan (poly:/ poly pcont)))
	      (spec-var-pf (svpf ppoly1))
	      (ppoly (poly:/ ppoly1 (univ:monomial 1 spec-var-pf spec-var))) ;(poly:^ spec-var-poly spec-var-pf)
	      (ppolyfactors
	       (cond ((zero? spec-var-pf)
		      (map-svpf-factors ppoly spec-var))
		     ((number? ppoly)
		      (list (list (list spec-var-poly) spec-var-pf)))
		     (else
		      (cons (list (list spec-var-poly) spec-var-pf)
			    (map-svpf-factors ppoly spec-var)))))
	      (facts
	       (cond ;; ((null? ppolyfactors) '())
		     ((number? pcont)
		      (prepend-integer-factor (* pcont psign) ppolyfactors))
		     (else
		      ;; (math:print 'prepend-integer-factor pcont)
		      (prepend-integer-factor
		       psign (append (poly:factorz pcont) ppolyfactors))))))
	 (cond (math:trace
		(display-diag 'yielding-factors:) (newline-diag)
		(math:print facts)
		(math:write facts *output-grammar*)))
	 (cond ((not (null? ppolyfactors)) facts)
	       ((>= vloopcnt nvars)
		(math:warn 'could-not-factor poly) '())
	       (else (varloop (+ vloopcnt 1)))))))))


(define (unlucky? maxdeg poly1 spec-var)
;;; (display-diag 'unlucky?:) (newline-diag) (math:write poly1 *output-grammar*)
  (or (not (= maxdeg (poly:degree poly1 spec-var)))
      (not (eqv? 1 (poly:gcd (poly:diff poly1 spec-var) poly1)))))

;;; Make a random ideal for the list of variables ivars with the
;;; random numbers > 1 and <= r.  Each variable must have a unique
;;; value to assist in sorting out leading coefficients.
;;; r > (length ivars) + (length nums)
;;; nums is a list of numbers which cannot appear in the ideal.
;;; The random number must be greater than one to enable the recognition of
;;; multiple factors in the leading coefficient.
(define ideal:prngs
  (make-random-state "repeatable seed for making random ideals"))
(define (make-random-ideal ivars nums r)
  (define (mri ivars sofar nums)
    (if (null? ivars)
	sofar
	(let ((rn (let loop ((rn1 (+ 2 (random (- r 1) ideal:prngs))))
		    (if (member rn1 nums)
			(loop (+ 2 (random (- r 1) ideal:prngs)))
			rn1))))
	  (mri (cdr ivars) (cons rn sofar) (cons rn nums)))))
;;; (display-diag 'make-random-ideal) (display-diag nums) (newline-diag)
;;; (for-each (lambda (var) (math:write (var->expl var) *output-grammar*)) ivars)
  (if (> (+ (length ivars) (length nums)) r)
      (math:error 'make-random-ideal 'r-too-small ivars nums r))
  (map (lambda (i n) (list i n -1)) ivars (mri ivars '() nums)))

;;; Evaluate a polynomial using Horner's rule, substituting number n
;;; for the variable var
(define (poly:evaln poly var n)
  (define d (poly:degree poly var))
  (let loop ((i d)
	     (acc (poly:coeff poly var d)))
    (if (< i 1)
	acc
	(loop (- i 1)
	      (poly:+ (poly:* acc n) (poly:coeff poly var (- i 1)))))))

;;; evaluate a polynomial poly mod the ideal i
;;; (An evaluation homomorphism)
(define (poly:eval-ideal poly i)
  (if (null? i)
      poly
      (poly:eval-ideal (poly:evaln poly (caar i) (cadar i))
		       (cdr i))))

;;; Generate the list of correct leading coefficients
;;;
;;; uniqs - association list of a unique prime factor for each leading
;;;   coefficient of a polynomial's factors, evaluated over some ideal
;;;   and their corresponding unevaluated multivariate factors.
;;;
;;; imagelcs - factors of the leading coefficient of the univariate
;;;   image of the polynomial, evaluated over the same ideal.
(define (correct-lcs uniqs imagelcs lc)
  (define lun (length uniqs))
  (map (lambda (imagelc)
	 (define res 1)
	 (let loop ((i 0))
	   (cond ((and (not (unit? lc)) (< i lun))
		  (let* ((current (list-ref uniqs i))
			 (unique-prime (car current))
			 (fact (cadr current)))
		    (let loop1 ()
		      (cond ((and (not (unit? unique-prime))
				  (not (unit? fact))
				  (divides? unique-prime imagelc)
				  (poly:/? lc fact))
			     (set! res (poly:* fact res))
			     (set! imagelc (quotient imagelc unique-prime))
			     (set! lc (poly:/ lc fact))
			     (loop1))
			    (else (loop (+ 1 i)))))))
		 (else res))))
       imagelcs))

;;; Strip a list of factors and powers of the powers
(define (strip-powers l acc)
  (if (null? l) acc (strip-powers (cdr l) (append (caar l) acc))))

;;; Factorise square-free primitive multivariate polynomial over Z (sorted)
(define (poly:factorszpp ppoly)
  (define vars (sort (poly:vars ppoly) var:>))
  (define nvars (length vars))
  (let varloop ((vloopcnt 0))
    (let* ((spec-var (car vars)) ;(list-ref vars (modulo vloopcnt nvars))
	   (ivars (remove spec-var vars))
	   (lcpoly (poly:leading-coeff ppoly spec-var))
	   (lcfactors (if (number? lcpoly)
			  (remove-duplicates (factor lcpoly))
			  (strip-powers (poly:factorz lcpoly) '())))
	   (lcnums
	    (apply append (map (lambda (x) (if (number? x) (list x) '()))
			       lcfactors))))
      (define maxdeg (poly:degree ppoly spec-var))
      (define nlcnums (length lcnums))
      ;;(math:print 'spec-var spec-var 'in ppoly)
      (let loopi ((mr+1 (+ nvars nlcnums 1))
		  (random-ideal (make-random-ideal
				 ivars lcnums (+ nvars nlcnums 1))))
	(let ((elcfactors
	       (map (lambda (x)
		      (remove-duplicates
		       (factor (poly:eval-ideal x random-ideal))))
		    lcfactors)))
	  (define us (uniques elcfactors))
	  (define image (poly:eval-ideal ppoly random-ideal))
;;; (display-diag 'unlucky?:) (newline-diag) (math:write image *output-grammar*)
	  (cond
	   ((or (some null? us) (unlucky? maxdeg image spec-var))
	    (cond ((< mr+1 (+ nvars nlcnums 10)) ;100
		   (loopi (+ 1 mr+1)
			  (make-random-ideal ivars lcnums (+ 2 mr+1))))
		  ((>= vloopcnt nvars)	;30
		   (math:warn 'poly:factorszpp 'giving-up-after vloopcnt 'rounds)
		   (list ppoly))
		  (math:debug
		   (math:warn 'poly:factorszpp
			      (- mr+1 (+ nvars nlcnums))
			      'tries-on
			      lcnums
			      (var:sexp spec-var)
			      (if (some null? us) 'null-us 'unlucky))
		   (varloop (+ 1 vloopcnt)))
		  (else (varloop (+ 1 vloopcnt)))))
	   (else
	    (let ((slpoly (u:unitz image))
		  (cpoly (univ:cont image))
		  (factors (poly:sort-factors (u:factorsz image))))
	      (define image-factors
		(if (and (one? cpoly) (one? slpoly))
		    factors
		    (cons (list spec-var (* slpoly cpoly)) factors)))
	      (if (one? (length image-factors))
		  (list ppoly)
		  (let ((p (let loopp ((cp 3))
			     (cond ((and (= maxdeg (ff:degree cp image))
					 (every (lambda (x) (eqv? 1 x))
						(map cadr (ff:sff cp image))))
				    cp)
				   ((> cp 1000)
				    (math:error 'poly:factorszpp
						'tried-all-primes-up-to
						cp))
				   (else (loopp (car (primes> (+ 1 cp) 1))))))))
		    (define res (hen:multivariate-hensel
				 ppoly
				 spec-var
				 random-ideal
				 p
				 (u:prime-power image p)
				 image-factors
				 (correct-lcs
				  (if (eqv? 1 lcpoly)
				      (map (lambda (x) '(1 1)) lcfactors)
				      (map list (map car us) lcfactors)) ; uniqs
				  (map (lambda (p) (univ:lc p)) image-factors)
				  lcpoly)))
		    (cond ((not (null? res)) res)
			  ((>= vloopcnt nvars) ;30
			   (math:warn 'poly:factorszpp 'too-many-tries vloopcnt) '())
			  (else
			   (varloop (+ 1 vloopcnt))))))))))))))

;;(require 'debug-jacal) (trace hen:diophantine hen:eea-lift hen:univariate-diophant hen:multivariate-diophant lcsubst hen:multivariate-hensel correct-lcs)
;;(require 'debug-jacal) (trace-all "hensel.scm")
