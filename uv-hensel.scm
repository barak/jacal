;; "uv-hensel.scm" Univariate Hensel Lifting.	-*-scheme-*-
;; Copyright 1994, 1995 Mike Thomas
;; Copyright 2002, 2020 Aubrey Jaffer
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

(require 'fluid-let)
(require 'polynomial-factors)
(require 'math-integer)
(require 'common-list-functions)
(require 'combinatorics)		; for memon

;;;; GCL Algorithm 2.2 (p. 36)
;;;
;;; Extended Euclidean algorithm returns the list (g s t),
;;; where g is the gcd of a and b, and g = s*a + t*b.
;;; for univariate polynomials over Z mod n:
(define (ff:eea-gcd a b mdls)
  (define var (car a))
  (fluid-let ((*modulus* (symmetric:modulus mdls)))
    (letrec ((feg (lambda (c c1 c2 d d1 d2)
		    (if (univ:zero? d)
			(list c c1 c2)
			(let ((qr (ff:p/p->qr mdls c d)))
			  (feg d
			       d1
			       d2
			       (cadr qr)
			       (number->univ (poly:- c1 (univ:* (car qr) d1))
					     var)
			       (number->univ (poly:- c2 (univ:* (car qr) d2))
					     var)))))))
      (let* ((aone (list var 1))
	     (azero (list var 0))
	     (c-c1-c2 (feg (univ:make-monic a) aone azero
			   (univ:make-monic b) azero aone))
	     (c (car c-c1-c2)))
	(cons
	 (univ:make-monic c)
	 (list
	  (poly:* (cadr c-c1-c2)
		  (coef:invert (* (ff:lc mdls a) (ff:lc mdls c))))
	  (poly:* (caddr c-c1-c2)
		  (coef:invert (* (ff:lc mdls b) (ff:lc mdls c))))))))))

(define (check-ext-gcd caller a s b t p)
  (define x (car a))
  (set! s (ff:unorm p s))
  (set! t (ff:unorm p t))
  (cond
   ((not (< (univ:deg s) (univ:deg b)))
    (math:warn "(>= (univ:deg s) (univ:deg b))" s b))
   ((not (< (univ:deg t) (univ:deg a)))
    (math:warn "(>= (univ:deg t) (univ:deg a))" t a)))
  (fluid-let ((*modulus* (symmetric:modulus p)))
    (let ((inner-prod (ff:unorm
		       p
		       (number->univ (poly:+ (poly:* a s) (poly:* b t))
				     x))))
      (cond ((not (univ:one? inner-prod x))
	     (math:warn caller "result is not correct.")
	     (math:warn " a = " a)
	     (math:warn " b = " b)
	     (math:warn " s = " s)
	     (math:warn " t = " t)
	     (math:warn " inner-prod = " inner-prod)
	     (math:error " should be = " (list x 1)))))))

;;;; GCL Algorithm 6.3 (p. 271)
;;;
;;; hen:eea-lift computes s, t such that s * a + t * b = 1 (mod p^k),
;;; with deg(s) < deg(b), deg(t) < deg(a).
;;; Assumption: GCD(a mod p, b mod p) = 1 in Z_p[x],
;;; a and b must have the same variable and be univariate.
;;;
;;; This is the extended euclidean gcd algorithm (mod p), but altered to
;;; lift its results to (mod p^k).  Arguments a and b are assumed to be
;;; relatively prime.
(define hen:eea-lift
  (memon
   (lambda (a b p k)
     (define p^k (expt p k))
     ;; (set! a (univ:demote a))
     ;; (set! b (univ:demote b))
     (if (not (equal? (car a) (car b)))
	 (math:error 'hen:eea-lift "polys not in the same variable." a b))
     (let* ((x (car a))
	    (amodp (ff:unorm p a))
	    (bmodp (ff:unorm p b))
	    (g-s-t (ff:eea-gcd amodp bmodp p)) ;returns list (g s t)
	    (s (cadr g-s-t))
	    (t (caddr g-s-t))
	    (smodp s)
	    (tmodp t)
	    (modulus p))
       (cond ((not (univ:one? (car g-s-t) x))
	      (math:error 'ff:eea-gcd 'returned (car g-s-t))))
       (check-ext-gcd 'ff:eea-gcd a s b t p)
       (do ((j 1 (+ 1 j)))
	   ((>= j k)
	    (check-ext-gcd 'hen:eea-lift amodp s bmodp t p)
	    (list s t))
	 (fluid-let ((*modulus* (symmetric:modulus p^k)))
	   (let* ((e  (number->univ
		       (poly:- (list x 1) (poly:+ (poly:* s a) (poly:* t b)))
		       x))
		  (c  (univ/scalar e modulus))
		  (sb (number->univ (poly:* smodp c) x))
		  (tb (number->univ (poly:* tmodp c) x))
		  (qr (ff:p/p->qr p sb bmodp))
		  (q  (car qr))
		  (si (cadr qr))
		  (ta (poly:+ tb (poly:* q amodp))))
	     (set! s (number->univ (poly:+ s (poly:* si modulus)) x))
	     (set! t (number->univ (poly:+ t (poly:* ta modulus)) x))
	     (set! modulus (* modulus p)))))))))

(define (check-mel as p k result)
  (define p^k (expt p k))
  (fluid-let ((*modulus* (symmetric:modulus p^k)))
    (let* ((la (length as))
	   (x (car (car as)))
	   (bs (make-vector la 0)))
      (let loop ((i 0))
	(cond ((< i la)
	       (let loop1 ((j 0)
			   (acc (list x 1)))
		 (cond ((< j la)
			(if (not (= i j))
			    (loop1 (+ 1 j) (poly:* acc (list-ref as j)))
			    (loop1 (+ 1 j) acc)))
		       (else (vector-set! bs i acc))))
	       (loop (+ 1 i)))))
      (set! bs (vector->list bs))
      (cond ((null? result) (math:print 'check-mel 'null 'result 'argument))
	    (else
	     (let ((inner-prod
		    (number->univ (reduce poly:+ (map poly:* bs result)) x)))
	       (cond ((not (univ:one? inner-prod x))
		      (math:print "In hen:multiterm-eea-lift: the result is not correct.")
		      (math:print " as = " as " bs = " bs " result = " result)
		      (math:print " inner-prod = " inner-prod " p^k = " p^k)))))))))

;;;; GCL Algorithm 6.3 (p. 271)
;;;
;;; Compute s_1, ..., s_r such that
;;;   S_1 * b_1 + ... + S_r * b_r = x^m (mod p^k)
;;; with deg(s_j) < deg(a_j) where, in terms of the given list of
;;; polynomials a_1, ..., a_r, the polynomials B_i are defined by:
;;;   b_i = a_1 * ... a_i-1 * a_i+1 * ... * a_r, i = 1, ..., r
;;;
;;; Conditions:
;;;   p must not divide lcoeff(a_i), i = 1, ..., r;
;;;   a_i mod p, i = 1, ..., r, must be pairwise relatively prime in Z_p[x].
(define hen:multiterm-eea-lift
  (memon (lambda (a p k)
	   ;;(math:print 'hen:multiterm-eea-lift a p k)
	   (let* ((r (length a))
		  (var (car (car a)))
		  (r-1 (+ -1 r))
		  (r-2 (+ -2 r))
		  (s (make-vector r (list var 0)))
		  (q (make-vector r-1 (list var 0)))
		  (beta (make-vector r (list var 1))))
	     (vector-set! q r-2 (list-ref a r-1))
	     (let loop ((j (- r-2 1)))
	       (cond ((>= j 0)
		      (vector-set! q j (poly:* (list-ref a (+ 1 j))
					       (vector-ref q (+ 1 j))))
		      (loop (+ -1 j)))))
	     (let loop ((j 0))
	       (if (< j r-1)
		   (let ((sigma (hen:diophantine
				 (list (vector-ref q j) (list-ref a j))
				 (vector-ref beta j) p k)))
		     (vector-set! beta (+ 1 j) (car sigma)) ;book says j
		     (vector-set! s j  (cadr sigma))
		     (loop (+ 1 j)))))
	     (vector-set! s r-1 (vector-ref beta r-1))
	     (check-mel a p k (vector->list s))
	     (vector->list s)))))

(define (check-hd as c p k result)
  (define la (length as))
  (define p^k (expt p k))
  (define bs (make-vector la 0))
  (define x (car c))
  (fluid-let ((*modulus* (symmetric:modulus p^k)))
    (let loop ((i 0))
      (cond ((< i la)
	     (let loop1 ((j 0)
			 (acc (list x 1)))
	       (cond ((< j la)
		      (if (not (= i j))
			  (loop1 (+ 1 j) (poly:* acc (list-ref as j)))
			  (loop1 (+ 1 j) acc)))
		     (else (vector-set! bs i acc))))
	     (loop (+ 1 i)))))
    (set! bs (vector->list bs))
    (let ((inner-prod
	   (ff:unorm
	    p^k (number->univ (reduce poly:+ (map poly:* bs result)) x)))
	  (cn (ff:unorm p^k c)))
      (cond ((equal? cn inner-prod) result)
	    (else
	     (math:print 'check-hd 'incorrect-result= result 'as= as 'bs= bs
			 'inner-prod= inner-prod 'cn= cn 'p^k= p^k)
	     result)))))

;;; memoised diophantine equation solver is much faster
(define hen:diophantine
  (memon (lambda (a c p k)
	   (let* ((r     (length a))
		  (p^k   (expt p k))
		  (x1    (car c))
		  (dc    (univ:deg c))
		  (sigma (make-list r 0)))
	     (let loop ((m 1))
	       (if (<= (+ -1 m) dc)
		   (let* ((cm (list-ref c m))
			  (ds (hen:univariate-diophant a x1 (+ -1 m) p k)))
		     (set! ds (map (lambda (x) (poly:* x cm)) ds))
		     (set! sigma (map (lambda (x y)
					(number->univ (poly:+ x y) x1))
				      sigma ds))
		     (loop (+ 1 m)))
		   (let ((result (map (lambda (x) (ff:unorm p^k x)) sigma)))
		     (check-hd a c p k result))))))))

(define (check-hd1 as x m p k result)
  (define la (length as))
  (define p^k (expt p k))
  (define bs (make-vector la 0))
  (fluid-let ((*modulus* (symmetric:modulus p^k)))
    (let loop ((i 0))
      (cond ((< i la)
	     (let loop1 ((j 0)
			 (acc (list x 1)))
	       (cond ((< j la)
		      (loop1 (+ 1 j)
			     (if (= i j)
				 acc
				 (univ:* acc (list-ref as j)))))
		     (else (vector-set! bs i acc))))
	     (loop (+ 1 i)))))
    (set! bs (vector->list bs))
    (let ((inner-prod
	   (ff:unorm
	    p^k (number->univ (reduce poly:+ (map poly:* bs result)) x)))
	  (x^m (number->univ (poly:^ (var->expl x) m) x)))
      (cond ((equal? x^m inner-prod) result)
	    (else
	     (math:print 'check-hd1 'incorrect-result= result 'as= as 'bs= bs
			 'inner-prod= inner-prod 'x^m= x^m 'x= x 'm= m 'p^k= p^k)
	     result)))))

;;;; GCL Algorithm 6.3 (p 270)
;;;
;;; Solve in Z_p^k[x] the univariate polynomial diophantine equation:
;;;    S_1 * b_1 + ... + S_r * b_r = x^m (mod p^k)
;;; where, in terms of the given list of polynomials a_1, ..., a_r,
;;; the polynomials b_i, i = 1, ..., r, are defined by:
;;;   b_i = a_1 * ... * a_i-1 * a_i+1 * ... a_r.
;;; The unique solution S_1, ..., S_r, will be computed such that
;;;   deg(S_i) < deg(a_i).
;;;
;;; Conditions:
;;;   p must not divide lcoeff(a_i), i = 1, ..., r;
;;;   a_i mod p, i = 1, ..., r, must be pairwise relatively prime in Z_p[x].
;;;
;;; The value returned is the list S = [S_1, ..., S_r].
(define hen:univariate-diophant
  (memon
   (lambda (a x m p k)
     (define a_i-mod-p
       (map (lambda (a_i) (ff:unorm p a_i)) a))
     (cond
      ((some (lambda (a_i) (zero? (modulo (univ:lc a_i) p))) a_i-mod-p)
       (math:error 'hen:univariate-diophant 'lc-0-divider a 'mod p)
       '())
      ((not (pairwise-relatively-prime? a_i-mod-p p (caar a)))
       (math:warn 'not-relatively-prime 'mod p) '())
      (else
       (let ((r (length a))
	     (p^k (expt p k))
	     (xm (poly:^ (var->expl x) m)))
	 (fluid-let ((*modulus* (symmetric:modulus p^k)))
	   (if (> r 2)
	       (let ((s (hen:multiterm-eea-lift a p k))
		     (result '()))
		 (do ((j 0 (+ 1 j)))
		     ((>= j r)
		      (check-hd1 a x m p k result))
		   (set! result (append result
					(cdr (ff:p/p->qr
					      p^k
					      (poly:* xm (list-ref s j))
					      (list-ref a j)))))))
	       (let* ((s (hen:eea-lift (cadr a) (car a) p k))
		      (q-r (ff:p/p->qr p^k (poly:* xm (car s))
				       (car a)))
		      (q (car q-r))
		      (result (list (cadr q-r)
				    (poly:+ (poly:* xm (cadr s)) ; mod p^k
					    (poly:* q (cadr a))))))
		 (check-hd1 a x m p k result))))))))))

;;; 2 factor univariate linear Hensel lifting algorithm
(define (hen:ulhensel2 f g1 h1 p k)
  (let* ((g g1)
	 (h h1)
	 (lf (univ:lc f))
	 (ghrecip (ff:eea-gcd g1 h1 p))
	 (grecip (cadr ghrecip))
	 (hrecip (caddr ghrecip))
	 (x (car f)))
    (let loop ((i 2)
	       (modulus p)
	       (modulus*p (* p p)))
      (if (<= i k)
	  (let* ((discrepancy
		  (univ/scalar
		   (fluid-let ((*modulus* (symmetric:modulus modulus*p)))
		     (number->univ (poly:- (poly:* f (coef:invert lf))
					   (poly:* g h))
				   x))
		   modulus))
		 (gc (cadr (ff:p/p->qr
			    p (number->univ (poly:* hrecip discrepancy) x) g1)))
		 (hc (cadr (ff:p/p->qr
			    p (number->univ (poly:* grecip discrepancy) x)
			    h1))))
	    (set! g (poly:+ g (poly:* gc modulus)))
	    (set! h (poly:+ h (poly:* hc modulus)))
	    (loop (+ 1 i) modulus*p (* modulus*p p)))
	  (list g h)))))

;;; Experimental (slow) 3 factor univariate linear Hensel lifting algorithm
(define (hen:ulhensel3 f g1 h1 i1 p k)
  (let* ((g g1)
	 (h h1)
	 (lf (univ:lc f))
	 (i i1)
	 (x (car f)))
    (let loop ((l 2)
	       (modulus p)
	       (modulus*p (* p p)))
      (if (<= l k)
	  (let* ((diff
		  (univ/scalar
		   (fluid-let ((*modulus* (symmetric:modulus modulus*p)))
		     (number->univ (poly:- (poly:* f (coef:invert lf))
					   (poly:* (poly:* g h) i))
				   x))
		   modulus))
		 (dghi (hen:diophantine (list g1 h1 i1) diff p 1))
		 (dg (car dghi))
		 (dh (cadr dghi))
		 (di (caddr dghi)))
	    (set! g (poly:+ g (poly:* dg modulus)))
	    (set! h (poly:+ h (poly:* dh modulus)))
	    (set! i (poly:+ i (poly:* di modulus)))
	    (loop (+ 1 l) modulus*p (* modulus*p p)))
	  (list g h i)))))

;;; Experimental (slow) n factor univariate linear Hensel lifting algorithm
(define (hen:ulhenseln f as p k)
  (let ((x (car f))
	(lf (univ:lc f)))
    (let loop ((l 2)
	       (modulus p)
	       (modulus*p (* p p)))
      (if (<= l k)
	  (let* ((diff
		  (univ/scalar
		   (fluid-let ((*modulus* (symmetric:modulus modulus*p)))
		     (number->univ (poly:- (poly:* f (coef:invert lf))
					   (reduce poly:* as))
				   x))
		   modulus))
		 (ds (hen:diophantine as diff p 1)))
	    (set! as (map (lambda (x y) (poly:+ x (poly:* y modulus))) as ds))
	    (loop (+ 1 l) modulus*p (* modulus*p p)))
	  as))))

;;;; From: Michael Thomas <mjt@octavia.anu.edu.au>

;;; From page 142 of Davenport et al "Computer Algebra Systems and
;;; Algorithms for Algebraic Computation":
;;;
;;; Let
;;;
;;; Q = b0 + b1 * x + b2 * x^2 + b3 * x^3 + ... + bi * x^i + ... + bq * x^q
;;;
;;; be a divisor of the polynomial
;;;
;;; P = a0 + a1 * x + a2 * x^2 + a3 * x^3 + ... + ai * x^i + ... + ap * x^p
;;;
;;; (where the ai and bi are integers).
;;;
;;; Then
;;;
;;; |b0| + |b1| + |b2| + ... + |bq| <=
;;;
;;;	 2^q * |(ap/bq)| * (a0^2 + a1^2 + ... ap^2)^(1/2)

;;; Page 237 of GCL refers you to Mignotte[4] for ways of determining
;;; the bounding integer.  That reference is:

;;; M. Mignotte, "Some Useful Bounds.," pp. 259-263 in Computer
;;; Algebra - Symbolic and Algebraic computation, ed. B. Buchberger,
;;; G.E. Collins and R. Loos, Springer-Verlag (1982).

;;;; Jaffer 2020:

;;; There seemed to be some confusion regarding the correct formula,
;;; so I tried 5 variations.  factor(x^n-1) for various n stressed the
;;; formula, allowing the correct one to be chosen.  Reducing the
;;; factor of 4 causes some factorings to fail.

;;; Return twice the Landau-Mignotte Bound of the coefficients of p
(define landau-mignotte-bound*2
  (cond
   ((provided? 'inexact)
    (lambda (p)
      (inexact->exact
       (ceiling
	(sqrt (* 4
		 (abs (univ:lc p))
		 (sqrt (apply + (map (lambda (x) (* x x)) (cdr p))))
		 (expt 2 (- (univ:deg p) 1))))))))
   (else
    (lambda (p)
      (integer-sqrt
       (* 4
	  (abs (univ:lc p))
	  (integer-sqrt (apply + (map (lambda (x) (* x x)) (cdr p))))
	  (expt 2 (- (univ:deg p) 1))))))))

;;; Return a suitable power for the prime p to be raised to, to
;;; cover all possible coefficients of poly and its factors.
(define (u:prime-power poly p)
  ;; ceiling integer logarithm base p
  (+ 1 (integer-log p (landau-mignotte-bound*2 poly))))

;;; Find a modulus good for factoring a square-free univariate polynomial
(define (squarefreeness-preserving-modulus poly)
  (define poly-degree (univ:deg poly))
  (let loop ((prime 3))
    (define ply (ff:unorm prime poly))
    (cond ((< 100 prime) (jacal:found-bug 'no-suitable-primes<100 poly))
	  ((or (not (= poly-degree (univ:deg ply)))
	       (not (univ:one? (ff:euclid-gcd prime
					      (ff:diff prime ply (car poly))
					      ply)
			       (car poly))))
	   (loop (car (primes> (+ 1 prime) 1))))
	  ;; (math:debug (print 'squarefreeness-preserving-modulus prime))
	  (else prime))))

;;; Factorise a square-free univariate polynomial over the integers
(define (u:factorsz f1)
  (let* ((var (car f1))
	 (lf (univ:lc f1))
	 (p (squarefreeness-preserving-modulus f1))
	 (pfs (ff:berlekamp p (fluid-let ((*modulus* (symmetric:modulus p)))
				(univ:make-monic f1))))
	 (n (length pfs))
	 (answer '()))
    (if (eqv? 1 n)
	(list f1)
	(let* ((k (u:prime-power f1 p))
	       (p^k (expt p k))
	       (s-p^k (symmetric:modulus p^k))
	       (factors
		(case n
		  ((2)  (hen:ulhensel2 f1 (car pfs) (cadr pfs) p k))
		  ((3)  (hen:ulhensel3 f1 (car pfs) (cadr pfs) (caddr pfs) p k))
		  (else (hen:ulhenseln f1 pfs p k)))))
	  (for-each (lambda (x)
		      (let ((x*lf (fluid-let ((*modulus* s-p^k))
				    (poly:times-const lf x))))
			(cond ((eqv? 0 (univ:prem (poly:* f1 lf) x*lf))
			       (set! answer (cons (u:primz x*lf) answer))
			       (set! factors (delete x factors))
			       (set! n (+ -1 n))))))
		    factors)
	  (if (null? factors)
	      answer
	      (case (length pfs)
		((2) (list f1))
		((3) (cons (u:primz
			    (fluid-let ((*modulus* s-p^k))
			      (reduce-init univ:* (list var lf) factors)))
			   answer))
		(else (call-with-current-continuation
		       (lambda (exit)
			 (let loop ((combine 2))
			   (cond
			    ((<= (* 2 combine) n)
			     (for-each
			      (lambda (u)
				(let ((g*lf (fluid-let ((*modulus* s-p^k))
					      (reduce-init univ:*
							   (list var lf) u))))
				  (cond
				   ((eqv? 0 (univ:prem f1 g*lf))
				    (set! answer (cons (u:primz g*lf) answer))
				    (for-each (lambda (x)
						(set! factors (delete x factors)))
					      u)
				    (set! n (- n combine))
				    (if (> (* 2 combine) n) (exit #t))))))
			      (combinations factors combine))
;;; factor((x^5+y^5)^5-(x^5-y^5)^5);
;;; exercises this loop
			     (loop (+ combine 1))
			     )))))
;;; At this point the code tests for (null? factors) and returns
;;; answer if true.  This doesn't seem to happen.
		      (if (null? factors)
			  (jacal:found-bug 'u:factorsz 'got-null-factors answer)
			  (cons (u:primz
				 (fluid-let ((*modulus* s-p^k))
				   (reduce-init univ:* (list var lf) factors)))
				answer)))))))))

;;(require 'debug-jacal) (trace-all "uv-hensel.scm")
