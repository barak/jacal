;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1995, 1997 Aubrey Jaffer.
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


;;;Functions which operate on polynomials as polynomials
;;;have prefix POLY:
;;;Functions which operate on polynomials with the same major variable
;;;have prefix UNIV:
;;;Functions which operate on scalar coefficients
;;;have prefix COEF:

(require 'modular)
(require 'common-list-functions)

;;; *MODULUS* (defined in "toploads.scm") is the modulus for the
;;; coefficient ring used by polynomials.  It gets dynamically bound
;;; using FLUID-LET.

;;(proclaim '(optimize (speed 3) (compilation-speed 0)))

(define (coef:invertable? k) (modular:invertable? *modulus* k))
(define (coef:invert j) (modular:invert *modulus* j))
(define (poly:0? n) (eqv? 0 n))
(define (poly:1? n) (eqv? 1 n))
(define (divides? a b) (zero? (remainder b a)))

(define (sign a) (if (negative? a) -1 1))

;;; poly is the internal workhorse data type in the form
;;; of numeric or list (var coeff0 coeff1 ...)
;;; where var is a variable and coeffn is the coefficient of var^n.
;;; coeffn is poly.  The variables are arranged reverse alphabetically
;;; with z higher order than A.

(define (univ:one? p var)
  (if (not (eqv? (car p) var)) (jacal:found-bug 'expecting var 'univ p))
  (and (= (length p) 2) (= (cadr p) 1)))
(define (univ:zero? p)
  (and (= (length p) 2) (zero? (cadr p))))
(define (univ:const? p)
  (and (= (length p) 2) (number? (cadr p)) (cadr p)))

;;; Degree of already normalized p
(define (univ:deg p)
  (- (length p) 2))

;;; Return a polynomial in variable v if given a number, otherwise
;;; returns its first argument.
(define (number->poly a v)
  (if (number? a) (list v a) a))

(define (poly:find-var? poly var)
  (poly:find-var-if? poly (lambda (x) (eqv? var x))))

(define (poly:find-var-if? poly proc)
  (cond ((number? poly) #f)
	((proc (car poly)))
	(else (some (lambda (x) (poly:find-var-if? x proc)) (cdr poly)))))

;;; This can call proc more than once per var
(define (poly:for-each-var proc poly)
  (cond ((number? poly))
	(else
	 (proc (car poly))
	 (for-each (lambda (b) (poly:for-each-var proc b))
		   (cdr poly)))))

;;;POLY:VARS returns a list of all vars used in POLY
(define (poly:vars poly)
  (let ((elts '()))
    (poly:for-each-var (lambda (v) (set! elts (adjoin v elts))) poly)
    elts))

(define (poly:total-degree poly)
  (if (number? poly)
      0
      (do ((lst (cdr poly) (cdr lst))
	   (tdg 0 (+ 1 tdg))
	   (mxdg 0 (max mxdg (+ tdg (poly:total-degree (car lst))))))
	  ((null? lst) mxdg))))

(define (poly:poly? p)
  (and (pair? p) (poly:var? (car p))))

(define (poly:univariate? p)
  (and (poly:poly? p) (every number? (cdr p))))

(define (poly:multivariate? p)
  (and (poly:poly? p) (not (every number? (cdr p)))))

;;;; the following functions are for internal use on the poly data type

;;; this normalizes short polys.
(define (univ:norm0 var col)
  (cond ((null? col) 0)
	((null? (cdr col)) (car col))
	(else (cons var col))))

(define (map-no-end-0s proc l)
  (if (null? l)
      l
      (let ((first (proc (car l)))
	    (rest (map-no-end-0s proc (cdr l))))
	(if (and (null? rest) (eqv? 0 first))
	    rest
	    (cons first rest)))))
(define (map2c-no-end-0s proc l1 l2)
  (cond ((null? l1) l2)
	((null? l2) l1)
	(else
	 (let ((first (proc (car l1) (car l2)))
	       (rest (map2c-no-end-0s proc (cdr l1) (cdr l2))))
	   (if (and (null? rest) (eqv? 0 first))
	       rest
	       (cons first rest))))))

(define (ipow-by-squaring x n acc proc)
  (cond ((zero? n) acc)
	((eqv? 1 n) (proc acc x))
	(else (ipow-by-squaring (proc x x)
				(quotient n 2)
				(if (even? n) acc (proc acc x))
				proc))))

(define (poly:add-scalar scalar p2)
  (if (zero? scalar) p2
      (poly:add-const scalar p2)))
(define (poly:add-const term p2)
  (cons (car p2) (cons (poly:+ term (cadr p2)) (cddr p2))))
(define (poly:+ p1 p2)
  (cond ((and (number? p1) (number? p2)) (modular:+ *modulus* p1 p2))
	((number? p1) (poly:add-scalar p1 p2))
	((number? p2) (poly:add-scalar p2 p1))
	((eq? (car p1) (car p2))
	 (univ:norm0 (car p1) (map2c-no-end-0s poly:+ (cdr p1) (cdr p2))))
	((var:> (car p2) (car p1)) (poly:add-const p1 p2))
	(else (poly:add-const p2 p1))))
(define (univ:+ p1 p2)
  (cons (car p1) (map2c-no-end-0s poly:+ (cdr p1) (cdr p2))))

;;; UNIV:* is called from POLY:*.  If *MODULUS* has 0-divisors, an
;;; unnormalized polynomial may be returned (leading 0 coefficients).

(define (univ:* p1 p2)
  (let ((res (make-list (+ (length (cdr p1)) (length (cdr p2)) -1) 0)))
    (do ((rpl res (cdr rpl))
	 (a (cdr p1) (cdr a)))
	((null? a) (cons (car p1) res))
      (do ((b (cdr p2) (cdr b))
	   (rp rpl (cdr rp)))
	  ((null? b))
	(set-car! rp (poly:+ (poly:* (car a) (car b)) (car rp)))))))
(define (poly:times-scalar scalar p2)
  (cond ((zero? scalar) 0)
	((eqv? 1 scalar) p2)
	(else (poly:times-const scalar p2))))
(define (poly:times-const term p2)
  (cons (car p2) (map (lambda (x) (poly:* term x)) (cdr p2))))
(define (poly:* p1 p2)
  (cond ((and (number? p1) (number? p2)) (modular:* *modulus* p1 p2))
	((number? p1) (poly:times-scalar p1 p2))
	((number? p2) (poly:times-scalar p2 p1))
	((eq? (car p1) (car p2)) (univ:* p1 p2))
	((var:> (car p2) (car p1)) (poly:times-const p1 p2))
	(else (poly:times-const p2 p1))))

(define (poly:negate p) (poly:* -1 p))
(define (poly:- p1 p2) (poly:+ p1 (poly:negate p2)))

;;; Divide coefficients by a scalar
(define (univ/scalar a c)
  (cons (car a) (map (lambda (x) (quotient x c)) (cdr a))))

(define (univ:/? u v)
  (let ((r (list->vector (cdr u)))
	(m (length (cddr u)))
	(n (length (cddr v)))
	(vn (univ:lc v))
	(q '()))
    (do ((k (- m n) (+ -1 k))
	 (qk (poly:/? (vector-ref r m) vn)
	     (and (> k 0) (poly:/? (vector-ref r (+ n k -1)) vn))))
	((not qk)
	 (and (< k 0)
	      (do ((k (+ -2 n) (+ -1 k)))
		  ((or (< k 0) (not (poly:0? (vector-ref r k))))
		   (< k 0)))
	      (univ:norm0 (car u) q)))
      (set! q (cons qk q))
      (let ((qk- (poly:negate qk)))
	(do ((j (+ n k -1) (+ -1 j)))
	    ((< j k))
	  (vector-set! r j (poly:+
			    (vector-ref r j)
			    (poly:* (list-ref v (+ (- j k) 1)) qk-))))))))

;;; POLY:/? returns U / V if V divides U, otherwise returns #f
(define (poly:/? u v)
  (cond ((equal? u v) 1)
	((eqv? 0 u) 0)
	((number? v)
	 (cond ((poly:0? v) #f)
;;;	       ((unit? v) (poly:* u v))
	       ((coef:invertable? v) (poly:* u (coef:invert v)))
	       ((number? u) (and (divides? v u) (quotient u v)))
	       (else (univ:/? u (const:promote (car u) v)))))
	((number? u) #f)
	((eq? (car u) (car v)) (univ:/? u v))
	((var:> (car u) (car v))
	 (univ:/? u (const:promote (car u) v)))
	(else #f)))

(define (univ:/ dividend divisor)
  (or (univ:/? dividend divisor)
      (math:error divisor 'does-not-udivide- dividend)))

(define (poly:/ dividend divisor)
  (or (poly:/? dividend divisor)
      (math:error divisor 'does-not-divide- dividend)))

(define (univ:monomial coeff n var)
  (cond ((eqv? 0 coeff) 0)
	((>= 0 n) coeff)
	(else
	 (cons var
	       ((lambda (x) (set-car! (last-pair x) coeff) x)
		(make-list (+ 1 n) 0))))))

(define (poly:degree p var)
  (cond ((number? p) 0)
	((eq? var (car p)) (length (cddr p)))
	((var:> var (car p)) 0)
	(else (reduce-init (lambda (m c) (max m (poly:degree c var)))
			   0
			   (cdr p)))))

(define (poly:leading-coeff p var) (poly:coeff p var (poly:degree p var)))

(define (poly:^ x n)
  (if (number? x)
      (expt x n)			; (ipow-by-squaring x n 1 *)
      (ipow-by-squaring x n 1 poly:*)))

;;;; Routines used in normalizing IMPL polynomials

(define (univ:lc p)
  (car (last-pair p)))

(define (leading-number p)
  (if (number? p) p (leading-number (univ:lc p))))

;;; This canonicalizes polys with respect to units by forcing the
;;; numerical coefficient of a certain term to always be positive.  In
;;; the case of finite field coefficients, the polynomial is made
;;; monic.
(define (unitcan p)
  (cond ((zero? *modulus*)
	 (if (negative? (leading-number p)) (poly:negate p) p))
	((number? p)
	 (math:warn 'unitcan 'of p)
	 p)
	(else (univ:make-monic p))))

(define (shorter? x y) (< (length x) (length y)))

(define (univ:degree p var)
  (if (or (number? p) (not (eq? (car p) var))) 0 (length (cddr p))))

;;; THE NEXT SEVERAL ROUTINES FOR SUBRESULTANT GCD ASSUME THAT THE
;;; ARGUMENTS ARE POLYNOMIALS WITH THE SAME MAJOR VARIABLE.  THESE TWO
;;; ROUTINES ASSUME THAT THE FIRST ARGUMENT IS OF GREATER OR EQUAL
;;; ORDER THAN THE SECOND.
;;; These algorithms taken from:
;;; Knuth, D. E.,
;;; The Art Of Computer Programming, Vol. 2: Seminumerical Algorithms,
;;; Addison Wesley, Reading, MA 1969.
;;; Pseudo Remainder

;;; This returns a list of the pseudo quotient and pseudo remainder.
(define (univ:pdiv u v)
  (let* ((r (list->vector (cdr u)))
	 (m (length (cddr u)))
	 (n (length (cddr v)))
	 (vn (univ:lc v))
	 (q (make-vector (+ (- m n) 1) 1)))
    (do ((tt (- (- m n) 1) (+ -1 tt))
	 (k 1 (+ 1 k))
	 (vnp 1))
	((< tt 0))
      (set! vnp (poly:* vnp vn))
      (vector-set! q k vnp)
      (vector-set! r tt (poly:* (vector-ref r tt) vnp)))
    (do ((k (- m n) (+ -1 k))
	 (rnk 0))
	((< k 0))
      (set! rnk (poly:negate (vector-ref r (+ n k))))
      (do ((j (+ n k -1) (+ -1 j)))
	  ((< j k))
	(vector-set! r j (poly:+ (poly:* (vector-ref r j) vn)
				 (poly:* (list-ref v (+ (- j k) 1)) rnk)))))
    (list
     (do ((k (- m n) (+ -1 k))
	  (end '() (cons (poly:* (vector-ref r (+ n k))
				 (vector-ref q k)) end)))
	 ((zero? k) (univ:norm0 (car u) (cons (vector-ref r n) end))))
     (do ((j (+ -1 n) (+ -1 j))
	  (end '()))
	 ((< j 0) (univ:norm0 (car u) end))
       (if (not (and (null? end) (eqv? 0 (vector-ref r j))))
	   (set! end (cons (vector-ref r j) end)))))))

;;; POLY:PDIV returns a list of the pseudo-quotient and pseudo-remainder
(define (poly:pdiv dividend divisor var)
  (let ((pd1 (poly:degree dividend var))
	(pd2 (poly:degree divisor var)))
    (cond ((< pd1 pd2) (list 0 dividend))
	  ((zero? (+ pd1 pd2))
	   (list (quotient dividend divisor) (remainder dividend divisor)))
	  ((zero? pd1) (list 0 dividend))
	  ((zero? pd2)
;;; This should work but doesn't.
;;;	   (map univ:demote (univ:pdiv (poly:promote var dividend)
;;;				       (const:promote var divisor)))
	   (list 0 dividend))
	  (else
	   (map univ:demote (univ:pdiv (poly:promote var dividend)
				       (poly:promote var divisor)))))))
(define (poly:prem dividend divisor var)
  (let ((pd1 (poly:degree dividend var))
	(pd2 (poly:degree divisor var)))
    (cond ((< pd1 pd2) dividend)
	  ((zero? (+ pd1 pd2)) (remainder dividend divisor))
	  ((zero? pd1) dividend)
	  ((zero? pd2)
;;; Does this work?
;;;	   (univ:demote (univ:prem (poly:promote var dividend)
;;;				   (const:promote var divisor)))
	   dividend)
	  (else
	   (univ:demote (univ:prem (poly:promote var dividend)
				   (poly:promote var divisor)))))))

(define (univ:prem u v)
  (let* ((r (list->vector (cdr u)))
	 (m (length (cddr u)))
	 (n (length (cddr v)))
	 (vn (univ:lc v)))
    (do ((k (- (- m n) 1) (+ -1 k))
	 (vnp 1))
	((< k 0))
      (set! vnp (poly:* vnp vn))
      (vector-set! r k (poly:* (vector-ref r k) vnp)))
    (do ((k (- m n) (+ -1 k))
	 (rnk 0))
	((< k 0))
      (set! rnk (poly:negate (vector-ref r (+ n k))))
      (do ((j (+ n k -1) (+ -1 j)))
	  ((< j k))
	(vector-set! r j (poly:+ (poly:* (vector-ref r j) vn)
				 (poly:* (list-ref v (+ (- j k) 1)) rnk)))))
    (do ((j (+ -1 n) (+ -1 j))
	 (end '()))
	((< j 0) (univ:norm0 (car u) end))
      (if (and (null? end) (eqv? 0 (vector-ref r j)))
	  #f
	  (set! end (cons (vector-ref r j) end))))))

;;; Pseudo Remainder Sequence
(define (univ:prs u v)
  (let ((var (car u))
	(g 1)
	(h 1)
	(delta 0))
    (do ((r (univ:prem u v) (univ:prem u v)))
	((eqv? 0 (univ:degree r var))
	 (if (eqv? 0 r) v r))
      (set! delta (- (univ:degree u var) (univ:degree v var)))
      (set! u v)
      (set! v (univ:/ r (const:promote (car r) (poly:* g (poly:^ h delta)))))
      (set! g (univ:lc u))
      (set! h (cond ((eqv? 1 delta) g)
		    ((zero? delta) h)
		    (else (poly:/ (poly:^ g delta)
				  (poly:^ h (+ -1 delta)))))))))

(define (univ:gcd u v)
  (let* ((cu (univ:cont u))
	 (cv (univ:cont v))
	 (c (poly:gcd cu cv))
	 (ppu (poly:/ u cu))
	 (ppv (poly:/ v cv))
	 (ans (if (shorter? ppv ppu)
		  (univ:prs ppu ppv)
		  (univ:prs ppv ppu))))
    (if (zero? (univ:degree ans (car u)))
	c
	(poly:* c (univ:primpart ans)))))

(define (poly:gcd p1 p2)
  (cond ((equal? p1 p2) p1)
	((and (number? p1) (number? p2)) (gcd p1 p2))
	((number? p1) (if (poly:0? p1) p2 (apply poly:gcd* p1 (cdr p2))))
	((number? p2) (if (poly:0? p2) p1 (apply poly:gcd* p2 (cdr p1))))
	((eq? (car p1) (car p2))
	 (cond ((zero? *modulus*) (univ:gcd p1 p2))
	       (else (univ:fgcd p1 p2))))
	((var:> (car p2) (car p1)) (apply poly:gcd* p1 (cdr p2)))
	(else (apply poly:gcd* p2 (cdr p1)))))

(define (poly:gcd* . li)
  (let ((nums (remove-if-not number? li)))
    (if (null? nums)
	(reduce poly:gcd li)
	(let ((gnum (reduce gcd nums)))
	  (if (= 1 gnum) 1
	      (reduce-init poly:gcd gnum (remove-if number? li)))))))

(define (univ:cont p) (apply poly:gcd* (cdr p)))
(define (univ:primpart p) (poly:/ p (univ:cont p)))
(define (poly:num-cont p)
  (if (number? p)
      p
      (do ((l (cdr p) (cdr l))
	   (n (poly:num-cont (cadr p))
	      (gcd n (poly:num-cont (cadr l)))))
	  ((or (= 1 n) (null? (cdr l))) n))))
(define (poly:primpart p) (poly:/ p (poly:num-cont p)))

;;; Returns the sign of the leading coefficient of univariate poly p
(define (u:unitz p) (sign (univ:lc p)))

;;; Returns the sign of the leading coefficient of the polynomial p.
(define (poly:unitz p var)
  (sign (leading-number (poly:leading-coeff p var))))

(define (u:primz p)
  (univ/scalar p (* (u:unitz p) (univ:cont p))))

;;; Primitive part of a multivariate polynomial p, with respect to var.
(define (poly:primz p var)
  (if (number? p)
      (abs p)
      (unitcan (poly:/ p (univ:cont p)))))

(define (list-ref? l n)
  (cond ((null? l) #f)
	((zero? n) (car l))
	(else (list-ref? (cdr l) (+ -1 n)))))

(define (univ:coeff p ord) (or (list-ref? (cdr p) ord) 0))
(define (poly:coeff p var ord)
  (cond ((or (number? p) (var:> var (car p)))
	 (if (zero? ord) p 0))
	((eq? var (car p)) (univ:coeff p ord))
	(else
	 (univ:norm0 (car p)
		     (map-no-end-0s (lambda (c) (poly:coeff c var ord))
				    (cdr p))))))

(define (poly:subst0 old e) (poly:coeff e old 0))

(define const:promote list)

(define (poly:promote var p)
  (if (eq? var (car p))
      p
      (let ((dgr (poly:degree p var)))
	(do ((i dgr (+ -1 i))
	     (ol (list (poly:coeff p var dgr))
		 (cons (poly:coeff p var (+ -1 i)) ol)))
	    ((zero? i) (cons var ol))))))

;;;this is bummed if v has higher priority than any variable in (cdr p)
(define (univ:demote p)
  (if (number? p)
      p
    (let ((v (car p)))
      (if (every (lambda (cof) (or (number? cof) (var:> v (car cof))))
		 (cdr p))
	  p
	(poly:+ (cadr p)
		(do ((trms (cddr p) (cdr trms))
		     (sum 0)
		     (mon (list v 0 1) (cons v (cons 0 (cdr mon)))))
		    ((null? trms) sum)
		    (set! sum (poly:+ sum (poly:* mon (car trms))))))))))

(define (poly:cabs p)
  (cond ((number? p) (abs p))
	((poly:find-var? p %i)
	 (^ (apply poly:+
		   (map (lambda (x) (poly:* x x))
			(cdr (poly:promote %i p))))
	    _1/2))
	(else (deferop _abs p))))

(define (poly:valid? p)
  (if (number? p) #t
      (let ((var (car p)))
	(every (lambda (q) (poly:valid1 q var))
	       (cdr p)))))

(define (poly:valid1 p var)
  (cond ((number? p) #t)
	((var:> var (car p)) (every (lambda (p) (poly:valid1 p var))
				    (cdr p)))
	(else
	 (display-diag "poly:valid detected that ")
	 (math:print var)
	 (display-diag " <= some var in ")
	 (math:print p)
	 (newline-diag)
	 #f)))

(define (sylvester p1 p2 var)
  (set! p1 (poly:promote var p1))
  (set! p2 (poly:promote var p2))
  (let ((d1 (univ:degree p1 var))
	(d2 (univ:degree p2 var))
	(m (list)))
    (do ((i d1 (+ -1 i))
	 (row (nconc (make-list (+ -1 d1) 0) (reverse (cdr p2)))
	      (append (cdr row) (list 0))))
	((<= i 1) (set! m (cons row m)))
      (set! m (cons row m)))
    (do ((i d2 (+ -1 i))
	 (row (nconc (make-list (+ -1 d2) 0) (reverse (cdr p1)))
	      (append (cdr row) (list 0))))
	((<= i 1) (set! m (cons row m)))
      (set! m (cons row m)))
    m))

;;; Bareiss's integer preserving gaussian elimination.
;;; Bareiss, E.H.: Sylvester's identity and multistep
;;; integer-preserving Gaussian elimination. Mathematics of
;;; Computation 22, 565-578, 1968.
;;; as related by:
;;; Akritas, A.G.: Exact Algorithms for the Matrix-Triangulation
;;; Subresultant PRS Method.  Computers and Mathematics, 145-155.
;;; Springer Verlag, 1989.
(define (bareiss m)
  4)

(define (poly:resultant p1 p2 var)
  (let ((u1 (poly:promote var p1))
	(u2 (poly:promote var p2)))
    (or (not (zero? (univ:degree u1 var)))
	(not (zero? (univ:degree u2 var)))
	(math:error var 'does-not-appear-in- p1 'or- p2))
    (let ((res (cond ((zero? (univ:degree u1 var)) p1)
		     ((zero? (univ:degree u2 var)) p2)
		     ((shorter? u1 u2) (univ:prs u2 u1))
		     (else (univ:prs u1 u2)))))
      (if (zero? (univ:degree res var)) res
	  0))))

(define (poly:elim2 p1 p2 var)
  (let* ((u1 (poly:promote var p1))
	 (u2 (poly:promote var p2))
	 (pg (poly:gcd (univ:lc u1) (univ:lc u2))))
    (or (not (zero? (univ:degree u1 var)))
	(not (zero? (univ:degree u2 var)))
	(math:error var 'does-not-appear-in- p1 'or- p2))
    (let* ((res (cond ((zero? (univ:degree u1 var)) p1)
		      ((zero? (univ:degree u2 var)) p2)
		      ((shorter? u1 u2) (univ:prs u2 u1))
		      (else (univ:prs u1 u2))))
	   (e (if (zero? (univ:degree res var)) res
		  0)))
      (if (or (number? pg)) e
	  (let ((q (poly:/ e pg)))
	    (if (number? q) e (univ:primpart q)))))))

(define (poly:modularize modulus poly)
  (if (number? poly)
      (modular:normalize modulus poly)
      (let ((coeffs (map-no-end-0s (lambda (x) (poly:modularize modulus x))
				   (cdr poly))))
	(if (null? coeffs) 0 (cons (car poly) coeffs)))))

;;;; UNIV:F routines for polynomials with (finite) field coefficients.

;;; This returns a list of the quotient and remainder.
;;; After Knuth Vol 2. 4.6.1 Algorithm D.
(define (univ:fdiv u v)
  (let* ((r (list->vector (cdr u)))
	 (m (length (cddr u)))
	 (n (length (cddr v)))
	 (vni (coef:invert (univ:lc v)))
	 (q '()))
    (do ((k (- m n) (+ -1 k))
	 (rnk 0))
	((< k 0))
      (set! q (cons (poly:* vni (vector-ref r (+ n k))) q))
      (set! rnk (poly:negate (car q)))
      (do ((j (+ n k -1) (+ -1 j)))
	  ((< j k))
	(vector-set! r j (poly:+ (vector-ref r j)
				 (poly:* (list-ref v (+ (- j k) 1)) rnk)))))
    (list (univ:norm0 (car u) q)
	  (do ((j (+ -1 n) (+ -1 j))
	       (end '()))
	      ((< j 0) (univ:norm0 (car u) end))
	    (if (and (null? end) (eqv? 0 (vector-ref r j)))
		#f
		(set! end (cons (vector-ref r j) end)))))))

(define (univ:frem u v)
  (let* ((r (list->vector (cdr u)))
	 (m (length (cddr u)))
	 (n (length (cddr v)))
	 (vni (poly:negate (coef:invert (univ:lc v)))))
    (do ((k (- m n) (+ -1 k))
	 (rnk 0))
	((< k 0))
      (set! rnk (poly:* vni (vector-ref r (+ n k))))
      (do ((j (+ n k -1) (+ -1 j)))
	  ((< j k))
	(vector-set! r j (poly:+ (vector-ref r j)
				 (poly:* (list-ref v (+ (- j k) 1)) rnk)))))
    (do ((j (+ -1 n) (+ -1 j))
	 (end '()))
	((< j 0) (univ:norm0 (car u) end))
      (if (and (null? end) (eqv? 0 (vector-ref r j)))
	  #f
	  (set! end (cons (vector-ref r j) end))))))

;;; Remainder Sequence for Polynomials with a Coefficient Field
(define (univ:frs u v)
  (let ((var (car u)))
    (do ((r (univ:frem u v) (univ:frem u v)))
	((eqv? 0 (univ:degree r var))
	 (if (eqv? 0 r) v r))
      (set! u v)
      (set! v r))))

(define (univ:fgcd u v)
  (let* ((ans (if (shorter? v u)
		  (univ:frs u v)
		  (univ:frs v u))))
    (if (zero? (univ:degree ans (car u)))
	1				; (list (car u) 1)
	(univ:make-monic ans))))

(define (univ:make-monic p)
  (poly:* p (coef:invert (univ:lc p))))

;;;; VERIFICATION TESTS
(define (poly:test)
  (define a (sexp->var 'a))
  (define b (sexp->var 'b))
  (define c (sexp->var 'c))
  (define x (sexp->var 'x))
  (define y (sexp->var 'y))
  (test (list a 0 -2)
	poly:gcd
	(list a 0 -2)
	(list a 0 0 -2))
  (test (list x (list a 0 1) 1)
	poly:gcd
	(list x (list a 0 0 -1) 0 1)
	(list x (list a 0 0 1) (list a 0 2) 1))
  (test (list x 0 (list a 0 1))
	poly:gcd
	(list x 0 (list a 0 0 1))
	(list x 0 0 (list a 0 1)))
  (test (list x (list b 0 0 1) 0 (list b 1 2) (list a 0 1) 1)
	poly:resultant
	(list y (list x (list b 0 1) 0 1) (list x 0 1))
	(list y (list x 1 (list a 0 1)) 0 1)
	y)
  (test (list y (list b 0 0 1) 0 (list b 1 2) (list a 0 1) 1)
	poly:resultant
	(list y (list b 0 1) (list x 0 1) 1)
	(list y (list x 1 0 1) (list a 0 1))
	x)
  (cond ((provided? 'bignum)
	 (test 1
	       poly:gcd
	       (list x -5 2 8 -3 -3 1 1)
	       (list x 21 -9 -4 5 3))
	 (test 1
	       poly:gcd
	       (list x -5 2 8 -3 -3 0 1 0 1)
	       (list x 21 -9 -4 0 5 0 3))))
  'done)
