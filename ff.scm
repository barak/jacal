;; "ff.scm" Polynomial factorization.		-*-scheme-*-
;; Copyright 1994, 1995 Mike Thomas
;; Copyright 1995, 1997, 1998, 1999, 2001, 2002 Aubrey Jaffer
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


;;; AUTHOR
;;;	  Mike Thomas
;;;	  46 Harold Street
;;;	  STAFFORD  QLD	 4053
;;;	  AUSTRALIA
;;;
;;;	  Phone: Intl + 61 7 356 8494
;;;	  Email: mjt@octavia.anu.edu.au

;;; SOURCES
;;; These algorithms are drawn from:
;;;
;;; (GCL)
;;; Algorithms for Computer Algebra
;;;  by Keith O. Geddes, Stephen R. Czapor, George Labahn
;;;  (October 1992) Kluwer Academic Pub; ISBN: 0-7923-9259-0
;;;
;;; Computer Algebra: Systems and Algorithms for Algebraic Computation
;;;  by Y. Siret (Editor), E. Tournier, J. H. Davenport, F. Tournier
;;;  2nd edition (June 1993) Academic Press; ISBN: 0-122-04232-8
;;;
;;; The Art of Computer Programming : Seminumerical Algorithms (Vol 2)
;;;  by Donald Ervin Knuth
;;;  2nd Ed (1981) Addison-Wesley Pub Co; ISBN: 0-201-03822-6
;;; A new edition of this book is availble:
;;;  3rd Ed (November 1997) Addison-Wesley Pub Co; ISBN: 0-201-89684-2

;;; DEVELOPMENT LANGUAGE
;;; SCM 4e1, 4e2 compiled with DICE v 2.07.54

;;; DEVELOPMENT SYSTEM
;;; Amiga 1200, 2Mb Chip RAM, 4Mb Fast RAM, 50MHz 68030 CPU No FPU,
;;; Workbench 3.0 (until a power supply problem stuffed it that is.)

;;; DEPENDENCIES
;;; You need R4RS Scheme, SLIB and JACAL (latest versions).
;;; Multiargument *, +

;;; REPRESENTATIONS
;;; The modular homomorphism sym:sym maps the integers to a symmetric
;;; representation, following the numerous examples in GCL.  This
;;; mapping does not seem to correspond with GCL's own definition of Z
;;; mod n using a simple remainder algorithm.

;;; The univariate polynomials expected and returned by the Scheme
;;; procedures are JACAL's list representation, not numbers by
;;; themselves.  There is no attempt to cover sparse polynomials yet.

;;; An ideal is a list of lists, each one specifying a variable and
;;; it's value eg '((y 1) (z 2) (p -1)).

;;; DESIGN PHILOSOPHY

;;; 1. Fun;

;;; 2. Correct operation for univariate and multivariate polynomials
;;;    over the rationals (Q), then maybe over algebraic number fields;

;;; 3. refine the algorithms for speed after they work correctly;

;;; 4. make the system workable even for smaller computers, based on the
;;;    crude and discriminatory assumption that people who want to crack
;;;    large problems can afford to buy or write larger systems;

;;; 5. and ideally, I want the system to handle investigation of the
;;;    stability of systems of differential equations which requires
;;;    algebraic solution of eigensystems, and for an attempt on the
;;;    Risch integration algorithm.

;;; I have therefore initially used Linear Hensel Lifting and the small
;;; prime Berlekamp Algorithm.  I make no attempt yet, among other
;;; possibilities, to optimise the factorisation process dynamically by
;;; checking several primes or restarting the algorithm on testing for
;;; true factors after the Berlekamp process.  Later, perhaps.

;;; NAMING
;;;	     u: univariate polynomials, should be JACAL's univ:
;;;	    ff: polynomials over finite integer fields using the
;;;		symmetric modular representation
;;;	   sym: symmetric representation of (modular) integers
;;;	  poly: Jacal polynomial types
;;;	   hen: Hensel lifting procedures

;;; ABBREVIATIONS
;;;	     Q:	The Rational numbers
;;;	     Z:	The integers

;;; ARGUMENT CODING CONVENTIONS
;;;	    ff: the modulus argument is the last in the argument list

;;; KNOWN BUGS

;;; -- factor((2*z+y-x)*(y^3*z-a*x^2)*(b*z^2+y));
;;;    factor((a*a*b*z+d)*(2*a*b*b*z+c)*((u+a)*z+1));
;;;    Hang my presently memory challenged computer during poly:sff
;;;    probably because of lack of room

;;; -- Parfrac worketh not.  Wherefore?  Because I trieth too hard to
;;;    be clevereth.  This one can wait.

;;; -- The result is not always sorted or normalised.


;;; KNOWN MINOR PROBLEMS (FROM MY POINT OF VIEW)

;;; -- Output format sometimes gets misaligned.

;;; -- Speed is not great.  See DESIGN PHILOSOPHY above.

;;; -- Factorization over Q needs to return monic polynomials if it is
;;;    to produce complete unique unit normal factorisations.
;;;    Factorisation over Z and Q would also need to factorise numeric
;;;    content in this case.

(require 'array)
(require 'array-for-each)
(require 'fluid-let)
(require 'common-list-functions)
(require 'modular)

(define sym:invert (lambda (m a) (modular:invert (symmetric:modulus m) a)))
(define sym:sym (lambda (m a) (modular:normalize (symmetric:modulus m) a)))

(define (modulus->array-prototype p . fill)
  (apply (cond ((not (number? p)) vector)
	       ((negative? p)
		(let ((len (integer-length (- p))))
		  (cond ((<= len  8) A:fixZ8b)
			((<= len 16) A:fixZ16b)
			((<= len 32) A:fixZ32b)
			(else vector))))
	       ((positive? p)
		(let ((len (integer-length (+ -1 p))))
		  (cond ((<= len  8) A:fixN8b)
			((<= len 16) A:fixN16b)
			((<= len 32) A:fixN32b)
			(else vector))))
	       (else vector))
	 fill))

(define (sym:array-prototype p . fill)
  (apply modulus->array-prototype (quotient p -2) fill))

;;; Reduce univariate p mod n and remove leading zero coefficients
(define (ff:unorm n p)
  (cons (car p)
	(cons (sym:sym n (cadr p))
	      (map-no-end-0s (lambda (x) (sym:sym n x)) (cddr p)))))

;;; Degree of (ff:unorm n p)
(define (ff:degree n p)
  (- (length (ff:unorm n p)) 2))

(define (ff:lc n p)
  (sym:sym n (univ:lc p)))

(define (ff:monic? n p)
  (and (not (univ:const? p)) (eqv? 1 (ff:lc n p))))

;;; Printer for diagnostic information
(define (ff:print . args)
  (define result #f)
  (for-each (lambda (x) (set! result x) (display-diag x) (display #\space)) args)
  (newline-diag)
  result)

;;;================================================================
;;; Standard Euclidean algorithm for polynomial gcd over Z mod n
(define ff:euclid-gcd
  (lambda (modulus x y)
    (set! x (ff:unorm modulus x))
    (set! y (ff:unorm modulus y))
    (fluid-let ((*modulus* (symmetric:modulus modulus)))
      (cond
       ((univ:zero? x) (univ:make-monic y))
       ((univ:zero? y) (univ:make-monic x))
       (else
	(let ((ans (univ:fgcd x y)))
	  (if (number? ans) (list (car x) ans) ans)))))))

;;; ===================== Truncating Division =====================
(define ff:p/p->qr
  ;;(debug:check ff:p/p->qr-mjt
  (lambda (modulus x y)
    (set! x (ff:unorm modulus x))
    (set! y (ff:unorm modulus y))
    (if (< (length x) (length y))
	(list (list (car x) 0) (ff:unorm modulus x))
	(fluid-let ((*modulus* (symmetric:modulus modulus)))
	  (map (lambda (ans)
		 (if (number? ans) (list (car x) ans) ans))
	       (univ:fdiv x y)))))
  ;;'ff:p/p->qr-mjt
  ;;'univ:fdiv)
  )
(define (ff:p/p p a b)
  (let ((u (ff:unorm p a))
	(v (ff:unorm p b)))
    (let ((m (univ:deg u))
	  (n (univ:deg v)))
      (cond
       ((univ:zero? v)
	(slib:error 'ff:p/p "Division by 0 is undefined."))
       (else
	(car (ff:p/p->qr p u v)))))))

;;; ===================== Top-Level Factoring =====================
(define (u:sff a)
  (define ppoly (poly:primpart a))
  (poly:sort-factors
   (yuniv:square-free-factorization ppoly (car ppoly))))

(define (ff:diff n p v)
  (if (equal? (car p) v)
      (do ((i (- (length p) 1) (+ -1 i))
	   (r '() (cons (sym:sym n (* (+ -1 i) (list-ref p i))) r)))
	  ((< i 2) (cons v r)))
      (list v 0)))

(define (ff:gfroot n p var)
  (let ((d (ff:degree n p)))
    (if (zero? (sym:sym n d))
	(let* ((u (quotient d n))
	       (v (make-vector (+ 1 u) 0)))
	  (vector-set! v u 1)
	  (vector-set! v 0 1)
	  (cons var (vector->list v)))
	(slib:error 'ff:gfroot "polynomial has no root." p))))

(define (ff:sff p a)
  (let* ((v (car a))
	 (b (ff:diff p a v))
	 (output '()))
    (cond ((univ:zero? b)
	   (set! a (ff:gfroot p a v))
	   (set! output (append output (list (list (ff:sff p a) p)))))
	  (else
	   (let* ((c (ff:euclid-gcd p a b))
		  (w (ff:p/p p a c))
		  (y 0)
		  (z 0))
	     (let loop ((i 1))
	       (cond ((not (univ:one? w v))
		      (set! y (ff:euclid-gcd p w c))
		      (set! z (ff:p/p p w y))
		      (if (not (univ:one? z v))
			  (set! output (append output (list (list z i)))))
		      (set! w y)
		      (set! c (ff:p/p p c y))
		      (loop (+ 1 i)))))
	     (cond ((not (univ:one? c v))
		    (set! c (ff:gfroot p c v))
		    (set! output (append output
					 (list (list (ff:sff p c) p)))))))))
    output))

(define (ff:q-matrix p a)
  (define n #f)
  (set! a (ff:unorm p a))
  (set! n (univ:deg a))
  (let ((n-1 (+ -1 n))
	(q (make-array (A:fixZ32b 0) n n))
	(r (make-array (A:fixZ32b 0) n))
	(r1 (make-array (A:fixZ32b 0) n))
	(b (list->vector a))
	(u (* (+ -1 n) p)))
    (array-set! q 1 0 0)
    (array-set! r 1 0)
    (array-set! r1 1 0)
    (let loop ((i 1))
      (cond ((> i u) q)
	    (else
	     (array-set! r (* (vector-ref b 1) (- 0 (array-ref r1 n-1))) 0)
	     (let loop1 ((j 1))
	       (cond ((<= j n-1)
		      (array-set!
		       r (sym:sym p (- (array-ref r1 (+ -1 j))
				       (sym:sym p (* (vector-ref b (+ 1 j))
						     (array-ref r1 n-1)))))
		       j)
		      (loop1 (+ 1 j)))))
	     (array-map! r1 identity r)
	     (if (zero? (modular:normalize p i))
		 (array-map! (make-shared-array
			      q (lambda (j) (list (quotient i p) j)) n)
			     identity r))
	     ;;(ff:print i "  r = " r "  q = " q)
	     (loop (+ 1 i)))))))

;;; Converts a list of vectors to a vector of polynomials.
(define (basis->polys basis var)
  (list->vector
   (map (lambda (vr)
	  (do ((idx (+ -1 (car (array-dimensions vr))) (+ -1 idx))
	       (lst '() (if (and (null? lst) (eqv? 0 (array-ref vr idx)))
			    '()
			    (cons (array-ref vr idx) lst))))
	      ((<= idx 0)
	       (cons var (cons (array-ref vr 0) lst)))))
	basis)))

;;; MJT: Knuth's null-space-basis, slower but works.
;;;
;;; AJ: ff:null-space-basis does its calculations in symmetric modular
;;; form.  But Knuth's example is vanilla mod 13.  ff:null-space-basis
;;; should be reworked using *modulus*.
;;;
;;; The Art of Computer Programming : Seminumerical Algorithms (Vol 2)
;;;  by Donald Ervin Knuth
;;;  2nd Ed (1981) Addison-Wesley Pub Co; ISBN: 0-201-03822-6
;;; 4.6.2 Algorithm N (null-space-algorithm)
;;;
;;; P is prime modulus of coeffient field.
;;; Q-I is an N by N matrix of elements of Z mod P.
;;; Returns a list of {N - rank(Q-I)} basis vectors.
(define (ff:null-space-basis p Q-I)
  (define n (car (array-dimensions Q-I)))
  (define prot (sym:array-prototype p))
  (let ((m (make-array prot n n))
	(c (make-vector n -1))
	(ivec (array-indexes (make-array (A:fixN16b 1) n)))
	(basis '()))
    (array-map! ivec car ivec)
    (array:copy! m Q-I)
    (do ((k 0 (+ 1 k)))
	((>= k n) (reverse basis))
      (let ((j (do ((b 0 (+ 1 b)))
		   ((or (>= b n)
			(and (negative? (vector-ref c b))
			     (not (zero? (array-ref m k b))))) b))))
	(if (< j n)
	    (let ((muinv (* -1 (sym:invert p (array-ref m k j))))
		  (mcolj (make-shared-array m (lambda (a) (list a j)) n)))
	      (array-map! mcolj (lambda (x) (sym:sym p (* x muinv))) mcolj)
	      (vector-set! c j k)
	      (do ((i 0 (+ 1 i)))
		  ((>= i n))
		(if (not (= i j))
		    (let ((mcoli (make-shared-array
				  m (lambda (a) (list a i)) n))
			  (mki (array-ref m k i)))
		      (array-map! mcoli
				  (lambda (x y z)
				    (if (>= z k)
					(sym:sym p (+ x (* mki y)))
					x))
				  mcoli mcolj ivec)))))
	    (let ((vr (make-array prot n))
		  (cl (vector->list c)))
	      (array-map! vr
			  (lambda (i)
			    (let ((cl1 (memv i cl)))
			      (cond ((= i k) 1)
				    (cl1 (array-ref m k (- n (length cl1))))
				    (else 0))))
			  ivec)
	      (set! basis (cons vr basis))))))))

;;; -- > (u:factorz p7)
;     ERROR: vector-ref: Argument out of range 2
;     ;Evaluation took 1934 mSec (601 in gc) 19181 cells work, 6439 bytes other
;     > p7
;     ;Evaluation took 1 mSec (0 in gc) 2 cells work, 31 bytes other
;     (x 1 -3 -1 -3 1 -3 1)
;     > (math)
;     type qed; to return to scheme, type help; for help.
;     e5 : factoruz(1-3*x-x^2-3*x^3+x^4-3*x^5 +x^6);
;     poly = (#(x x #f () #f ()) 1 -3 -1 -3 1 -3 1)
;     e1 = (#(x x #f () #f ()) 1 -3 -1 -3 1 -3 1)
;     Arithmetic Error; Last expression lost.
;;; This problem is caused by berlekamp expecting to find factors when
;;; there are none (ie the polynomial is irreducible).	This problem
;;; comes from the ff:null-space-basis-gcl-bug procedure below.	 This
;;; is the only occurrence of this problem I am aware of.
;;; Fixed by using Knuth's null space basis algorithm above.
;;; Unfortunately, this one is faster.
(define (ff:null-space-basis-gcl-bug p Q-I var)
  (let* ((n (car (array-dimensions Q-I)))
	 (m (make-array '#(#f) n n)))
    (ff:print " Q-I = " Q-I)
    (array:copy! m Q-I)
    (do ((k 0 (+ 1 k)))
	((>= k n))
      (let ((i (do ((b k (+ 1 b)))
		   ((or (>= b n) (not (zero? (array-ref m k b)))) b))))
	(if (< i n)
	    (let* ((u	  (array-ref m k i))
		   (uinv  (sym:invert p u))
		   (mcoli (make-shared-array m (lambda (a) (list a i)) n))
		   (mcolk (make-shared-array m (lambda (a) (list a k)) n))
		   (temp  (make-array '#(#f) n)))
	      (array-map! mcoli (lambda (x) (sym:sym p (* x uinv))) mcoli)
	      (array-map! temp	identity mcoli)
	      (array-map! mcoli identity mcolk)
	      (array-map! mcolk identity temp)
	      (do ((j 0 (+ 1 j)))
		  ((>= j n))
		(if (not (= j k))
		    (let ((mcolj (make-shared-array
				  m (lambda (a) (list a j)) n))
			  (mkj (array-ref m k j)))
		      (array-map! mcolj
				  (lambda (x y)
				    (sym:sym p (- x (sym:sym p (* mkj y)))))
				  mcolj mcolk))))
;;;(ff:print " m = " m)
	      ))))
    (let ((mdiag (make-shared-array m (lambda (a) (list a a)) n)))
      (array-map! mdiag (lambda (x) (sym:sym p (+ -1 x))) mdiag))
;;;    (ff:print "  After subtraction of 1 along the diagonal,	m = " m)
;;;	 (array-map! m (lambda (x) (sym:sym p (* -1 x))) m)
;;;    (ff:print "  After multiplication of -1 along the diagonal,  m = " m)
    (let (;;(i 0)
	  (ret '()))
      (let loop1 ((j 0))
	(if (< j n)
	    (let ((zerow (make-vector n 0)))
	      (let loop2 ()
		(if (< j n)
		    (let ((mrow (make-shared-array
				 m (lambda (a) (list j a)) n)))
		      (cond ((array-equal? mrow zerow)
			     (set! j (+ 1 j)) (loop2))))))
	      (if (< j n)
		  (let ((v (make-vector n 0))
			(mrowj (make-shared-array
				m (lambda (a) (list j a)) n)))
		    ;;(set! i (+ 1 i))
;;;			 (ff:print " mrowj = " mrowj)
		    (array:copy! v mrowj)
;;;			 (ff:print " v = " v)
		    (set! ret (cons v ret))
		    (loop1 (+ 1 j)))))))
;;;	  (ff:print " ret = " ret)
      (map (lambda (x) (ff:unorm p (cons var (vector->list x))))
	   (reverse ret)))))

;;; return an ordered list of elements of Z mod n (symmetric)
(define (ff:generate-field n)
  (define b (quotient (+ -1 n) 2))
  (do ((idx (+ -1 n) (+ -1 idx))
       (lst '() (cons (- idx b) lst)))
      ((negative? idx) lst)))

;;; Return a sorted list of factors of a mod p, where a is square free
;;; and p is a prime number.
(define (ff:berlekamp p a)
  (let* ((q	(ff:q-matrix p a))
	 (n	(car (array-dimensions q)))
	 (var	(car a))
	 (qdiag (make-shared-array q (lambda (a) (list a a)) n)))
    (array-map! qdiag (lambda (x) (sym:sym p (+ -1 x))) qdiag)
    (fluid-let ((*modulus* (symmetric:modulus p)))
      (let* ((vs (basis->polys (ff:null-space-basis p q) var))
	     (factors (list a))
	     (ffp (ff:generate-field p))
	     (k (vector-length vs)))
	(do ((r 1 (+ 1 r)))
	    ((not (< (length factors) k))
	     (poly:sort-factors factors))
	  (do ((us factors (if start factors (cdr us)))
	       (start #f #f))
	      ((not (and (< (length factors) k) (not (null? us)))))
	    (let ((u (car us)))
	      (let loop3 ((ss ffp))
		(cond ((and (not (null? ss)) (< (length factors) k))
		       ;;(print '!)
		       (let ((g (ff:euclid-gcd ;poly:gcd
				 p
				 (poly:- (vector-ref vs r) (car ss)) ;(list var )
				 u)))
			 (cond ((and (not (equal? g u))
				     (not (univ:one? g var)))
				(set! factors (delete u factors))
				(set! u (ff:p/p p u g))
				(set! factors
				      (delete '()
					      (append factors
						      (list u)
						      (list g))))
				(set! start #t))
			       (else (loop3 (cdr ss)))))))))))))))

;;; Partial fraction expansion of a rational univariate polynomial
;;; The denominator, dr, must be square free.
(define (u:partial-fraction-expand nr dr)
  (let* ((drfs1 (u:factorq dr))
	 (drfs (remove-exponents drfs1 '()))
	 (p 3)
	 (k 5)
	 (ss   (hen:diophantine drfs (list (car (car drfs)) 1) p k)))
    (let ((res (map (lambda (x y) (make-rat (poly:* nr x) y)) ss drfs)))
      (ff:print res)
      res)))

(define (remove-exponents fs fs1)
  (cond
    ((null? fs) fs1)
    ((number? (caar fs))
      (ff:print "n " (caar fs))
      (remove-exponents (cdr fs) (fs1)))
    (else
      (ff:print "e " (caar fs))
      (remove-exponents (cdr fs) (append (caar fs) fs1)))))

(define (ff:check-arg e)
  (cond ((not (poly:univariate? e))
	 (bltn:error 'not-a-univariate-polynomial e))))

(define (ff:check-prime n)
  (cond ((not (prime? n))
	 (bltn:error 'not-a-prime-number n))))

(defbltn 'sff
  (lambda (poly)
    (let ((e (licit->polxpr poly)))
      (if (not (eqv? 1 (unitcan (univ:cont e))))
	(bltn:error 'not-a-primitive-polynomial poly)
	(u:sff e)))))

(defbltn 'usff
  (lambda (poly)
    (let ((e (licit->polxpr poly)))
      (ff:check-arg e)
      (cond
       ((not (equal? e (u:primz e)))	   ;this test should be replaced
	(bltn:error 'not-a-primitive-polynomial poly))
       (else (u:sff e))))))

(defbltn 'ffsff
  (lambda (poly pn . k1)
    (let ((p (licit->polxpr poly))
	  (n (licit->polxpr pn))
	  (k (licit->polxpr (if (null? k1) 1 (car k1)))))
      (ff:check-arg p)
      (ff:check-prime n)
      (cond
       ((not (ff:monic? n p))
	(bltn:error 'not-monic-mod-n p))
       ((not (> k 0))
	(bltn:error 'not-greater-than-zero k))
       (else (ff:sff (expt n k) p))))))

(defbltn 'berl
  (lambda (poly pn)
    (let ((p (licit->polxpr poly))
	  (n (licit->polxpr pn)))
      (ff:check-arg p)
      (ff:check-prime n)
      (cond
       ((not (ff:monic? n p))
	(bltn:error 'not-monic-mod-n p))
       ((not (= (univ:deg p) (ff:degree n p)))
	(bltn:error 'not-same-degree-when-reduced-mod-n (ff:norm p n)))
       (else (ff:berlekamp n p))))))

(defbltn 'parfrac
  (lambda (poly)
    (let ((e1 (expr:normalize poly)))
      (u:partial-fraction-expand (num e1) (denom e1)))))
