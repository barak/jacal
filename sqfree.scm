;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1997 Aubrey Jaffer.
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

;;; Functions involved with square freeness.
(define (poly:square-free-var p var)
  (poly:/ p (poly:gcd p (poly:diff p var))))

(define (poly:square-and-num-cont-free p)
  (if (number? p) (if (zero? p) p 1)
      (poly:* (poly:square-and-num-cont-free (univ:cont p))
	      (poly:/ p (poly:gcd p (poly:diff p (car p)))))))

(define (poly:sqfr-var c var)
  (poly:sqfr-split c (poly:diff c var)))

(define (poly:sqfr-all c)
  (poly:sqfr-split c (poly:diff-all c)))

;;; This algorithm is due to:
;;; Multivariate Polynomial Factorization
;;; David R. Musser
;;; Journal of the Association for Computing Machinery
;;; Vol 22, No. 2, April 1975
(define (poly:sqfr-split c splitter)
  (let ((d '()) (aj '()) (b (poly:gcd c splitter))) ; changed #f's to ()
    (do ((b b (poly:/ b d))
         (a (poly:/ c b) d))
        ((number? b)
         (if (one? b)
             (cons a aj)                ; nreverse removed
             (cons b (cons a aj))))     ; nreverse removed
      (set! d (poly:gcd a b))
      (set! a (poly:/ a d))             ; 'a' is used as a temporary variable
      (if (not (eqv? a 1))              ; keeps extraneous `1's
        (set! aj (cons a aj))))))       ;  out of the results list

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
(define (univ:square-free-factorization a v)
  (define b (poly:diff a v))
  (define output '())
  (define y 0)
  (define z 0)
  (let* ((c (unitcan (poly:gcd a b)))
	 (w (poly:/ a c)))
    (let loop ((i 1))
      (cond ((eqv? 1 c) (cons (list w i) output))
	    (else (set! y (unitcan (poly:gcd w c)))
		  (set! z (poly:/ w y))
;;;		  (ff:print "y = " y " w = " w " c = " c " z = " z)
		  (if (not (number? z))
		      (set! output (cons (list z i) output)))
		  (set! w y)
		  (set! c (poly:/ c y))
;;;		  (ff:print "c = " c)
		  (loop (+ 1 i)))))))

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
	 (math:print 'yuniv:square-free-factorization v)
	 (math:print a)))
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

(define (univ:split-all poly) (univ:split poly (poly:vars poly)))

(define (sqfr-free-var p var)
  (poly:gcd p (poly:subst0 var p)))

(define (sqfr:test)
  (define x 'x)
  (test (list (list x -1 -2) (list x -1 0 1))
        poly:sqfr-all
        (list x -1 -4 -3 4 4)))

;;(require 'debug-jacal) (trace yuniv:square-free-factorization)
;;(trace poly:sqfr-all poly:diff-all poly:sqfr-split)
