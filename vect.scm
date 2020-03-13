;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993 Aubrey Jaffer.
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

(require 'rev4-optional-procedures)
(require 'common-list-functions)

(define (copymatrix m)
  (if (matrix? m)
      (map copy-list m)
      (math:error 'not-a-matrix:- m)))

(define (row-vector? a)
  (and (bunch? a)
       (or (null? a)
	   (and (null? (cdr a)) (bunch? (car a))))))

(define (column-vector? a)
  (and (matrix? a)
       (every (lambda (r) (null? (cdr r))) a)))

(define (matrix? a)
  (and (bunch? a)
       (not (null? a))
       (bunch? (car a))
       (let ((len (length (car a))))
	 (and (every (lambda (r) (and (bunch? r) (= (length r) len)))
		     (cdr a))
	      len))))

(define (matrix . lst)
  (if (matrix? lst) lst (matrix lst)))

(define (mtrx:butnth n lst)
  (cond ((null? lst) (math:error 'coordinate-out-of-range:- n " " lst))
	((zero? n) (cdr lst))
	(else (cons (car lst)
		    (mtrx:butnth (+ -1 n) (cdr lst))))))

(define (mtrx:minor m i j)
  (mtrx:butnth (+ -1  i)
	  (map (lambda (x) (mtrx:butnth (+ -1 j) x))
	       m)))

(define (transpose m)
  (cond ((matrix? m)
	 (apply map list m))
	((bunch? m) (map list m))
	(else m)))

(define (mtrx:square? m)
  (if (not (matrix? m))
      #f
      (let ((rows (length m)))
	(every (lambda (r) (= (length r) rows)) m))))

(define (mtrx:genmatrix fun i2 j2 i1 j1)
  (do ((i i2 (+ -1 i))
       (mat '()
	    (cons
	     (do ((j j2 (+ -1 j))
		  (row '() (cons (app* fun i j) row)))
		 ((< j j1) row))
	     mat)))
      ((< i i1) mat)))

;;;Create a matrix of indeterminates

(define (nmatrix var . rst)
  (apply genmatrix
	 (lambda subs (apply rapply var subs))
	 rst))

(define (row? a)
  (and (bunch? a)
       (notevery bunch? a)))

(define (dotproduct a b)
  (let ((cols (matrix? a))
	(colbs (matrix? b)))
    (cond ((or (and cols (bunch? b)) (and colbs (bunch? a)))
	   (if (not (and (eqv? cols colbs) (eqv? (length a) (length b))))
	       (math:error 'dotproduct-of-unequal-size-matrices:- a
			   " " b))
	   (reduce (lambda (x y) (app* $1+$2 x y))
		   (reduce (lambda (x y) (app* $1+$2 x y))
			   (map (lambda (x y) (app* $1*$2 x y))
				a b))))
	  ((and (row? a) (row? b))
	   (reduce (lambda (x y) (app* $1+$2 x y))
		   (map (lambda (x y) (app* $1*$2 x y))
			a b)))
	  ((bunch? a) (map (lambda (x) (app* $1*$2 x b))
			   a))
	  ((bunch? b) (map (lambda (x) (app* $1*$2 a x))
			   b))
	  (else (app* $1*$2 a b)))))

(define (ncmult a b)
  (let ((cols (matrix? a))
	(colbs (matrix? b)))
    (cond ((or (and cols (bunch? b)) (and colbs (bunch? a)))
	   (if (not colbs) (set! b (list b)))
	   (if (not (= (or cols (length a)) (length b)))
	       (math:error 'matrix-product-of-unequal-size-matrices:- a
			   " " b))
	   (or cols (set! a (matrix a)))
	   (map (lambda (arow)
		  (apply map
			 (lambda bcol
			   (dotproduct bcol arow))
			 b))
		a))
	  (else (deferop _ncmult a b)))))

(define (mtrx:expt a pow)
  (if (not (mtrx:square? a)) (math:error 'not-a-square-matrix:-- a))
  (cond ((zero? pow) 1)
	((one? pow) a)
	((negative? pow) (mtrx:expt (mtrx:inverse a) (- pow)))
	(else (ipow-by-squaring a (+ -1 pow) a ncmult))))

(define (crossproduct a b)
  (if (and (row? a) (row? b))
      (cond ((not (= (length a) (length b)))
	     (math:error 'crossproduct 'unequal-length-vectors a b))
	    ((= 2 (length a))
	     (app* $1-$2
		   (app* $1*$2 (car a) (cadr b))
		   (app* $1*$2 (car b) (cadr a))))
	    ((= 3 (length a))
	     (list
	      (app* $1-$2
		    (app* $1*$2 (cadr a) (caddr b))
		    (app* $1*$2 (cadr b) (caddr a)))
	      (app* $1-$2
		    (app* $1*$2 (caddr a) (car b))
		    (app* $1*$2 (caddr b) (car a)))
	      (app* $1-$2
		    (app* $1*$2 (car a) (cadr b))
		    (app* $1*$2 (car b) (cadr a)))))
	    (else (math:error 'crossproduct 'funny-length-vectors a b)))
      (dotproduct a b)))

(define (mtrx:scalarmatrix n x)
  (do ((i 1 (+ 1 i))
       (rows (list (nconc (make-list (+ -1 n) 0) (list x)))
	     (cons (append (cdar rows) (list 0)) rows)))
      ((>= i n) rows)))

(define (mtrx:diagmatrix diag)
  (set! diag (reverse diag))
  (do ((i (+ -1 (length diag)) (+ -1 i))
       (j 0 (+ 1 j))
       (diag diag (cdr diag))
       (rows '()
	     (cons (nconc (make-list i 0) (list (car diag)) (make-list j 0))
		   rows)))
      ((null? diag) rows)))

(define (determinant m)
  (if (not (mtrx:square? m)) (math:error 'determinant-of-non-square-matrix m))
  (letrec
      ((sign 1)
       (ds (lambda (m)
	     (cond ((null? m) '())
		   ((not (expr:0? (caar m)))
		    (set! sign
			  (if (negative? sign) (app* _-$1 (caar m)) (caar m)))
		    (map (lambda (ro)
			   (cond ((expr:0? (car ro)) (cdr ro))
				 (else (app* $1-$2*$3
					     (cdr ro)
					     (cdar m)
					     (app* $1/$2 (car ro) (caar m))))))
			 (cdr m)))
		   ((null? (cdr m))
		    #f)
		   (else
		    (set! sign (- sign))
		    (let ((submat (ds (cdr m))))
		      (if (not submat) #f
			  (cons (cdar m) submat))))))))
    (cond ((null? (cdr m)) (caar m))
	  ((null? (cddr m)) (crossproduct (car m) (cadr m)))
	  (else (let ((subdet (ds m)))
		  (if (not subdet) 0
		      (app* $1*$2 sign (determinant subdet))))))))

(define (charpoly m var)
  (determinant (app* $1-$2
		     m
		     (mtrx:scalarmatrix (length m) var))))

(define (cofactor m i j)
  (let ((det (determinant (mtrx:minor m i j))))
    (if (odd? (+ i j))
	(app* _-$1 det)
	det)))

(define (mtrx:inverse ra)
  (app* $1/$2
	(mtrx:genmatrix (lambda (i j) (cofactor ra j i))
			(length ra) (length ra) 1 1)
	(determinant ra)))

(define (coefmatrix eqns vars)
  (map
   (lambda (eq)
     (map (lambda (var)
	    (let ((c (coeff eq var 1)))
	      (set! eq (coeff eq var 0))
	      c))
	  vars))
   eqns))

(define (augcoefmatrix eqns vars)
  (map
   (lambda (eq)
     (nconc
      (map (lambda (var)
	     (let ((value (coeff eq var 1)))
	       (set! eq (coeff eq var 0))
	       value))
	   vars)
      (list eq)))
   eqns))

;;; This algorithm taken from:
;;; Knuth, D. E.,
;;; The Art Of Computer Programming, Vol. 2: Seminumerical Algorithms,
;;; Addison Wesley, Reading, MA 1969.
;;; pp 425-426
(define (rank m)
  (if (not (matrix? m)) (math:error 'not-a-matrix:-- m))
  (let* ((cols (matrix? m))
	 (n (length m))
	 (c (make-list n -1))
	 (r 0))
    (cond ((null? cols) (set! m (list m))
			(set! n 1))
	  ((> cols n) (set! m (copymatrix m)))
	  (else (set! m (transpose m))
		(set! n cols)))
    (do ((k 0 (+ 1 k)))
	((>= k n) (- n r))
      (let* ((l #f)
	     (j
	      (do ((j 0 (+ 1 j)))
		  ((or l (>= j n)) l)
		(if (and (< (list-ref c j) 0)
			 (not (poly:0? (list-ref (list-ref m k) j))))
		    (set! l j))))
	     (rowj (and j (list-ref m j))))
	(cond (j
	       (set! rowj (app* _-$1/$2
				rowj
				(list-ref rowj k)))
	       (set-car! (list-tail m j) rowj)
	       (do ((restrows m (cdr restrows)))
		   ((null? restrows))
		 (if (not (eq? (car restrows) rowj))
		     (set-car! restrows
			       (app* $1*$2+$3
				     rowj
				     (list-ref (car restrows) k)
				     (car restrows)))))
	       (set-car! (list-tail c j) k))
	      (else (set! r (+ 1 r))))))))
