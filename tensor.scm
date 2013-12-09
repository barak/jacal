;; TENSOR.SCM -- Tensor-like support functions for JACAL
;; Copyright (C) 1993 Jerry D. Hedden
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

(require 'common-list-functions)

; Does not support the notion of contra-/covariant indices.
;  Users must keep track of this information themselves.

; Assumes that all matrices are "proper" (i.e., that all "dimensions"
;  of the matrix are the same length, e.g., 4x4x4) and "compatible"
;  (e.g., a 3x3 matrix is not compatible with a 4x4x4 matrix).


(definfo 'indexshift
  "Shifts an index within a tensor")

(definfo 'indexswap
  "Swaps two indices within a tensor")

(definfo 'contract
  "Tensor contraction")

(definfo 'tmult
  "Tensor multiplication")


; helper function to determine the "rank" of a tensor
(define (tnsr:rank m)
  (let rnk ((rank  0)
	    (mm    m))
    (if (bunch? mm)
      (rnk (+ 1 rank) (car mm))
      rank)))


(defbltn 'indexshift
  (lambda (m . args)
    (let ((rank  (tnsr:rank m)))
      (cond
	((= rank 0)
	   ; scalar -- no-op
	   m)
	((= rank 1)
	   ; vector -- returns a pseudo-tensor
	   (map list m))
	((or (= rank 2) (null? args))
	   ; matrix -- transpose
	   (apply map list m))
	(else
	   ; tensor
	   ; set and constrain the "from" and "to" positions
	   (let* ((a  (car args))
		  (b  (if (null? (cdr args))
			(+ 1 a)
			(cadr args))))
	     (if (< a 1)
	       (set! a 1)
	       (if (> a rank)
		 (set! a rank)))
	     (if (< b 1)
	       (set! b 1)
	       (if (> b rank)
		 (set! b rank)))
	     (if (= a b)
	       (if (= a rank)
		 (set! a (+ -1 b))
		 (set! b (+ 1 a))))

	     (if (< a b)
	       ; index shift right
	       (let isr1 ((ma  m)
			  (nn  1))
		 (if (< nn a)
		   (map (lambda (mm) (isr1 mm (+ 1 nn))) ma)
		   (let isr2 ((mb  ma)
			      (aa  (+ 1 a)))
		     (if (= aa b)
		       (apply map list mb)
		       (map (lambda (mm) (isr2 mm (+ 1 aa)))
			    (apply map list mb))))))
	       ; index shift left
	       (let isl1 ((ma  m)
			  (nn  1))
		 (if (< nn b)
		   (map (lambda (mm) (isl1 mm (+ 1 nn))) ma)
		   (let isl2 ((mb  ma)
			      (bb  (+ 1 b)))
		     (if (= bb a)
		       (apply map list mb)
		       (apply map list (map (lambda (mm) (isl2 mm (+ 1 bb)))
					    mb)))))))))))))


(defbltn 'indexswap
  (lambda (m . args)
    (let ((rank  (tnsr:rank m)))
      (cond
	((= rank 0)
	   ; scalar -- no-op
	   m)
	((= rank 1)
	   ; vector -- returns a pseudo-tensor
	   (map list m))
	((or (= rank 2) (null? args))
	   ; matrix -- transpose
	   (apply map list m))
	(else
	   ; tensor
	   ; set and constrain the indices to be swapped
	   (let* ((a  (car args))
		  (b  (if (null? (cdr args))
			(+ 1 a)
			(cadr args))))
	     (if (< b a)
	       (let ((c  a))
		 (begin (set! a b)
			(set! b c))))
	     (if (< a 1)
	       (begin (set! a 1)
		      (if (<= b a)
			(set! b 2)))
	       (if (> b rank)
		 (begin (set! b rank)
			(if (<= b a)
			  (set! a (+ -1 b))))
		 (if (= a b)
		   (if (= a rank)
		     (set! a (+ -1 b))
		     (set! b (+ 1 a))))))

	     ; perform the swapping operation
	     (let swap1 ((ma  m)
			 (n   1))
	       (if (< n a)
		   (map (lambda (mm) (swap1 mm (+ 1 n))) ma)
		   (let swap2 ((mb  ma)
			       (aa  (+ 1 a)))
		     (if (= aa b)
			 (apply map list mb)
			 (apply map list
				    (map (lambda (mm) (swap2 mm (+ 1 aa)))
					 (apply map list mb)))))))))))))


; helper function for the contraction operation
;  sums the diagonal elements of a matrix
(define (tnsr:contract m)
  (let cxt ((mm  (map cdr (cdr m)))
	    (ss  (car (car m))))
    (if (null? mm)
      ss
      (cxt (map cdr (cdr mm)) (app* $1+$2 ss (car (car mm)))))))


(defbltn 'contract
  (lambda (m . args)
    (let ((rank  (tnsr:rank m)))
      (cond
	((= rank 0)
	   ; scalar -- no-op
	   m)
	((= rank 1)
	   ; vector -- sum elements
	   (reduce (lambda (x y) (app* $1+$2 x y)) m))
	((= rank 2)
	   ; matrix -- sum diagonal elements
	   (tnsr:contract m))
	(else
	   ; tensor
	   ; set and constrain the indices for the contraction operation
	   (let* ((a  (car args))
		  (b  (if (null? (cdr args))
			(+ 1 a)
			(cadr args))))
	     (if (< b a)
	       (let ((c  a))
		 (begin (set! a b)
			(set! b c))))
	     (if (< a 1)
	       (begin (set! a 1)
		      (if (<= b a)
			(set! b 2)))
	       (if (> b rank)
		 (begin (set! b rank)
			(if (<= b a)
			  (set! a (+ -1 b))))
		 (if (= a b)
		   (if (= a rank)
		     (set! a (+ -1 b))
		     (set! b (+ 1 a))))))

	   ; perform the contraction operation
	   (let cxt1 ((ma  m)
		      (nn  1))
	     (if (< nn a)
	       (map (lambda (mm) (cxt1 mm (+ 1 nn))) ma)
	       (let cxt2 ((mb  ma)
			  (aa  (+ 1 a)))
		 (if (< aa b)
		   (map (lambda (mm) (cxt2 mm (+ 1 aa))) (apply map list mb))
		   (let cxt3 ((mc  mb)
			      (bb  b))
		     (if (= bb rank)
		       (tnsr:contract mc)
		       (map (lambda (mm) (cxt3 mm (+ 1 bb)))
			    (apply map list (map (lambda (mx)
						   (apply map list mx))
						 mc)))))))))))))))


(defbltn 'tmult
  (lambda (m1 m2 . args)
    (let ((r1  (tnsr:rank m1))
	  (r2  (tnsr:rank m2)))
      (cond
	((or (= r1 0) (= r2 0))
	   ; scalar multiplication
	   (app* $1*$2 m1 m2))
	((null? args)
	   ; outerproduct -- scalar multiplication of the second
	   ;  tensor by each element of the first tensor
	   (let outerproduct ((ma  m1)
			      (r   1))
	     (if (< r r1)
	       (map (lambda (mm) (outerproduct mm (+ 1 r))) ma)
	       (map (lambda (x) (app* $1*$2 x m2)) ma))))
	(else
	   ; innerproduct
	   ; set and contrain indices to be used
	   (let* ((a  (car args))
		  (b  (if (null? (cdr args))
			a
			(cadr args))))
	     (if (< a 1)
	       (set! a 1)
	       (if (> a r1)
		 (set! a r1)))
	     (if (< b 1)
	       (set! b 1)
	       (if (> b r2)
		 (set! b r2)))

	     ; perform the multiplication operation
	     (let mult1 ((ma1  m1)
			 (n1   1))
	       (if (< n1 a)
		 ; find index to multiply in first tensor
		 (map (lambda (mm) (mult1 mm (+ n1 1))) ma1)
		 (let mult2 ((mb1  ma1)
			     (a1   a))
		   (if (< a1 r1)
		     ; shift index to last position in first tensor
		     (map (lambda (mm) (mult2 mm (+ a1 1)))
			  (apply map list mb1))
		     (let mult3 ((ma2  m2)
				 (n2   1))
		       (if (< n2 b)
			 ; find index to multiply in second tensor
			 (map (lambda (mm) (mult3 mm (+ n2 1))) ma2)
			 (let mult4 ((mb2  ma2)
				     (a2   b))
			   (if (< a2 r2)
			     ; shift index to last position in second tensor
			     (map (lambda (mm) (mult4 mm (+ a2 1)))
				  (apply map list mb2))
			     ; the actual multiplication is done here
			     (reduce (lambda (x y) (app* $1+$2 x y))
				     (map (lambda (x y) (app* $1*$2 x y))
					  mb1 mb2))))))))))))))))
