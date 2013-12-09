;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1995 Aubrey Jaffer.
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

;;; delete all occurrences of a from b non-destructively
(define (del a b acc)
  (cond ((null? a) b)
	((null? b) acc)
	((equal? a (car b))
	  (del a (cdr b) acc))
	(else (del a (cdr b) (append acc (list (car b)))))))

(define (factorial n)
  (letrec ((factorial1
	     (lambda (i acc)
	       (if (< i 2) acc (factorial1 (+ -1 i) (* acc i))))))
    (factorial1 n 1)))

(define (cart-prod choices)
  (if (null? choices) '(())
      (let* ((choice (car choices)))
	(apply append
	       (map (lambda (tuple)
		      (map (lambda (elt)
			     (cons elt tuple))
			   choice))
		    (cart-prod (cdr choices)))))))

;;;; From Mike Thomas:
;;; Return all the unique subsets of size n obtainable from the list l

(define (combinations l n)
  (letrec ((combs1
	    (lambda (l1 l2 n acc)
	      (let* ((l1l (length l1))
		     (l2l (length l2))
		     (sumls (+ l1l l2l)))
		(cond
		 ((< sumls n) acc)
		 ((= sumls n) (append acc (list (append l1 l2))))
		 ((= l1l (+ -1 n))
		  (append acc (map (lambda (x) (append l1 (list x))) l2)))
		 (else (apply append
			      (map (lambda (x y)
				     (combs1 (append l1 (list x)) y n acc))
				   l2 (make-ends (cdr l2) '()))))))))
	   (make-ends
	    (lambda (l acc)
	      (if (null? l)
		  (append acc '(()))
		  (make-ends (cdr l) (append acc (list l)))))))
    (combs1 '() l n '())))

;;; (UNIQUES L) removes any element in each member of a list of lists
;;; that is present in any other member of the list of lists,
;;; preserving order.

;;; (BUILD-UNIQUE-ITEMS L1 L2) builds a list of the members of l1
;;; which are not members of each list in the list l2.

(define (uniques l)
  (letrec ((unis
	    (lambda (a b c)
	      (cond ((null? b) c)
		    (else
		     (unis (cons a (list (car b)))
			   (cdr b)
			   (let ((l1 (car b))
				 (l2 (append a (cdr b))))
			     (for-each
			      (lambda (x)
				(if (some (lambda (sl) (member x sl)) l2)
				    (set! l1 (del x l1 '())))) l1)
			     (append
			      c (list l1)))))))))
    (unis '() l '())))
