;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 2024 Aubrey Jaffer.
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

;;; This implements a Grobner sort of basis where all the polynomials
;;; are factored.  Thanks to Sarah Jaffer for help designing the
;;; algorithm.

(require 'common-list-functions)

(define mono:null (make-monomial 1 '() '()))

(define (poly:leading-monomial poly)
  (define max-lm mono:null)
  (define (maxit! lm)
    (if (mono:> lm max-lm) (set! max-lm lm)))
  (define (swim vvars pows coeffs idx)
    (cond ((number? (car coeffs))
	   (maxit! (make-monomial (car coeffs) vvars (cons idx pows))))
	  (else
	   (dive vvars (cons idx pows) (car coeffs))))
    (cond ((null? (cdr coeffs)))
	  (else (swim vvars pows (cdr coeffs) (+ 1 idx)))))
  (define (dive vars pows poly)
    (if (number? (cadr poly))
	(maxit! (make-monomial (cadr poly) vars pows))
	(dive vars pows (cadr poly)))
    (swim (cons (car poly) vars) pows (cddr poly) 1))
  (cond ((number? poly))
	(else (dive '() '() poly)))
  max-lm)

(define (mono:overlap? obj1 obj2)
  (let lp ((vars1 (mono:vars obj1))
	   (vars2 (mono:vars obj2)))
    (cond ((null? vars1) #f)
	  ((null? vars2) #f)
	  ((var:> (car vars2) (car vars1))
	   (lp (cdr vars1) vars2))
	  ((var:> (car vars1) (car vars2))
	   (lp vars1 (cdr vars2)))
	  ((eq? (car vars1) (car vars2)))
	  (else (math:error 'duplicate-var? (car vars1) (car vars2))))))

(define (mono:divides? obj1 obj2)
  (let lp ((vars1 (mono:vars obj1))
	   (pows1 (mono:pows obj1))
	   (vars2 (mono:vars obj2))
	   (pows2 (mono:pows obj2)))
    (cond ((null? vars1) #t)
	  ((null? vars2) #f)
	  ((var:> (car vars2) (car vars1))
	   (lp (cdr vars1) (cdr pows1) vars2 pows2))
	  ((var:> (car vars1) (car vars2))
	   (lp vars1 pows1 (cdr vars2) (cdr pows2)))
	  ((not (eq? (car vars1) (car vars2)))
	   (math:error 'duplicate-var? (car vars1) (car vars2)))
	  ((> (car pows1) (car pows2)) #f)
	  (else
	   (lp (cdr vars1) (cdr pows1) (cdr vars2) (cdr pows2))))))
