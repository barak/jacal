;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1990, 1992, 1993, 1997 Aubrey Jaffer.
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


;;; This section deals with functions which are non-algebraic or
;;; unspecified.  These functions are canonicalized with regard to
;;; symmetric, antisymmetric, even, odd, distributive and anti-distributive
;;; properties on a per argument basis.  This does not canonicalize
;;; functions because there are many other functional properties.  What is
;;; possible in this area?  What other properties are important to include?

;;; f(x,y,z) = f(y,x,z)		f is symmetric in arguments 1 and 2.
;;; f(x,y,z) = -f(y,x,z)	f is anti-symmetric in arguments 1 and 2.
;;; f(x,y) = f(-x,y)		f is even in argument 1.
;;; f(x,y) = -f(-x,y)		f is odd in argument 1.
;;; f(g(x,y),z) = g(f(x,z),y)	f and g distribute over argument 1.
;;; f(g(x,y),z) = -g(f(x,z),y)	f and g anti-distribute over argument 1.
;;; f(f(x,z),z) = f(x,z)	f idempotent over argument 1.

;;; When one of the functions in a distributive pair is '+' (and '-') the
;;; system knows that constant factors can be pulled out.  When one of the
;;; functions in a distributive pair is '*' the system knows that it applies
;;; to '/' and exponentiated terms also.

;;; Normalization procedure:

;;; First, apply distributive rules to pairs (to sets?) of distributive
;;; functions in such a was as to push the lower priority function to the
;;; bottom (smallest terms).  '+' followed by '*' and exponentiation are the
;;; highest priority functions.

;;; Colapse any idempotent cases.

;;; Starting with the innermost terms conditionally negate arguments in
;;; order to make the highest order term of the argument positive (negating
;;; the function if odd).

;;; Finally, sort sets of symmetric arguments by highest order term ranking.

(require 'sort)
(require 'common-list-functions)

(defbltn 'symmetrical
  (lambda (func . symmetry-lists)
    (let* ((f (expl->var func))
	   (osls (and f (or (func-syms f) '()))))
      (for-each (lambda (sl)
		  (cond ((or (not (every number? sl))
			     (has-duplicates? sl))
			 (bltn:error 'symmetrical sl))
			(else (set sl (sort sl <))
			      (cond ((member sl osls)
				     (bltn:warn 'symmetrical 'already-knew sl))

		  (func-set-syms! f v)))
			))))))
