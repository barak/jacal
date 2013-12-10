;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 2005, 2010 Aubrey Jaffer.
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

(require 'alist)
(define info:defs '())

(define info:get
  (let ((hassq (predicate->asso eq?)))
    (lambda (sym)
      (let ((p (hassq sym info:defs))) (and p (cdr p))))))

(define (info:describe x)
  (let ((info (info:get x)))
    (cond (info
	   (for-each (lambda (i)
		       (cond ((string? i) (display i))
			     ((sexp? i) (write-sexp i *output-grammar*))
			     (else (eval-error 'bad-info-entry)))
		       (newline))
		     (or (info:get x) '()))
	   #t)
	  (else #f))))

(define (info:example x)
  (let ((info (and (expl:var? x) (info:get (var:sexp (expl->var x))))))
    (cond
     ((and info (not (null? info)))
      (push-modes)
      (let ((ans
	     (do ((info (or info '()) (cdr info)))
		 ((or (null? info) (not (string? (car info))))
		  (cond ((null? info) (math:warn 'no-example-for x) novalue)
			(else (write-sexp (car info) *input-grammar*)
			      (newline)
			      (sexp->math (car info))))))))
	(pop-modes)
	ans))
     (else (math:warn 'no-example-for x)))))

(define definfo
  (let ((heqput! (alist-associator eq?)))
    (lambda (sym . info)
      (cond ((list? sym)
	     (for-each (lambda (v) (apply definfo v info)) sym))
	    (else
	     (set! info:defs (heqput! info:defs sym info))
	     sym)))))

(define (defflaginfo sym . info)
  (apply definfo sym (string-append "flag: " (car info)) (cdr info)))

(define (defboolinfo sym . info)
  (apply definfo sym (string-append "boolean flag: " (car info)) (cdr info)))

(defflaginfo 'ingrammar "grammar for input expressions")
(defflaginfo 'outgrammar "grammar for output expressions")
(defflaginfo 'echogrammar "grammar for echoing input expressions")
(defflaginfo 'grammars "list of available grammars")
(defboolinfo 'horner "output expressions using Horner's rule")
(defboolinfo 'trace "print trace of all variable eliminations")
(defboolinfo 'debug "break on soft errors")
(defboolinfo 'linkradicals
  "make all identical looking radicals the same object")
(defflaginfo 'version "Version number of Jacal")
(defflaginfo 'all "list of available flags")
(defflaginfo 'prompt "template for system-generated names")
(defflaginfo 'page
  "turns pagination on or off or to a given number of lines")
(defflaginfo 'width
  "set width to a given number of characters, ON for default")
(defflaginfo 'priority "ordering priority of variables")

(definfo 'set
  "Set options."
  '(set outgrammar scheme))

(definfo 'show
  "Show options."
  '(show outgrammar))

(definfo 'commands
  "List of commands"
  '(commands))

(definfo '%
  "Last non-null expression")

(definfo 'depends "What this function or expression depends on"
  '(depends (f (^ x (/ 1 2)) (* y a))))

(definfo 'args
  "The arguments to this function"
  '(args (f (^ x (/ 1 2)) (* y a))))

(definfo 'describe
  "Describes object or command"
  '(describe describe))

(definfo 'example
  "Gives an example of the use of a command"
  '(example '(commands)))

(definfo 'terms
  "Describes the License applying to this program"
  '(terms))

(definfo 'verify
  "Verify if arguments are equivalent"
  '(verify (* a b) (+ a b))
  '(verify (* a b) (* a b)))

(definfo 'differential
  "total differential of an object"
  '(differential (f (^ x 2) y)))

(definfo 'negate
  "Unary negation."
  '(negate a)
  '(* -1 a))

(definfo '^				;need to do expt also
  "Exponentiation."
  '(^ (+ a 1) 2)
  '(+ 1 a (^ a 2)))

(definfo '^^				;need to do ncexpt also
  "Non-commutative Exponentiation.  For vectors, this is repeated dot
product."
  '(^^ #(a b) 2)
  '(+ (^ a 2) (^ b 2))
  "For matrices, this is repeated matrix multiplication.  If n is
negative, the inverse of the matrix is raised to -n."
  '(^^ #(#(a b) #(c d)) 2)
  "For single-valued functions of one variable, This is the
composition of the function with itself n times.  If n is negative,
the inverse of the function is raised to -n."
  '(^^ (lambda #(x) (+ 1 (* 2 x))) -2))

(definfo '*
  "Multiplication, times."
  '(* a 7)
  '(* 7 a))

(definfo '+
  "Addition, plus."
  '(+ a b))

(definfo '-
  "Subtraction, minus."
  '(- a 9))

(definfo '/
  "Quotient, division, divide, over."
  '(/ a b))

(definfo 'bunch
  "bunch, vector, list."
  '(bunch a b c)
  '#(a b c))

(definfo 'rapply
  "subscripted reference"
  '(rapply #(a b) 2)
  'b)

(definfo 'or
  "union, multiple value.  Or of two equations returns an equation
with either condition true."
  '(or (= a b) (= a c))
  '(= 0 (- (^ a 2) (* b c)))
  "Or of two values yields a multiple value, such as +/-x"
  '(or x (negate x))
;  '(+/- x)
  "Or of an equation and a value will yield the value.")

(definfo '=
  "equals, equality.  This expresses a relation between variables and
numbers"
  '(= a (^ b 2))
  '(= 0 (- a (^ b 2)))
  "it does not conote value assignment")

(definfo 'qed
  "qed, bye, exit.  This leaves the math system")

(definfo 'quit
  "quit.  This leaves the math system and scheme")

(definfo 'listofvars
  "This returns a list of variables occuring in the argument"
  '(listofvars (+ a (/ b c))))

(definfo 'coeff
  "coeff, coefficient.  Returns the coefficient of (optional 1) power
of var in poly")

(definfo 'num
  "num, numerator, top.  The numerator of a rational expression.")

(definfo 'denom
  "denom, denominator, bottom.  The denominator of a rational
expression.")

(definfo 'divide
  "divide.  A bunch of the quotient and remainder.")

(definfo 'content
  "Returns a list of content and primitive part of a polynomial with
respect to the variable.  The content is the GCD of the coefficients
of the polynomial in the variable.  The primitive part is poly divided
by the content"
  '(content (+ (* 2 x y) (* 4 (^ x 2) (^ y 2))) y)
  '#((* x y) (+ y (* 2 x (^ y 2)))))

(definfo 'gcd
  "Greatest Common Divisor"
  '(gcd (* a a) (* a b)))

(definfo 'mod
  "the first argument modulo (or reduced by) the second argument")

(definfo 'resultant
  "resultant.  The result of eliminating a variable between 2
equations (or polynomials).")

(definfo 'sylvester
  "sylvester.  Matrix whose determinant is the resultant of 2
equations (or polynomials).")

(definfo 'jacobi
  "The matrix composed of the gradients of each argument function."
  '(jacobi (lambda #(x y) (* x y))
	   (lambda #(x y) (/ x y))))

(definfo 'jacobian
  "The determinant of the matrix composed of the gradients of each
argument function."
  '(jacobian (lambda #(x y) (* x y))
	     (lambda #(x y) (/ x y))))

(definfo 'wronski
  "The matrix constructed by placing the functions in the first row,
the first derivative of each function in the second row, and so on
through the n-1 derivative, thus forming a square matrix."
  '(wronski x (^ e x) (* (^ e x) x)))

(definfo 'wronskian
  "Returns the determinant of the matrix constructed by placing the
functions in the first row, the first derivative of each function in
the second row, and so on through the n-1 derivative, thus forming a
square matrix."
  '(wronskian x (^ e x) (* (^ e x) x)))

(definfo 'discriminant
  "discriminant of a polynomial.  the square of the product of the
differences of all pairs of roots."
  '(discriminant (* (- x a) (- x b) (- x c)) x))

(definfo 'eliminate
  "eliminate.  An equation or set of equations with vars eliminated")

(definfo 'polyelim
  "An polynomial or set of equations with vars eliminated")

(definfo 'factor
  "Return bunch of factors of number or polynomial")

(definfo 'prime?
  "Returns true or false depending on when number is prime")

(definfo 'matrix
  "matrix.  makes a copy of a matrix")

(definfo 'genmatrix
  "genmatrix.  A matrix whose entries are the function applied to its indices")

(definfo 'augcoefmatrix
  "Returns matrix of coefficients (including constants) of vars in eqns")

(definfo 'coefmatrix
  "Returns matrix of coefficients of vars in eqns")

(definfo 'rank
  "Rank of a matrix")

(definfo 'ident
  "ident, identity matrix.  A square matrix of 0s except the diagonal
entries are 1")

(definfo 'scalarmatrix
  "scalarmatrix, diagonal matrix.  A square matrix of 0s except the
diagonal entries = argument")

(definfo 'diagmatrix
  "diagmatrix takes as input a list of algebraic values and returns
a diagonal matrix whose diagonal entries are the elements of that
list.")

(definfo 'determinant
  "determinant.  The determinant of a square matrix")

(definfo 'charpoly
  "characteristic Polynomial of a matrix in terms of variable")

(definfo 'crossproduct
  "crossproduct.  Crossproduct of 2 vectors")

(definfo 'dotproduct
  "dotproduct.  dotproduct of 2 vectors.")

(definfo 'ncmult
  "ncmult.  non-commutative matrix multiplication of 2 vectors.")

(definfo 'row
  "Row.  row of a matrix")

(definfo 'col
  "column.  column of a matrix")

(definfo 'minor
  "minor.  minor of a matrix")

(definfo 'cofactor
  "cofactor of a matrix with column and row")

(definfo 'transpose
  "transpose of a matrix")

(definfo 'cartprod
  "Given a bunch of bunches, returns the Cartesian product of the bunches"
  '(cartprod #(#(a b c) #(d e))))

(definfo 'elementwise
  "compute procedure on corresponding elements of matrix or matrices")

(definfo 'finv
  "functional inverse")

(definfo 'load
  "load a file of scheme code")

(definfo 'require
  "load a file of scheme code from the JACAL directory")

(definfo 'batch
  "Take a file of JACAL commands as though typed by user")

(definfo 'transcript
  "Start a transcript file of all input and output."
  "With no argument, turn off transcript")

(definfo 'system
  "Run command on underlying system")

(definfo 'coeffs
  "Return coefficients in poly of var")

(definfo 'poly
  "Given var and coefficients, construct polynomial")

(definfo 'diff
  "Derivative of exp with respect to var(s)")

(definfo 'partial
  "Partial derivative of function with respect to argument(s) n or `@n'")
