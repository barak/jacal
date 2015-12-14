;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1995, 1997, 2005, 2006 Aubrey Jaffer.
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

(require 'object->string)
(require 'hash-table)
(require 'rev4-optional-procedures)
(require 'common-list-functions)

;;; Scheme doesn't allow for definition of new types which are
;;; distinct from existing types.  So we will carefully use BUNCH
;;; instead of LIST in order to distinguish the types.
;;; This requires that boolean?, pair?, symbol?, number?,
;;; string?, vector? and procedure? be disjoint as outlined in:
;;; Jonathan Rees and William Clinger, editors. The Revised^3
;;; Report on the algorithmic language Scheme, ACM SIGPLAN Notices
;;; 21(12), ACM, December 1986.
;;; If the types are not disjoint you WILL lose.

;;; The following types are mutually exclusive:
;;; SEXP, VARIABLE, EXPL, IMPL, EQLT, BUNCH
;;; INTEGERs are EXPL
;;; An EXPR is an EXPL or IMPL
;;; A LICIT is an EXPL, IMPL, or EQLT.
;;; VARIBLEs can only occur as part of EXPRS and EQLTS.
;;; SYMBOLs can only occur in SEXP.
;;; BUNCHES can contain SYMBOLs, LICITs, and BUNCHEs.
;;; An EXPL, IMPL, or EQLT, or BUNCH of these can be a
;;; lambda expression.

;;; A VAR is a vector which consists of:
;;; 0 var:sexp		- s-expression	;lambda vars have leading "@"
;;; 1 var:pri		- string	;ordering priority
					;first char is priority override
					;last char is differential order
;;; 2 var:def		- poleq		;ext defining equation
;;;		     or	- integer	;simple lambda position
;;;		     or - procedure	;
;;; 3 var:depends	- list of vars	;vars used in var:def
;;; 4 var:shadow	- var		;shadow copies of this lambda var or #f
;;;;		   THE REST ARE FOR FUNCTIONS ONLY
;;; 5 func-arglist			;list of (function and) argument values.
;;; 6 func-parity	- list of atoms	;EVEN, ODD, 0, or #F
;;; 7 func-syms		- list of lists	;of positions of arguments
;;; 8 func-anti-syms	- list of lists	;of positions of arguments
;;; 9 func-dists	- list of lists	;of functions which distribute
;;; 10 func-anti-dists	- list of lists	;of functions which anti-distribute
;;; 11 func-idems	- list		;of positions of arguments
					; perserved in idempotency

(define make-var vector)
(define poly:var? vector?)
(define (var:sexp v) (vector-ref v 0))
(define (var:pri v) (char->integer (string-ref (vector-ref v 1) 0)))
(define (var:set-pri! v i) (string-set! (vector-ref v 1) 0 (integer->char i)))
(define (var:def v) (vector-ref v 2))
(define (var:set-def! v i) (vector-set! v 2 i) v)
(define (var:depends v) (vector-ref v 3))
(define (var:set-depends! v i) (vector-set! v 3 i) v)

(define (func-arglist f) (vector-ref f 5))
(define (func-set-arglist f i) (vector-set! f 5 i))

(define func? func-arglist)

(define (func-parity f) (vector-ref f 6))
(define (func-set-parity! f v) (vector-set! f 6 v))
(define (func-syms f) (vector-ref f 9))
(define (func-set-syms! f v) (vector-set! f 9 v))
(define (func-anti-syms f) (vector-ref f 10))
(define (func-set-anti-syms! f v) (vector-set! f 10 v))
(define (func-dists f) (vector-ref f 11))
(define (func-set-dists! f v) (vector-set! f 11 v))
(define (func-anti-dists f) (vector-ref f 12))
(define (func-set-anti-dists! f v) (vector-set! f 12 v))
(define (func-idems f) (vector-ref f 13))
(define (func-set-idems! f v) (vector-set! f 13 v))

(define (copy-vector v)
  (define iv (make-vector (vector-length v)))
  (do ((i (+ -1 (vector-length iv)) (+ -1 i)))
      ((negative? i) iv)
    (vector-set! iv i (vector-ref v i))))

;;; The shadowed variables are kept in a list in the var:shadow slot
;;; in the var.  This list has the bumped and shadowed transformations
;;; of the var for each arglist-length less than or equal
;;; (var:max-lambda-position var).

;;; For simple lambda variables (@1, @2, ...) the last shadowed
;;; variable is identical to the original except that its shadow slot
;;; is #f.
(define (var:shadow v arglist-length)
  (if (not (vector-ref v 4))
      (vector-set! v 4 (make-list (var:max-lambda-position v) #f)))
  (let ((shadpair (memshad arglist-length (vector-ref v 4))))
    (if (not (car shadpair))
	(set-car! shadpair
		  (cond ((< arglist-length (var:min-lambda-position v))
			 (var:lambda-bump v (- arglist-length)))
			(else (make-shadow v arglist-length)))))
    (car shadpair)))
(define (memshad arglist-length shadlist)
  (cond ((= 1 arglist-length) shadlist)
	((null? (cdr shadlist)) shadlist)
	(else (memshad (+ -1 arglist-length) (cdr shadlist)))))
(define (make-shadow v arglist-length)
  (let ((nv (copy-vector v)))
    (vector-set! nv 4 #f)
    (vector-set!
     nv 0 (if (symbol? (var:sexp nv))	;got here because can't bump
	      (string->symbol
	       (string-append
		(symbol->string (var:sexp nv)) ":"))
	      (do-sexp-symbols
	       (lambda (s)
		 (define st (symbol->string s))
		 (if (and (char=? #\@ (string-ref st 0))
			  (> (string-length st) 1))
		     (let ((i (string->number
			       (substring st 1 (string-length st)))))
		       (if (> i arglist-length)
			   (var:sexp (lambda-var (- i arglist-length) 0))
			   (string->symbol (string-append st ":"))))
		     s))
	       (var:sexp v))))
    ;; Now avoid priority conflicts with minimum increment.
    (let* ((s (string-copy (vector-ref v 1)))
	   (si (+ -1 (string-length s))))
      (string-set! s si (integer->char
			 (+ 1 (char->integer (string-ref s si)))))
      (vector-set! nv 1 s))
    (var:set-depends!
     nv (map (lambda (var)
	       (if (simple-lambdavar? var) (var:shadow var arglist-length)
		   var))
	     (var:depends v)))
    (if (var:def v)
	(var:set-def! nv (licits:do-vars
			  (lambda (var)
			    (if (lambdavardep? var)
				(if (eq? var v) nv
				    (var:shadow var arglist-length))
				var))
			  (var:def v))))
    (if (>= (vector-length v) 5)
	(func-set-arglist nv (licits:do-vars
			      (lambda (var)
				(if (lambdavardep? var)
				    (var:shadow var arglist-length) var))
			      (func-arglist v))))
    nv))

(define (var:> v2 v1)
  (string>? (vector-ref v2 1) (vector-ref v1 1)))

;(define var-tab '())
;(define var-tab-lookup (predicate->asso equal?))
;(define var-tab-define (alist-associator equal?))
;(define var-tab-for-each alist-for-each)

(define var-tab (make-hash-table 43))
(define var-tab-lookup (predicate->hash-asso equal?))
(define var-tab-define (hash-associator equal?))
(define var-tab-for-each hash-for-each)

(define (list-of-vars)
  (define vars '())
  (var-tab-for-each (lambda (k v) (set! vars (cons v vars))) var-tab)
  vars)

(define (sexp->new-var v)
  (let ((base v)
	(diffs 0)
	(lambda? (and (pair? v) (eq? 'lambda (car v)))))
    (do () ((not (and (pair? base) (eq? 'differential (car base)))))
      (set! base (cadr base))
      (set! diffs (+ 2 diffs)))		;leave space for shadow priority.
    (let* ((s (object->string base))
	   (sl (string-length s))
	   (arglist (if (pair? v) (map sexp->math (if lambda? (caddr v) v))
			'())))
      (if lambda?
	  (make-var
	   (caddr v)
	   (string-append (case (string-ref s 0)
			    ((#\@) lambda-var-pri-str)
			    (else lambda-var-pri-str))
			  s
			  (string (integer->char diffs)))
	   (and (char=? #\@ (string-ref s 0))
		(not (= sl 1))
		(not (char=? #\^ (string-ref s 1)))
		(string->number (substring s 1 sl)))
	   (var:build-depends arglist)
	   #f
	   arglist
	   #f #f #f #f #f #f		; function properties
	   )
	  (make-var
	   v
	   (string-append (case (string-ref s 0)
			    ((#\@) lambda-var-pri-str)
			    (else median-pri-str))
			  s
			  (string (integer->char diffs)))
	   (and (char=? #\@ (string-ref s 0))
		(not (= sl 1))
		(not (char=? #\^ (string-ref s 1)))
		(string->number (substring s 1 sl)))
	   (var:build-depends arglist)
	   #f
	   arglist
	   )))))

(define (sexp->var sexp)
  (let ((vcell (var-tab-lookup sexp var-tab)))
    (if vcell (cdr vcell)
	(let ((val (sexp->new-var sexp)))
	  (set! var-tab (var-tab-define var-tab sexp val))
	  val))))
(define (string->var s) (sexp->var (string->symbol s)))

(define (var:build-depends args)
  (let ((deps '()))
    (for-each (lambda (e)
		(poly:for-each-var
		 (lambda (v)
		   (cond (($var? v))
			 ((symbol? (var:sexp v)) (set! deps (adjoin v deps)))
			 (else
			  (set! deps
				(adjoin v (union
					   (var:depends v) deps))))))
		 e))
	      args)
    deps))

(define (deferop . args)
  (var->expl (sexp->var (deferedmath->sexp args))))

(define lambda-var-pri (+ -5 char-code-limit))
(define lambda-var-pri-str (string (integer->char lambda-var-pri)))
(define median-pri-str (string (integer->char (quotient char-code-limit 2))))

(define (var->string v)
  (let ((sexp (var:sexp v)))
    (if (symbol? sexp) (symbol->string sexp)
	(math:error 'expected-simple-symbol sexp))))

(define (make-rad-var radicand exponent-reciprocal)
  (let ((e (univ:monomial -1 exponent-reciprocal $)))
    (set-car! (cdr e) radicand)
    (let ((v (defext (sexp->var (list '^ (bunch->sexp radicand #t)
				      (list '/ 1 exponent-reciprocal)))
	       e)))
      (func-set-arglist v (list _^ radicand (make-rat 1 exponent-reciprocal)))
      (set! radical-defs (cons (extrule v) radical-defs))
      v)))

(define (var:nodiffs v)
  (do ((base (vector-ref v 0) (cadr base)))
      ((not (and (pair? base) (eq? 'differential (car base))))
       (if (eq? base (vector-ref v 0)) v (sexp->var base)))))
(define (var:differential? v)
  (not (zero? (var:diff-depth v))))
(define (var:diff-depth v)
  (let ((s (vector-ref v 1)))
    (quotient (char->integer (string-ref s (+ -1 (string-length s)))) 2)))
(define (var:differential v)
  (sexp->var (list 'differential (var:sexp v))))

(define (var:undiff v)
  (sexp->var (cadr (var:sexp v))))

(define (radicalvar? v)
  (let ((ve (var:sexp v)))
    (and (pair? ve) (eq? '^ (car ve)))))
(define (lambdavar? v)
  (= lambda-var-pri (var:pri v)))
(define (simple-lambdavar? v)
  (number? (lambda-position v)))
(define (simple-shadowed-lambdavar? v)
  (and (number? (lambda-position v)) (not (vector-ref v 4))))
(define (lambdavarext? v)
  (< lambda-var-pri (var:pri v) $-pri))
(define (lambdavardep? v)
  (< (+ -1 lambda-var-pri) (var:pri v) $-pri))
(define ($var? v)
  (char=? #\:  (string-ref (vector-ref v 1) 1)))
(define (lambda-var i diff-depth)
  (if (zero? diff-depth)
      (let ((v (sexp->var
		(string->symbol
		 (string-append "@" (number->string i))))))
	(var:set-def! v i)
	(or (vector-ref v 4)		;so simple-shadowed-lambdavar? works
	    (vector-set! v 4 (make-list i #f)))
	v)
      (var:differential (lambda-var i (+ -1 diff-depth)))))
;;; This sometimes is called with shadowed variables (:@4)
(define lambda-position var:def)

;;; currently unused
;; (define (var:sexp-string v)
;;   (var->string (var:nodiffs v)))
;; (define (var:sexp-apply proc var)
;;   (if (var:differential? var)
;;       (var:differential (var:sexp-apply proc (var:undiff var)))
;;       (apply proc var '())))

(define (extrule e)
  (let ((vd (var:def e)))
    (and (pair? vd) vd)))
(define (potent-extrule e)
  (let ((vd (var:def e)))
    (and (pair? vd) (not (eqv? 0 (cadr vd))) vd)))
(define (defext var impl)
  (let ((deps (var:depends var)))
    (set! deps
	  (if (null? deps) (remove var (var:build-depends (list impl)))
	      (remove (car _^) deps)))
    (var:set-depends! var deps)
    (var:set-pri! var (if (null? deps) 10 ;must be a constant.
			  (+ 1 (apply max (map var:pri deps)))))
    (var:set-def! var (vsubst var $ impl))
    var))

;;; IMPL is a data type consisting of a poly with major variable
;;; $.  The value of the IMPL is negative of the poly solved for $.
;;; Using this representation, if poly is square-free and has no
;;; content (gcd (coefficients) = 1), we can express any
;;; algebraic function or number uniquely, even those with no standard
;;; representation (order > 4 roots).

(define (expr? p)
  (or (number? p)
      (and (pair? p)
	   (poly:var? (car p)))))
(define (impl? p) (and (pair? p) (poly:var? (car p)) ($? (car p))))
(define (rat:number? p)
  (or (number? p)
      (and (impl? p)
	   (= 3 (length p))
	   (number? (cadr p))
	   (number? (caddr p)))))
(define (expr:0? p) (or (eqv? 0 p) (and (impl? p) (eqv? 0 (rat:num p)))))
(define (expl? p)
  (or (number? p)
      (and (pair? p)
	   (poly:var? (car p))
	   (not ($? (car p))))))
;;; Rational impl?
(define (rat? p) (and (impl? p) (= 3 (length p))))
(define (make-rat num denom) (list $ num (poly:negate denom)))
(define (rat:num p) (poly:negate (cadr p)))
(define (rat:denom p) (caddr p))
(define (rat:unit-denom? p) (unit? (caddr p)))

(define (bunch? p)
  (or (null? p)
      (and (pair? p)
	   (not (poly:var? (car p)))
	   (not (eqv? $= (car p))))))

(define $= "=")
(define (eqn? p) (and (pair? p) (eqv? $= (car p))))
(define (eqns? p) (if (bunch? p) (some eqns? p) (eqn? p)))
(define (licit? p)
  (or (number? p)
      (and (pair? p)
	   (or (poly:var? (car p))
	       (eqv? $= (car p))))))

(define eqn->poly cdr)
(define (poly->eqn p) (cons $= p))
(define (polys->eqns p) (if (bunch? p) (map polys->eqns p) (poly->eqn p)))
(define (var->expl v) (list v 0 1))
(define (expl->impl p) (make-rat p 1))
(define (var->impl v) (make-rat (var->expl v) 1))

;;; Two paradigms for doing algebra on equations and expressions:
;;; Polynomials as expressions and Polynomials as equations.
;;; Polynomials are used as expressions in GCD.
;;; Polynomials are used as equations in ELIMINATE.
;;;	licit->	polxpr	poleqn
;;;	eqn	expl	expl
;;;	expl	expl	impl
;;;	impl	expl(?)	impl
;;; After the operation is done, we need to convert back.  For
;;; Polynomials as expressions, the result is already expl.  For
;;; polynomials as equations:
;;;	poleqn->licit
;;;	expl	eqn
;;;	impl	expr
(define (licit->poleqn p)
  (cond ((symbol? p) (var->impl (sexp->var p)))
	((eqn? p) (eqn->poly p))
	((impl? p) p)
	((expl? p) (expl->impl p))
	(else (math:error 'cannot-be-coerced-to-poly-eqn:- p))))
(define (licits->poleqns p)
  (if (bunch? p) (map licits->poleqns p) (licit->poleqn p)))
(define (poleqn->licit p)
  (cond ((impl? p) (expr:norm p))
	((expl? p) (poly->eqn p))
	(else (math:error 'not-a-polynomial-equation p))))
(define (poleqns->licits p)
  (if (bunch? p) (map poleqns->licits p) (poleqn->licit p)))
(define (licit->polxpr p)
  (cond ((symbol? p) (var->expl (sexp->var p)))
	((eqn? p) (eqn->poly p))
	((expl? p) p)
	((and (impl? p) (poly:/? (rat:num p) (rat:denom p))))
	(else (math:error 'cannot-be-coerced-to-expl:- p))))
(define (licit->impl p)
  (cond ((symbol? p) (var->impl (sexp->var p)))
	((eqn? p) (math:error 'value-expected-equation-found:- p))
	((impl? p) p)
	((expl? p) (expl->impl p))
	(else (math:error 'cannot-be-coerced-to-implicit:- p))))
(define (licits->impls p)
  (if (bunch? p) (map licits->impls p) (licit->impl p)))
(define (expr p)
  (cond ((symbol? p) (var->expl (sexp->var p)))
	((expr? p) p)
	(else (math:error 'cannot-be-coerced-to-expr:- p))))
(define (exprs p)
  (if (bunch? p) (map exprs p) (expr p)))
(define (expl:var? p)
  (and (pair? p)
       (expl? p)
       (equal? (cdr p) '(0 1))))
(define (expl->var p)
  (cond ((symbol? p) (sexp->var p))
;	((poly:var? p) p)
	((expl:var? p)
	 (car p))
	(else (math:error 'not-a-simple-variable:- p))))
(define (variables p)
  (cond ((symbol? p) (list (sexp->var p)))
;	((poly:var? p) (list p))
	((expl:var? p)
	 (list (car p)))
	((list? p) (map expl->var p))
	(else (math:error 'not-a-simple-variable:- p))))
(define (plicit->integer p)
  (cond ((integer? p) p)
	((not (rat:number? p)) (math:error 'not-an-integer- p))
	((rat:unit-denom? p) (* (rat:denom p) (rat:num p) -1))
	(else (math:error 'not-an-integer- p))))
(define (unit? x) (member x '(1 -1)))
(define (expr:norm p)
  (if (and (rat? p) (rat:unit-denom? p))
      (poly:* (rat:num p) (rat:denom p))
    p))
(define (expr:norm-or-unitcan p)
  (if (and (rat? p) (rat:unit-denom? p))
      (poly:* (rat:num p) (rat:denom p))
      (unitcan p)))

;;; These two functions return type expl
(define (num p)
  (cond ((impl? p) (rat:num p))
	((expl? p) p)
	(else (math:error 'cannot-extract-numerator- p))))
(define (denom p)
  (cond ((rat? p) (rat:denom p))
	((expl? p) 1)
	(else (math:error 'cannot-extract-denominator- p))))
(define (sexp? e)
  (cond ((number? e) #t)
	((symbol? e) #t)
	((pair? e) (symbol? (car e)))
	((vector? e) #t)
	(else #f)))

;;; A useful companion for ZERO?
(define (one? n) (eqv? 1 n))
