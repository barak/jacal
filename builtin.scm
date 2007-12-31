;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1997, 2005 Aubrey Jaffer.
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

(require 'sort)
(require 'hash-table)
(require 'transcript)
(require 'object->string)
(require 'rev4-optional-procedures)
(require 'common-list-functions)
(require-if 'compiling 'factor)
(require-if 'compiling 'hensel)
(require-if 'compiling 'combinatorics)

;;;; First, what case are symbols in?  Determine the standard case:
(define char-standard-case
  (cond ((not (string=? (symbol->string 'a) (symbol->string 'A)))
	 char-downcase)
	((string=? (symbol->string 'a) "A")
	 char-upcase)
	((string=? (symbol->string 'A) "a")
	 char-downcase)
	(else
	 char-downcase)))
(define (string-standard-case s)
  (set! s (string-copy s))
  (do ((i 0 (+ 1 i))
       (sl (string-length s)))
      ((>= i sl) s)
      (string-set! s i (char-standard-case (string-ref s i)))))
(define (bltn:error . args)
  (apply math:warn args)
  novalue)

;;; Predefined Constants
(define expl:t (var->expl (sexp->var 't)))
(define $ (string->var ":@"))
(define $-pri (+ -1 char-code-limit))
(var:set-pri! $ $-pri)
(define ($? v) (or (eq? v $) (= (var:pri v) $-pri)))
(define d$ (var:differential $)) ;used only in total-differential in norm.scm
(var:set-pri! d$ (+ -2 char-code-limit))
(define $1 (string->var "@1"))
(define $2 (string->var "@2"))
(define $3 (string->var "@3"))
(define _$ (string->var "::@"))

(define _1/2 (list $ 1 -2))

;;; Canonical functions
(define _-$1 (list $1 0 -1))
(define $1+$2 (list $2 (list $1 0 1) 1))
(define $1-$2 (list $2 (list $1 0 1) -1))
(define $1*$2 (list $2 0 (list $1 0 1)))
(define $1/$2 (list $ (list $1 0 1) (list $2 0 -1)))
(define $1=$2 (list $= $2 (list $1 0 -1) 1))
(define cidentity (list $1 0 1))
(define $1!!$2 (list $ (list $2 0 (list $1 0 1)) (list $2 (list $1 0 -1) -1)))

;;; Canoncial functions for vect.scm
(define $1-$2*$3 (list $3 (list $1 0 1) (list $2 0 -1)))
(define $1*$2+$3 (list $3 (list $2 0 (list $1 0 1)) 1))
(define _-$1/$2 (make-rat (list $1 0 -1) (list $2 0 1)))

;;; set up initial radical and extension
(define %sqrt1 (defext (sexp->var '%sqrt1) (list $ 1 0 -1)))
(var:set-pri! %sqrt1 5)
(define %i (defext (sexp->var '%i) (list $ -1 0 -1)))
(var:set-pri! %i 5)
(define radical-defs (list (extrule %i) (extrule %sqrt1)))
(define _+/-$1 (list $1 0 (list %sqrt1 0 1)))
(define _-/+$1 (list $1 0 (list %sqrt1 0 -1)))
(define $1+/-$2 (list $2 (list $1 0 1) (list %sqrt1 0 1)))
(define $1-/+$2 (list $2 (list $1 0 1) (list %sqrt1 0 -1)))

;;; non-canonical functions for use with DEFEROP
(define _^ (list (string->var "^") 0 1))
(define _^^ (list (string->var "^^") 0 1))
(define _partial (list (string->var "partial") 0 1))
(define _ncmult (list (string->var "ncmult") 0 1))
(define _abs (list (string->var "abs") 0 1))
(define _rapply (list (string->var "rapply") 0 1))

(define novalue (var->expl (sexp->var '?)))
(define (novalue? x) (equal? novalue x))

(define *modestack* '())
(define (push-modes)
  (set! *modestack*
	(cons
	 `((*input-grammar* ,*input-grammar*)
	   (*output-grammar* ,*output-grammar*)
	   (*echo-grammar* ,*echo-grammar*)
	   (horner ,horner)
	   (math:trace ,math:trace)
	   (math:debug ,math:debug)
	   (math:phases ,math:phases)
	   (linkradicals ,linkradicals)
	   (newlabelstr ,newlabelstr)
	   (newlabelsym ,newlabelsym)
	   (page-height ,page-height)
	   (page-width ,page-width))
	 *modestack*))
;;;  (set! *input-grammar* (get-grammar 'standard))
;;;  (set! *output-grammar* (get-grammar 'disp2d))
  (set! *echo-grammar* (get-grammar 'null))
;;;  (set! horner #f)
;;;  (set! math:trace #f)
;;;  (set! math:debug #f)
;;;  (set! math:phases #f)
;;;  (set! linkradicals #t)
  (set! newlabelstr (string-standard-case "EX0")) ;prompt template
  (set! newlabelsym (string->symbol newlabelstr))
  (set! % novalue))

(define (pop-modes)
  (define modes (car *modestack*))
  (set! *input-grammar* (cadr (assq '*input-grammar* modes)))
  (set! *output-grammar* (cadr (assq '*output-grammar* modes)))
  (set! *echo-grammar* (cadr (assq '*echo-grammar* modes)))
  (set! horner (cadr (assq 'horner modes)))
  (set! math:trace (cadr (assq 'math:trace modes)))
  (set! math:debug (cadr (assq 'math:debug modes)))
  (set! math:phases (cadr (assq 'math:phases modes)))
  (set! linkradicals (cadr (assq 'linkradicals modes)))
  (set! newlabelstr (cadr (assq 'newlabelstr modes)))
  (set! newlabelsym (cadr (assq 'newlabelsym modes)))
  (set! page-height (cadr (assq 'page-height modes)))
  (set! page-width (cadr (assq 'page-width modes)))
  (set! *modestack* (cdr *modestack*)))

;(define *flags* '())
;(define flag-associator (alist-associator eq?))
;(define flag-inquirer (alist-inquirer eq?))
;(define (list-of-flags)
;  (define flags '())
;  (alist-for-each (lambda (k v) (set! flags (cons k flags))) *flags*)
;  flags)

(define *flags* (make-hash-table 5))
(define flag-associator (hash-associator eq?))
(define flag-inquirer (hash-inquirer eq?))
(define (list-of-flags)
  (define flags '())
  (hash-for-each (lambda (k v) (set! flags (cons k flags))) *flags*)
  flags)

(define (defflag name setter getter)
  (set! *flags* (flag-associator *flags* name (cons setter getter)))
  name)

(define flag:setter car)
(define flag:getter cdr)

(define (flag-set name . values)
  (let ((flag (flag-inquirer *flags* name)))
    (cond ((not flag) (bltn:error 'flag name 'is-not-defined))
	  ((flag:setter flag) (apply (flag:setter flag) flag values) novalue)
	  (else (bltn:error 'flag name 'can-not-be-set)))))

(define (flag-get name . rest)
  (let ((flag (flag-inquirer *flags* name)))
    (cond ((not flag) (bltn:error 'flag name 'is-not-defined))
	  ((flag:getter flag) (apply (flag:getter flag) flag rest))
	  (else (bltn:error 'flag name 'can-not-be-read)))))

(defflag 'ingrammar
  (lambda (f v)
    (define name (var:sexp (expl->var v)))
    (cond ((get-grammar name)
	   (set! *input-grammar* (get-grammar name)))
	  (else
	   (bltn:error 'grammar name 'not-known))))
  (lambda (f) (var->expl (sexp->var (grammar-name *input-grammar*)))))

(defflag 'outgrammar
  (lambda (f v)
    (define name (var:sexp (expl->var v)))
    (cond ((get-grammar name)
	   (set! *output-grammar* (get-grammar name)))
	  (else
	   (bltn:error 'grammar name 'not-known))))
  (lambda (f) (var->expl (sexp->var (grammar-name *output-grammar*)))))

(defflag 'echogrammar
  (lambda (f v)
    (define name (var:sexp (expl->var v)))
    (cond ((get-grammar name)
	   (set! *echo-grammar* (get-grammar name)))
	  (else
	   (bltn:error 'grammar name 'not-known))))
  (lambda (f) (var->expl (sexp->var (grammar-name *echo-grammar*)))))

(defflag 'grammars
  #f
  (lambda (f)
    (map (lambda (g) (var->expl (sexp->var g))) (list-of-grammars))))

(define (set-boolean v)
  (define val (var:sexp (expl->var v)))
  (case val
    ((off 0 false) #f)
    ((on 1 true) #t)
    (else (bltn:error 'expected-boolean v))))

(define (show-boolean v)
  (var->expl (sexp->var (if v 'on 'off))))

(defflag 'horner
  (lambda (f v) (set! horner (set-boolean v)))
  (lambda (f) (show-boolean horner)))

(defflag 'trace
  (lambda (f v) (set! math:trace (set-boolean v)))
  (lambda (f) (show-boolean math:trace)))

(defflag 'debug
  (lambda (f v) (set! math:debug (set-boolean v)))
  (lambda (f) (show-boolean math:debug)))

(defflag 'phases
  (lambda (f v) (set! math:phases (set-boolean v)))
  (lambda (f) (show-boolean math:phases)))

(defflag 'linkradicals
  (lambda (f v) (set! linkradicals (set-boolean v)))
  (lambda (f) (show-boolean linkradicals)))

(defflag 'version
  #f
  (lambda (f)
    (var->expl (string->var *jacal-version*))))

(defflag 'all
  #f
  (lambda (f)
    (block-write-strings
     (sort! (map symbol->string (list-of-flags))
	    string<?))
    novalue))

(defflag 'prompt
  (lambda (f v)
    (set! newlabelstr (var->string (expl->var v)))
    (set! newlabelsym (string->symbol newlabelstr))
    novalue)
  (lambda (f) (var->expl (string->var newlabelstr))))

(defflag 'page
  (lambda (f v)
    (define val (if (number? v) v (var:sexp (expl->var v))))
    (set! page-height
	  (case val ((off 0 false) #f)
		((on 1 true) #t)
		(else (if (number? val) val
			  (bltn:error 'expected-boolean-or-number v))))))
  (lambda (f) (if (boolean? page-height)
		  (show-boolean page-height)
		  page-height)))

(defflag 'width
  (lambda (f v)
    (define val (if (number? v) v (var:sexp (expl->var v))))
    (set! page-width
	  (case val ((off 0 false) #f)
		((on 1 true) #t)
		(else (if (number? val) val
			  (bltn:error 'expected-boolean-or-number v))))))
  (lambda (f) (if (boolean? page-width)
		  (show-boolean page-width)
		  page-width)))

(defflag 'priority
  (lambda (f v p)
    (if (not (and (number? p) (< 0 p lambda-var-pri))) (math:error))
    (var:set-pri! (expl->var v) p))
  (lambda args
    (if (null? (cdr args))
	(let ((l (list-of-vars)))
	  (block-write-strings (map object->string
				    (map var:sexp (sort! l var:>))))
	  novalue)
	(var:pri (expl->var (cadr args))))))

;(define transcript-name #f)
;(defflag 'transcript
;  (lambda (f v)
;    (define file (and v (not (null? v)) (var->string (expl->var v))))
;    (if v (transcript-on file) (transcript-off))
;    (set! transcript-name file))
;  (lambda (f) (if transcript-name
;		  (var->expl (string->var transcript-name))
;		  '#())))

;;;; Built in functions
(defbltn 'set
  (lambda (name . values)
    (apply flag-set (var:sexp (expl->var name)) values)))

(defbltn 'show
  (lambda (name . rest) (apply flag-get
			       (var:sexp (expl->var name))
			       rest)))

(defbltn 'commands
  (lambda ()
    (block-write-strings
     (sort! (map object->string (list-of-procedure-defsyms))
	    string<?))
    novalue))

(defbltn '%
  (lambda () %))

(defbltn 'depends
  (lambda (x) (map var->expl (var:depends (expl->var x)))))

(defbltn 'args
  (lambda (x) (cdr (func-arglist (expl->var x)))))

(defbltn 'func
  (lambda (x) (car (func-arglist (expl->var x)))))

(defbltn 'describe
  (lambda (x)
    (cond
     ((and (expl:var? x)
	   (info:describe (var:sexp (expl->var x)))))
     ((bunch? x) (display (bunch-type x)) (newline))
     ((not (expl:var? x)) (display (scalar-type x)))
     (else (describe-var (expl->var x))))
    (if (clambda? x)
	(let ((hlv (licits:max-lambda-position (if (eqn? x) (eqn->poly x) x))))
	  (tran:display 'function-of-)
	  (display hlv)
	  (if (= 1 hlv) (tran:display 'argument) (tran:display 'arguments))))
    novalue))

(define (describe-var v)
  (cond ((var:differential? v)
	 (tran:display 'differential-)
	 (set! v (var:nodiffs v))))
  (display
   (cond ((radicalvar? v) 'radical)
	 ((not (symbol? (var:sexp v))) 'application)
	 (else 'variable))))

(define (scalar-type x)
  (cond ((number? x) 'number)
	((eqn? x) 'equation)
	((expl? x) 'polynomial)
	((rat? x) 'rational-expression)
	((impl? x) 'implicit-expression)
	(else 'unknown)))

(define (bunch-type x)
  (cond ((matrix? x) 'matrix)
	((row? x) 'row-vector)
	((column? x) 'column-vector)
	(else 'bunch)))

(defbltn 'example
  (lambda (x) (info:example x)))

(define (terms) (paginate-file (in-vicinity jacal-vicinity "COPYING")))
(defbltn 'terms (lambda () (terms) novalue))

(define (help) (paginate-file (in-vicinity jacal-vicinity "HELP")))
(defbltn 'help (lambda () (help) novalue))

(define (boolify x)
  (var->expl (sexp->var (if x 'true 'false))))

(defbltn 'verify
  (lambda (try expect)
    (let ((tv (normalize try)) (ev (normalize expect)))
      (cond ((equal? tv ev) (boolify #t))
	    (else
	     (newline-diag)
	     (display-diag (tran:translate 'did-not-verify:)) (newline-diag)
	     (write-sexp (cano->sexp tv horner) *input-grammar*) (newline-diag)
	     (display-diag (tran:translate 'but-expected:)) (newline-diag)
	     (write-sexp (cano->sexp ev horner) *input-grammar*) (newline-diag)
	     ;;(if math:debug (do-more))
	     (boolify #f))))))

(defbltn 'differential
  (lambda (obj) (total-differential obj)))

(defbltn 'negate
  (lambda (obj) (app* _-$1 obj)))

(defbltn 'u+/-
  (lambda (obj) (app* _+/-$1 obj)))

(defbltn 'u-/+
  (lambda (obj) (app* _-/+$1 obj)))

(defbltn '^				;need to do expt also
  (lambda (x exp)
    (if (and (expl? x) (number? exp) (positive? exp))
	(poly:^ x (normalize exp))
	(^ (expr x) exp))))

(defbltn '^^				;need to do ncexpt also
  (lambda (a pow) (ncexpt (exprs a) (normalize pow))))

(defbltn '*
  (lambda args (reduce (lambda (x y)
			 (if (and (expl? x) (expl? y))
			     (poly:* x y)
			     (app* $1*$2 x y)))
		       args)))

(defbltn '+
  (lambda args (reduce (lambda (x y)
			 (if (and (expl? x) (expl? y))
			     (poly:+ x y)
			     (app* $1+$2 x y)))
		       args)))

(defbltn '-
  (lambda args (reduce (lambda (x y)
			 (if (and (expl? x) (expl? y))
			     (poly:- x y)
			     (app* $1-$2 x y)))
		       args)))

(defbltn 'b+/-
  (lambda args (reduce (lambda (x y) (app* $1+/-$2 x y)) args)))

(defbltn 'b-/+
  (lambda args (reduce (lambda (x y) (app* $1-/+$2 x y)) args)))

(defbltn '/
  (lambda args (reduce (lambda (x y) (app* $1/$2 x y)) args)))

(defbltn 'over
  (lambda args (reduce (lambda (x y) (app* $1/$2 x y)) args)))

(defbltn 'parallel
  (lambda args (reduce (lambda (x y) (app* $1!!$2 x y)) args)))

(defbltn 'bunch
  (lambda args args))

(defbltn 'flatten
  (letrec ((flatten-bunch
	    (lambda (b)
	      (if (bunch? b)
		  (apply append (map flatten-bunch b))
		  (list b)))))
    flatten-bunch))

(defbltn 'rapply
  (lambda args (apply rapply args)))

(defbltn 'or
  (lambda args
    (poleqn->licit (reduce poly:* (map licit->poleqn args)))))

(defbltn '=
  (lambda (x y) (app* $1=$2 x y)))

(defbltn 'qed
  (lambda ()
    (cleanup-handlers!)
    (math:exit #t)))

(defbltn 'quit
  (lambda ()
    (cleanup-handlers!)
    (slib:exit)
    (set-handlers!)
    (math:greet)
    novalue))

(defbltn 'continue
  (lambda (value)
    (cond ((null? math:break-continuation-stack) #f)
	  (else
	   (let ((cont (car math:break-continuation-stack)))
	     (set! math:break-continuation-stack
		   (cdr math:break-continuation-stack))
	     (cont value))))))

(defbltn 'restart
  (lambda ()
    (restart)))

;;;; User callable functions

(defbltn 'listofvars
  (lambda (exp)
    (let ((deps '()))
      (licits:for-each (lambda (poly) (set! deps (union (alg:vars poly) deps)))
		       exp)
      (map var->expl (remove $ deps)))))

(defbltn 'degree
  (lambda (ply . args)
    (define xp (if (eqn? ply) (eqn->poly ply) ply))
    (cond ((null? args)
	   (poly:total-degree xp))
	  ((null? (cdr args))
	   (poly:degree xp (expl->var (car args))))
	  (else
	   (bltn:error 'degree (car ply args))))))

(define (coeff p var ord)
  (cond ((rat? p)
	 (if (zero? (poly:degree (rat:denom p) var))
	     (app* $1/$2 (poly:coeff (rat:num p) var ord) (rat:denom p))
	     (bltn:error 'not-a-polynomial? p var)))
	((expl? p) (poly:coeff p var ord))
	(else (bltn:error 'not-a-polynomial? p var))))

(define (coeffs poly var)
  (cond ((rat? poly)
	 (if (zero? (poly:degree (rat:denom poly) var))
	     (app* $1/$2
		   (cdr (poly:promote var (rat:num poly)))
		   (rat:denom poly))
	     (bltn:error 'not-a-polynomial? poly var)))
	((and (expl? poly) (not (number? poly)))
	 (cdr (poly:promote var poly)))
	(else (bltn:error 'not-a-polynomial? poly var))))

(defbltn 'coeff
  (lambda (p var . optional)
    (let ((ord (if (null? optional) 1 (car optional))))
      (coeff p (expl->var var) (plicit->integer ord)))))

(defbltn 'coeffs
  (lambda (p . optional)
    (cond ((not (null? optional))
	   (coeffs p (expl->var (car optional))))
	  ((expl? p)
	   (coeffs p (car p)))
	  ((rat? p)
	   (coeffs p (car (rat:num p))))
	  (else (bltn:error not-a-polynomial? p)))))

(define (remove-tautologies l)
  (cond ((null? l) l)
	((equal? '("=" . 0) (car l)) (remove-tautologies (cdr l)))
	(else (cons (car l) (remove-tautologies (cdr l))))))

(define (promote var p)
  (if (poly:poly? p)
      (poly:promote var p)
      (const:promote var p)))

(define (poly:equate-coeffs poly1 poly2 var)
  (set! poly1 (cdr (promote var poly1)))
  (set! poly2 (cdr (promote var poly2)))
  (cond ((< (length poly1) (length poly2))
	 (set! poly1 (append poly1 (make-list
				    (- (length poly1) (length poly2))
				    0))))
	((> (length poly1) (length poly2))
	 (set! poly2 (append poly1 (make-list
				    (- (length poly2) (length poly1))
				    0)))))
  (remove-tautologies (map (lambda (p1 p2) (app* $1=$2 p1 p2))
			   poly1 poly2)))

(define (equate-coeffs poly1 poly2 var)
  (cond ((or (rat? poly1) (rat? poly2))
	 (remove-duplicates
	  (append (poly:equate-coeffs (num poly1) (num poly2) var)
		  (poly:equate-coeffs (denom poly1) (denom poly2) var))))
	(else (remove-duplicates (poly:equate-coeffs poly1 poly2 var)))))

(defbltn 'equatecoeffs (lambda (p1 p2 var)
			 (equate-coeffs p1 p2 (expl->var var))))

(defbltn 'poly
  (lambda (var . args)
    (cond ((expl:var? var)
	   (reduce (lambda (p c) (poly:+ (poly:* p var) c))
		   (reverse
		    (cond ((> (length args) 1) args)
			  ((and (= (length args) 1) (bunch? (car args)))
			   (car args))
			  (else
			   (bltn:error 'not-a-bunch? (car args)))))))
	  ((and (null? args) (eqn? var))
	   (eqn->poly var))
	  (else
	   (bltn:error 'poly? (cons var args))))))

(defbltn 'num
  (lambda (exp) (num (expr:normalize exp))))

(defbltn 'denom
  (lambda (exp) (denom (expr:normalize exp))))

(defbltn 'divide
  (lambda (dividend divisor . vars)
    (set! dividend (licit->polxpr dividend))
    (set! divisor (licit->polxpr divisor))
    (poly:pdiv dividend divisor (if (null? vars)
				    (if (number? divisor)
					(if (number? dividend) 0
					    (car dividend))
					(car divisor))
				    (expl->var (car vars))))))

(defbltn 'content
  (lambda (poly var)
    (let* ((var (expl->var var))
	   (poly (poly:promote var (licit->polxpr poly)))
	   (cont (apply poly:gcd* (cdr poly))))
      (list cont (poly:/ poly cont)))))

;;; This is user callable GCD.
(defbltn 'gcd
  (lambda args
    (if (null? args) 0
	(reduce poly:gcd (map licit->polxpr args)))))

(define (jacal:modulus m proc-name)
  (cond ((zero? m) (bltn:error 'mod "zero modulus?"))
	((negative? m) (symmetric:modulus (- m)))
	(else m)))

(defbltn 'mod
  (lambda (licit . args)
    (let* ((modulus (if (null? args) *modulus* (licit->polxpr (car args))))
	   (var (if (or (null? args) (null? (cdr args)))
		    (if (poly:univariate? modulus) (car modulus) #f)
		    (expl->var (cadr args)))))
      (cond
       ((number? modulus)
	(poly:modularize (jacal:modulus modulus 'mod) (licit->polxpr licit)))
       ((not var) (bltn:error 'mod "no variable given?"))
       (else
	(poleqn->licit
	 (let ((p (poly:prem (licit->poleqn licit) modulus var)))
	   (if (and (rat? p) (pair? modulus)
		    (pair? (rat:denom p))
		    (eq? (car modulus) (car (rat:denom p))))
	       (poly:prem (poly:* p (alg:conjugate (rat:denom p) modulus))
			      modulus
			      var)
	       p))))))))

;;; This is user callable RESULTANT.  It always operates on
;;; polynomials and does not know about extensions etc.
(defbltn 'resultant
  (lambda (a b v)
    (let ((res (poly:resultant
		(licit->polxpr a)
		(licit->polxpr b)
		(expl->var v))))
      res)))

(defbltn 'sylvester
  (lambda (p1 p2 var)
    (sylvester (licit->polxpr p1)
	       (licit->polxpr p2)
	       (expl->var var))))

(defbltn 'discriminant
  (lambda (poly var)
    (set! poly (licit->polxpr poly))
    (set! poly (poly:/ poly (if (> (leading-number poly) 0)
				(poly:num-cont poly)
				(- (poly:num-cont poly)))))
    (let* ((v (expl->var var))
	   (deg (poly:degree poly v)))
      (if (> deg 1)
	  (poly:* (quotient (* deg (+ -1 deg)) 2)
		  (poly:resultant (poly:diff poly v) poly v))
	  0))))

(define (jacobi-matrix . funcs)
  (normalize
   (map (lambda (func)
	  (do ((idx (length funcs) (+ -1 idx))
	       (lst '() (cons (diff func (lambda-var idx 0))
			      lst)))
	      ((< idx 1) lst)))
	funcs)))

(defbltn 'jacobi jacobi-matrix)

(defbltn 'jacobian
  (lambda args
    (determinant (apply jacobi-matrix args))))

(define (wronskian-matrix var funcs)
  (do ((idx (+ -1 (length funcs)) (+ -1 idx))
       (row funcs (map (lambda (func) (diff func var)) row))
       (lst '() (cons row lst)))
      ((negative? idx) (reverse lst))))

(define (wronski-matrix var . funcs)
  (wronskian-matrix (expl->var var)
		      (map licit->polxpr
			   (if (= 1 (length funcs))
			       (car funcs)
			       funcs))))

;; The matrix constructed by placing the functions in the first row,
;; the first derivative of each function in the second row, and so on
;; through the n-1 derivative, thus forming a square matrix.

(defbltn 'wronski wronski-matrix)

(defbltn 'wronskian
  (lambda (var . funcs)
    (determinant (wronski-matrix var funcs))))

(defbltn 'eliminate
  (lambda (eqns vars)
    (poleqns->licits (eliminate (licits->poleqns eqns)
				(variables (normalize vars))))))

(defbltn 'polyelim
  (lambda (eqns vars)
    (poleqns->licits (poly:elim (licits->poleqns eqns) (variables vars)))))

(define (int:factor e1)
  (require 'factor)			;autoload from SLIB
  (sexp:terms->product-of-powers (sort! (factor e1) <)))

(defbltn 'factor
  (lambda (e0)
    (define (fctr e0)
      (let ((e1 (expr:normalize e0)))
	(cond ((number? e1) (int:factor e1))
	      ((rat:number? e1) (sexp:over (int:factor (num e1))
					   (int:factor (denom e1))))
	      (else (require 'hensel)
		    (rat:factor->sexp e1)))))
    (cond ((eqn? e0) (*->or-eqns (fctr (eqn->poly e0))))
	  (else (fctr e0)))))

(define (int:factors e1)
  (require 'factor)			;autoload from SLIB
  (terms->factors-list (sort! (factor e1) <)))

(define (rat:factors-list e1)
  (cond ((number? e1) (int:factors e1))
	((rat:number? e1)
	 (poly:sort-factors
	  (append (int:factors (num e1))
		  (negate-factors-exps (int:factors (denom e1))))))
	(else (require 'hensel)
	      (rat:factors e1))))

(defbltn 'factors
  (lambda (e1)
    (cond ((eqn? e1) (rat:factors-list (eqn->poly e1)))
	  (else (rat:factors-list (expr:normalize e1))))))

(defbltn 'prime?
  (lambda (n)
    (let ((e (licit->polxpr n)))
      (cond ((number? e)
	     (require 'factor)		;autoload from SLIB
	     (boolify (prime? e)))
	    (else (bltn:error 'not-a-number n))))))

(defbltn 'matrix
  (lambda args (apply matrix args)))

(defbltn 'genmatrix
  (lambda (fun i2 j2 . i1j1)
    (let ((i1 1) (j1 1))
      (cond ((null? i1j1))
	    ((begin (set! i1 (car i1j1))
		    (set! i1j1 (cdr i1j1))
		    (set! j1 i1)
		    (null? i1j1)))
	    ((begin (set! j1 (car i1j1))
		    (set! i1j1 (cdr i1j1))
		    (null? i1j1)))
	    (else (math:error 'genmatrix wna)))
      (mtrx:genmatrix
       fun
       (plicit->integer i2)
       (plicit->integer j2)
       (plicit->integer i1)
       (plicit->integer j1)))))

(defbltn 'augcoefmatrix
  (lambda (eqns vars)
    (augcoefmatrix (licits->poleqns eqns) (variables vars))))

(defbltn 'coefmatrix
  (lambda (eqns vars)
    (coefmatrix (licits->poleqns eqns) (variables vars))))

(defbltn 'rank
  rank)

(defbltn 'ident
  (lambda (n) (mtrx:scalarmatrix n 1)))

(defbltn 'scalarmatrix
  (lambda (n x) (mtrx:scalarmatrix (plicit->integer n) x)))

(defbltn 'diagmatrix
  (lambda args (mtrx:diagmatrix args)))

(defbltn 'determinant
  (lambda (m) (determinant m)))

(defbltn 'charpoly
  charpoly)

(defbltn 'crossproduct
  (lambda (x y) (crossproduct x y)))

(defbltn 'dotproduct
  (lambda (x y) (dotproduct x y)))

(defbltn 'ncmult
  (lambda (x y) (ncmult x y)))

(defbltn 'row
  (lambda (m i)
    (if (matrix? m)
	(list-ref m (+ -1 (plicit->integer i)))
	(bltn:error 'row-of-non-matrix?:- m))))

(defbltn 'col
  (lambda (m i)
    (cond ((matrix? m)
	   (map (lambda (row)
		  (list (list-ref row (+ -1 (plicit->integer i)))))
		m))
	  ((bunch? m) (list-ref m (plicit->integer i)))
	  (else (bltn:error 'column-of-non-matrix?:- m)))))

(defbltn 'minor
  (lambda (m i j)
    (mtrx:minor m (plicit->integer i) (plicit->integer j))))

(defbltn 'cofactor
  (lambda (m i j)
    (cofactor m (plicit->integer i) (plicit->integer j))))

(defbltn 'transpose
  (lambda (m) (transpose m)))

(defbltn 'cartprod
  (lambda (m)
    (require 'combinatorics)
    (cart-prod m)))

(defbltn 'factorial
  (lambda (m)
    (require 'combinatorics)
    (factorial m)))

(defbltn 'elementwise
  (lambda (f . args)
    (apply map (lambda args (sapply f args)) args)))

(defbltn 'finv
  (lambda (f)
    (fcinverse f)))

(defbltn 'load
  (lambda (file)
    (slib:load (var->string (expl->var file)))
    file))

(defbltn 'require
  (lambda (file)
    (slib:load (in-vicinity jacal-vicinity (var->string (expl->var file))))
    file))				;this should use require.
					;need to mess with file extension.

(defbltn 'batch
  (lambda (file)
    (batch (var->string (expl->var file)))
    novalue))

(defbltn 'transcript
  (lambda files
    (cond ((null? files)
	   (transcript-off)
	   novalue)
	  ((not (null? (cdr files))) (bltn:error 'transcript wna files))
	  (else
	   (let ((file (var->string (expl->var (car files)))))
	     (transcript-on file)
	     (car files))))))

(defbltn 'system
  (lambda (command)
    (system (var->string (expl->var command)))
;    command		;uncomment this line if system doesn't return nicely
    ))

(defbltn 'diff
  (lambda (exp . args)
    (reduce-init diff exp (map expl->var args))))

(defbltn 'polydiff
  (lambda (exp . args)
    (reduce-init expls:diff exp (map expl->var args))))

(defbltn 'partial
  (lambda (func . args)
    (cond ((number? func) (bltn:error 'not-a-function? func))
	  ((null? args) (bltn:error 'no-variables?))
	  ((not (clambda? func)) (apply deferop _partial func args))
	  (else
	   (reduce-init
	    diff func
	    (map (lambda (a)
		   (cond ((and (number? a) (positive? a)) (lambda-var a 0))
			 ((clambda? a) (expl->var a))
			 (else (math:error 'partial-with-respect-to? a))))
		 args))))))

(defbltn 'scheme
  (lambda (expr)
    (cond ((expl:var? expr)
	   (sexp->math
	    (call-with-input-string (var->string (expl->var expr)) read)))
	  (else
	   (write-sexp (if (sexp? expr) expr (cano->sexp expr horner))
		       (get-grammar 'schemepretty))
	   novalue))))

(define (make-grammar-bltn name)
  (lambda (expr)
    (define grammar (get-grammar name))
    (cond ((expl:var? expr)
	   (sexp->math
	    (call-with-input-string (var->string (expl->var expr))
	      (lambda (port)
		(prec:parse (grammar-read-tab grammar) #\; port)))))
	  (else
	   (write-sexp (if (sexp? expr) expr (cano->sexp expr horner))
		       grammar)
	   novalue))))

(defbltn 'tex (make-grammar-bltn 'tex))
(defbltn 'standard (make-grammar-bltn 'standard))
(defbltn 'disp2d (make-grammar-bltn 'disp2d))

(defbltn '(abs cabs)
  (lambda (exp)
    (let ((e1 (expr:normalize exp)))
      (cond ((rat? e1)
	     (app* $1/$2
		   (poly:cabs (rat:num e1))
		   (poly:cabs (rat:denom e1))))
	    ((expl? e1) (poly:cabs e1))
	    (else (bltn:error 'abs-of-non-rational-expression exp))))))

(defbltn 'realpart
  (lambda (exp)
    (let ((e1 (expr:normalize exp)))
      (cond ((rat? e1)
	     (app* $1/$2
		   (poly:coeff (rat:num e1) %i 0)
		   (rat:denom e1)))
	    ((expl? e1) (poly:coeff e1 %i 0))
	    (else (bltn:error 'abs-of-non-rational-expression exp))))))

(defbltn 'imagpart
  (lambda (exp)
    (let ((e1 (expr:normalize exp)))
      (cond ((rat? e1)
	     (app* $1/$2
		   (poly:coeff (rat:num e1) %i 1)
		   (rat:denom e1)))
	    ((expl? e1) (poly:coeff e1 %i 1))
	    (else (bltn:error 'abs-of-non-rational-expression exp))))))

(defbltn 'extrule
  (lambda (x) (poly->eqn (or (extrule (expl->var x)) 0))))

;;; commands for debugging:

(defbltn 'chain
  (lambda (exp)
    (let ((e (expl->var exp)))
      (poly->eqn (chain-rule e (var:differential e))))))

(defbltn 'shadow
  (lambda (x) (map (lambda (v) (if v (var->expl v) '()))
		   (or (vector-ref (expl->var x) 4) '()))))
