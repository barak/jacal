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

(require 'common-list-functions)

;;;; Variable elimination
(define (poly:elim poleqns vars)
;;;   (cond (math:trace
;;; 	 (display-diag 'eliminating:) (newline-diag)
;;; 	 (math:write (map var->expl vars) *output-grammar*)
;;; 	 (display-diag 'from:) (newline-diag)
;;; 	 (math:write (poleqns->licits poleqns) *output-grammar*)))
  (do ((vs vars (cdr vs)) (polys poleqns) (poly #f))
      ((null? vs)
;;;        (cond (math:trace (display-diag 'yielding:) (newline-diag)
;;; 			 (math:write polys *output-grammar*)))
       polys)
    (do ((var (car vs))
	 (pl polys (if (null? pl)
		       (math:error 'not-enough-equations poleqns vars)
		       (cdr pl)))
	 (npl '() (cons (car pl) npl)))
	((poly:find-var? (car pl) var)
	 (set! poly (poly:promote var (car pl)))
	 (do ((pls (cdr pl) (cdr pls)))
	     ((null? pls) (set! polys npl))
	   (if (bunch? (car pls)) (math:error 'elim-bunch? (car pls)))
	   (set! npl (cons (poly:elim2 poly (car pls) var) npl))))
      (if (bunch? (car pl)) (math:error 'elim-bunch? (car pl))))))

;(define (poly:restest p1 p2 var)
;  (let ((v1 (poly:resultant p1 p2 var))
;	(v2 (poly:elim2 p1 p2 var)))
;    (cond ((not (equal? v1 v2))
;	   (display-diag "  restest:") (newline-diag)
;	   (math:write (poleqns->licits p1) *output-grammar*)
;	   (newline)
;	   (math:write (poleqns->licits p2) *output-grammar*)
;	   (display-diag "  ==>") (newline-diag)
;	   (math:write (poleqns->licits v1) *output-grammar*)
;	   (display-diag "different from:") (newline-diag)
;	   (math:write (poleqns->licits v2) *output-grammar*)))
;    v2))

(define (intersection? l1 l2)
  (cond ((null? l1) #f)
	((null? l2) #f)
	((memq (car l1) l2) #t)
	(else (intersection? (cdr l1) l2))))

;; (sort! ?? (lambda (x y) (< (ext:depth x) (ext:depth y))))
;(define (ext:depth v)
;  (let ((vds (var:depends v)))
;    (if vds (+ 1 (apply max (map ext:depth vds)))
;	1)))

;;;EVS are all the extension vars used in extensions which are
;;; not being eliminated.
;;;IEVS are those EVS which involve VARS.
;;; sort them so nested extensions are done last.
(define (ext:elim poleqns vars)
  (let* ((eqs (remove-if impl? poleqns))
	 (exps (remove-if-not impl? poleqns)))
    (if (> (length exps) 1)
	(math:error 'eliminating-from-more-than-one-expression? exps))
    (let* ((aes (map chainables poleqns))
	   (evs (set-difference (reduce union aes) vars))
	   (ievs (remove-if-not
		  (lambda (ev) (intersection? (var:depends ev) vars))
		  evs)))
      (cond
       ((not (null? ievs))
	;;tievs are the new extensions after any VARS are eliminated
	(do ((ievs ievs (cdr ievs)))
	    ((null? ievs))
	  (let* ((iev (car ievs))
		 (tiev (var:elim iev
				 (remove-if (lambda (x) (poly:find-var? x iev))
					    eqs)
				 vars)))
	    (set! eqs (cons (univ:norm0
			     iev
			     (if (expl? tiev) (list tiev -1) (cdr tiev)))
			    eqs))
	    (set! vars (cons iev vars))))))
      (poly:elim (append eqs exps) vars))))

(define (var:elim var eqs ovars)
  (let* ((vars (var:depends var)))
    (cond ((null? vars) (math:warn 'free-var-to-var:elim var ovars))
	  ((radicalvar? var)
	   (^ (find-if
	       impl?
	       (poly:elim (cons (expl->impl (cadr (func-arglist var))) eqs)
			  ovars))
	      (caddr (func-arglist var))))
	  ((pair? (var:sexp var))
	   (apply deferop
		  (map (lambda (e)
			 (normalize
			  (find-if impl? (ext:elim
					  (cons (licit->impl e) eqs)
					  ovars))))
		       (func-arglist var))))
	  (else (math:error 'elimination-type-not-handled var)))))

;;; This tries to solve the equations no matter what is involved.
;;; It will eliminate variables in vectors of equations.
(define (eliminate eqns vars)
  (bunch:norm
   (if (some bunch? eqns)
       (let ((len #f))
	 (for-each (lambda (eqn)
		     (cond ((not (bunch? eqn)))
			   ((not len) (set! len (length eqn)))
			   ((eqv? (length eqn) len))
			   (else (math:error
				  'bunches-to-eliminate-not-same-length
				  len eqns))))
		   eqns)
	 (apply map
		(lambda arglist (eliminate arglist vars))
		(map (lambda (eqn)
		       (if (bunch? eqn)
			   eqn
			   (make-list len eqn)))
		     eqns)))
       (ext:elim eqns vars))))

(define (elim:test)
  (define a (sexp->var 'a))
  (define x (sexp->var 'x))
  (define y (sexp->var 'y))
  (test (list (list a 0 0 124 81 11 3 45))
	poly:elim
	(list (list y (list x (list a 0 0 2) (list a 0 1)) 1)
	      (list y (list x (list a 5 1) 0 -1) 0 1)
	      (list y (list x (list a -1 3) 5) -1))
	(list x y)))

(define (bunch:map proc b)
  (cond ((bunch? b) (map (lambda (x) (bunch:map proc x)) b))
	(else (proc b))))
(define (licits:for-each proc b)
  (cond ((bunch? b) (for-each (lambda (x) (licits:for-each proc x)) b))
	((eqn? b) (proc (eqn->poly b)))
	(else (proc b))))
(define (licits:map proc b)
  (cond ((bunch? b) (map (lambda (x) (licits:map proc x)) b))
	((eqn? b) (poleqn->licit (proc (eqn->poly b))))
	(else (proc b))))
(define (implicits:map proc b)
  (cond ((bunch? b) (map (lambda (x) (implicits:map proc x)) b))
	((eqn? b) (poleqns->licits (proc (eqn->poly b))))
	((expl? b) (proc (expl->impl b)))
	(else (proc b))))

;;; replaces each var in poly with (proc var).
;;; Used for substitutions in clambda and capply.

(define (poly:do-vars proc poly)
  (if (number? poly) poly
      (univ:demote (cons (proc (car poly))
			 (map (lambda (b) (poly:do-vars proc b))
			      (cdr poly))))))
(define (licits:do-vars proc licit)
  (licits:map (lambda (poly) (poly:do-vars proc poly))
	      licit))

;;;; Canonical Lambda
;;;; This needs to handle algebraic extensions as well.
(define (clambda symlist body)
  (let ((num-new-vars (length (remove-if lambdavar? symlist))))
    (licits:do-vars
     (lambda (var)
       (let ((pos (position (var:nodiffs var) symlist)))
	 (cond (pos (lambda-var (+ 1 pos) (var:diff-depth var)))
	       ((lambdavar? var) (var:lambda-bump var num-new-vars))
	       ((lambdavarext? var) (bump-lambda-ext))
	       (else var))))
     body)))

(define (clambda? cexp)
  (cond ((number? cexp) #f)
	((bunch? cexp) (some clambda? cexp))
	((expr? cexp) (poly:find-var-if? cexp lambdavardep?))
	((eqn? cexp) (poly:find-var-if? (eqn->poly cexp) lambdavardep?))
	(else #f)))

;;;In order to keep the lambda application hygenic (in case a function
;;;of a function is called), we need to substitute occurences of
;;;lambda variables in the body with shadowed versions of the
;;;variables before we eliminate them.  See:
;;;	Technical Report No. 194
;;;	Hygenic Macro Expansion
;;;	E.E.Kohlbecker, D.P.Friedman, M.Fellinson, and B.Duba
;;;	Indiana University
;;;	May, 1986

;;; The bumped-only case is different from the some-bumped
;;; some-shadowed case in that it returns a publicly available (not
;;; shadowed) var.  This is called from var:shadow in "types.scm".
(define (var:lambda-bump var delta)
  (if (simple-lambdavar? var)
      (lambda-var (+ (lambda-position var) delta) (var:diff-depth var))
      (sexp->var
       (do-sexp-symbols
	(lambda (s)
	  (define st (symbol->string s))
	  (if (and (char=? #\@ (string-ref st 0)) (> (string-length st) 1))
	      (var:sexp (lambda-var
			 (+ delta (string->number
				   (substring st 1 (string-length st))))
			 0))
	      s))
	(var:sexp var)))))
(define (do-sexp-symbols proc sexp)
  (cond ((symbol? sexp) (proc sexp))
	((pair? sexp) (map (lambda (s) (do-sexp-symbols proc s)) sexp))
	(else sexp)))

;;; currently capply puts the structure of the clambda inside the
;;; structure of the arguments.
(define (capply body larglist)
  (let* ((arglist (licits->impls larglist))
	 (arglist-length (length arglist))
	 (svlist '())
	 (sbody
	  (licits:do-vars
	   (lambda (var)
	     (cond
	      ((lambdavardep? var)
	       (set! var (var:shadow var arglist-length))
	       (set! svlist (union (remove-if-not
				    simple-shadowed-lambdavar?
				    (cons var (var:depends var)))
				   svlist))
	       var)
	      (else var)))
	   body))
	 (dargs (diffargs svlist arglist)))
    (implicits:map (lambda (p) (eliminate (cons p dargs) svlist)) sbody)))

(define (diffargs vlist args)
  (map (lambda (var)
	 (bunch:map (lambda (e)
		      (univ:demote (cons var (cdr (licit->impl e)))))
	   (diffarg var args)))
    vlist))
(define (diffarg var args)
  (cond ((var:differential? var)
	 (total-differential (diffarg (var:undiff var) args)))
	(else (list-ref args (- (lambda-position var) 1)))))

(define (licits:for-each-var proc polys)
  (licits:for-each (lambda (poly) (poly:for-each-var proc poly)) polys))

(define (licits:max-lambda-position polys)
  (let ((maxpos 0) (deps '()))
    (licits:for-each-var
     (lambda (v) (cond ((lambdavardep? v)
			(set! maxpos (max maxpos (var:max-lambda-position v)))
			(set! deps (adjoin v deps)))))
     polys)
    (for-each
     (lambda (v)
       (for-each
	(lambda (x) (if (lambdavardep? x)
			(set! maxpos (max maxpos (var:max-lambda-position x)))))
	(var:depends v)))
     deps)
    maxpos))

(define (var:max-lambda-position var)
  (let ((maxpos 0))
    (for-each
     (lambda (x)
       (if (lambdavar? x)
	   (set! maxpos (max maxpos (or (lambda-position x) maxpos)))))
     (cons var (var:depends var)))
    maxpos))

(define (var:min-lambda-position var)
  (let ((minpos 9999))
    (for-each
     (lambda (x)
       (if (lambdavar? x)
	   (set! minpos (min minpos (or (lambda-position x) minpos)))))
     (cons var (var:depends var)))
    (if (= minpos 9999) (math:error 'var:min-lambda-position var))
    minpos))
