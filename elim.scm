;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1998, 1999, 2001, 2002, 2005, 2007, 2009, 2019, 2020, 2024, 2026 Aubrey Jaffer.
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
  (do ((vs vars (cdr vs)) (polys poleqns) (poly #f))
      ((or (null? vs)
	   (not (some (lambda (p) (poly:find-var? p (car vs))) polys)))
       polys)
    (do ((var (car vs))
	 (pl polys (if (null? pl)
		       (math:error 'not-enough-equations poleqns vars)
		       (cdr pl)))
	 (npl '() (cons (car pl) npl)))
	((poly:find-var? (car pl) var)
	 (set! poly (car pl))
	 (do ((pls (cdr pl) (cdr pls)))
	     ((null? pls) (set! polys npl))
	   (if (bunch? (car pls)) (math:error 'elim-bunch? (car pls)))
	   (set! npl (cons (poly:elim2 var poly (car pls)) npl))))
      (if (bunch? (car pl)) (math:error 'elim-bunch? (car pl))))))

;(define (poly:restest p1 p2 var)
;  (let ((v1 (poly:resultant p1 p2 var))
;	(v2 (poly:elim2 var p1 p2)))
;    (cond ((not (math:equal? v1 v2))
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

;;;EVS are all the extension vars used in extensions which are
;;; not being eliminated.
;;;IEVS are those EVS which involve VARS.
(define (ext:elim vars poleqns)
  (elim-diag
   'ext:elim
   (lambda (vars poleqns)
     (define eqs (remove-if impl? poleqns))
     (define exps (remove-if-not impl? poleqns))
     ;; (math:print 'ext:elim vars poleqns)
     (if (> (length exps) 1)
	 (math:error 'eliminating-from-more-than-one-expression? exps))
     (let* ((aes (extensions poleqns))
	    (evs (set-difference aes vars)) ; needed for integer recurrences
	    (ievs (remove-if-not
		   (lambda (ev)
		     ;; (math:print ev 'depends (var:depends ev) 'vars vars)
		     (intersection? (var:depends ev) vars))
		   evs)))
       ;; (if (not (null? aes)) (math:print 'aes aes 'evs evs 'ievs ievs 'vars vars))
       (cond
	((not (null? ievs))
	 ;;ievs are the new extensions after any VARS are eliminated
	 (do ((ievs ievs (cdr ievs)))
	     ((null? ievs))
	   (let* ((iev (car ievs)))
	     (define tiev
	       (var:elim iev
			 (remove-if (lambda (x) (poly:find-var? x iev)) eqs)
			 vars))
	     (set! eqs
;;; this bit of magic keeps fractions working in transcendental reductions.
		   (cons
		    (univ:demote (univ:norm0 iev (if (impl? tiev) (cdr tiev) (list tiev -1))))
		    eqs))
	     (set! vars (cons iev vars))))))
       (poly:elim (append eqs exps) vars)))
   vars poleqns))

(define (var:elim var eqs ovars)
  (elim-diag
   'var:elim
   (lambda (vars eqs)
     (define var (car vars))
     (define ovars (cdr vars))
     (define (doarg e)
       (define reds (ext:elim ovars (cons (licit->impl e) eqs)))
       (cond ((and (pair? reds) (null? (cdr reds)))
	      (canonicalize (car reds)))
	     (else (math:error 'var:elim ovars 'from (cons e eqs) '==> reds))))
     (define varglst (var:arglist var))
     (cond ((radicalvar? var)
	    (let ((neqs (cons (expl->impl (car varglst)) eqs)))
	      (define reds (poly:elim neqs ovars))
	      (cond ((and (pair? reds) (null? (cdr reds)))
		     (^ (car reds) (cadr varglst)))
		    (else
		     (math:error 'var:elim ovars 'from neqs '--> reds)))))
	   ((var:recurrence? var)
	    (apply rref (map doarg varglst)))
	   ((var:dffrule var)
	    (let ((func (var:func var))
		  (args (map doarg varglst)))
	      (cond ((symbol? (var:sexp (expl->var func)))
		     (tcall (expl->var func) (car args)))
		    (else (mapply func args)))))
	   ;; ((pair? (var:sexp var))
	   ;;  (let ((func (doarg (var:func var)))
	   ;; 	  (args (map doarg varglst)))
	   ;;    (apply deferop func args)))
	   (else (math:error 'elimination-type-not-handled var))))
   (cons var ovars)
   eqs))

;;; This tries to solve the equations no matter what is involved.
;;; It will eliminate variables in bunches of equations.
(define (eliminate vars eqns)
  (elim-diag
   'eliminate
   (lambda (vars eqns)
     (define (bunch:norm x)
       (cond ((null? x) x)
	     ((not (null? (cdr x))) x)
	     ((number? (car x))
	      (math:advise 'ELIMINATE 'singular-reduction x)
;;; why does this work?
	      novalue)
	     (else (car x))))
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
		  (lambda args (eliminate vars args))
		  (map (lambda (eqn)
			 (if (bunch? eqn)
			     eqn
			     (make-list len eqn)))
		       eqns)))
	 (bunch:norm (ext:elim vars eqns))))
   vars eqns))

(define (elim:test)
  (define a (sexp->var 'a))
  (define x (sexp->var 'x))
  (define y (sexp->var 'y))
  (math:test (list (list a 0 0 124 81 11 3 45))
	poly:elim
	(list (list y (list x (list a 0 0 2) (list a 0 1)) 1)
	      (list y (list x (list a 5 1) 0 -1) 0 1)
	      (list y (list x (list a -1 3) 5) -1))
	(list x y)))

;;; applies PROC to B, preserving bunch structure of B.
;;; LICIT:MAP is used by LICIT:DO-VARS.
;;; IMPLICIT:MAP is used by CAPPLY.
(define (licit:map proc b)
  (cond ((bunch? b) (map (lambda (x) (licit:map proc x)) b))
	((eqn? b) (poleqn->licit (proc (eqn->poly b))))
	(else (proc b))))
(define (implicit:map proc b)
  (cond ((bunch? b) (map (lambda (x) (implicit:map proc x)) b))
	((eqn? b) (poleqn->licit (proc (eqn->poly b))))
	((expl? b) (proc (expl->impl b)))
	(else (proc b))))

;;; replaces each var in poly with (proc var).
;;; Used for substitutions in CLAMBDA and CAPPLY.

(define (poly:do-vars proc poly)
  (cond ((number? poly) poly)
	((pair? poly)
	 (univ:demote (cons (proc (car poly))
			    (map (lambda (b) (poly:do-vars proc b))
				 (cdr poly)))))
	(else (jacal:found-bug 'poly:do-vars 'not-a-poly poly)
	      novalue)))
(define (licit:do-vars proc licit)
  (licit:map (lambda (poly) (poly:do-vars proc poly))
	     licit))

;; canonical lambda expression; does not descend into extension vars
(define (clambda? cexp)
  ;; (math:print cexp 'CLAMBDA?
  (cond ((number? cexp) #f)
	((eqn? cexp) (poly:find-var-if? (eqn->poly cexp) lambdavardep?))
	((bunch? cexp) (some clambda? cexp))
	((expr? cexp) (poly:find-var-if? cexp lambdavardep?))
	(else #f)))

;;;In order to keep the lambda application hygienic (in case a function
;;;of a function is called), we need to substitute occurences of
;;;lambda variables in the body with shadowed versions of the
;;;variables before we eliminate them.  See:
;;;	Technical Report No. 194
;;;	Hygienic Macro Expansion
;;;	E.E.Kohlbecker, D.P.Friedman, M.Fellinson, and B.Duba
;;;	Indiana University
;;;	May, 1986

;;; The bumped-only case is different from the some-bumped
;;; some-shadowed case in that it returns a publicly available (not
;;; shadowed) var.  This is called from var:shadow in "types.scm".
(define (var:lambda-bump var delta)
  (if (simple-lambdavar? var)
      (lambda-var (+ (var:def var) delta) (var:diffcnt var))
      (sexp->var
       (do-sexp-symbols
	(lambda (s)
	  (define st (symbol->string s))
	  (if (and (> (string-length st) 1) (char=? #\@ (string-ref st 0)))
	      (var:sexp (lambda-var
			 (+ delta (string->number
				   (substring st 1 (string-length st))))
			 0))
	      s))
	(var:sexp var)))))

;;; used by MAKE-SHADOW VAR:LAMBDA-BUMP
(define (do-sexp-symbols proc sexp)
  (cond ((symbol? sexp) (proc sexp))
	((pair? sexp) (map (lambda (s) (do-sexp-symbols proc s)) sexp))
	(else sexp)))

;;; CAPPLY maps the clambda over the elements of buches using
;;; LICIT:DO-VARS; for example, sqrt([a,b]) --> [sqrt(a),sqrt(b)]
(define (capply body args)
  (trace-diag
   'capply
   (lambda (body args)
     (define svlist '())
     (define impls (licits->impls args))
     (define arg-count (length impls))
;;; first, shadow the lambda-dependent extension vars while collecting
;;; them into svlist.
     (define sbody
       (licit:do-vars
	(lambda (var)
	  (cond ((lambdavardep? var)
		 ;; (math:print var 'LAMBDAVARDEP? (lambdavardep? var))
		 (set! var (var:shadow var arg-count))
		 (set! svlist
		       (union (remove-if-not
			       simple-shadowed-lambdavar?
			       (adjoin var (var:depends var)))
			      svlist))
		 var)
		(else var)))
	body))
;;; then collect derivatives for any differentials in sbody
     (define dargs (diffargs svlist impls))
     ;; (math:print 'sbody sbody)
;;; finally, use ELIMINATE to replace all shadowed vars
     (implicit:map (lambda (p) (eliminate svlist (math:adjoin p dargs))) sbody))
   body args))

;;; used only in CAPPLY
(define (diffargs vlist args)
;;; BUNCH:MAP is used only by DIFFARGS
  (define (bunch:map proc b)
    (cond ((bunch? b) (map (lambda (x) (bunch:map proc x)) b))
	  (else (proc b))))
  (define (diffarg var args)
    (cond ((var:differential? var)
	   (total-differential (diffarg (var:undiff var) args)))
	  (else (list-ref args (- (var:def var) 1)))))
  (map (lambda (var)
	 (bunch:map (lambda (e)
		      (univ:demote (cons var (cdr (licit->impl e)))))
		    (diffarg var args)))
       vlist))

(define (var:max-lambda-position var)
  (let ((maxpos 0))
    (for-each (lambda (x)
		(cond ((simple-lambdavar? x)
		       (set! maxpos (max maxpos (var:def x))))))
	      (adjoin var (var:depends var)))
    maxpos))

(define (var:min-lambda-position var)
  (let ((minpos 9999))
    (for-each (lambda (x)
		(cond ((simple-lambdavar? x)
		       (set! minpos (min minpos (var:def x))))))
	      (adjoin var (var:depends var)))
    (cond ((= minpos 9999)
	   ;; (math:error 'var:min-lambda-position var)
	   0)
	  (else minpos))))

(define (licit:deep-arity top-licit)
  (define maxpos 0)
  (define deps '())
  (define (do-licit! licit)
    (licit:for-each-var
     (lambda (v)
       (cond ((simple-lambdavar? v)
	      (set! maxpos (max maxpos (var:def v))))
	     ((procedure? (var:def v)))
	     ((memq v deps))
	     (else
	      (set! deps (cons v deps))
	      ;; mutually exclusive
	      (cond ((var:dffrule v) (do-licit! (var:dffrule v)))
		    ((var:algrule v) (do-licit! (var:algrule v)))))))
     licit))
  (do-licit! top-licit)
  maxpos)
