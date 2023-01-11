;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1997, 2019, 2020, 2021 Aubrey Jaffer.
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

;;(proclaim '(optimize (speed 3) (compilation-speed 0)))

(define (vsubst new old e)
  (cond ((eq? new old) e)
	((number? e) e)
	((bunch? e) (map (lambda (e) (vsubst new old e)) e))
	(else (univ:demote (cons new (cdr (poly:promote old e)))))))

(define (make-var-eqn new old)
  (if (var:> old new)
      (list old (list new 0 -1) 1)
      (list new (list old 0 -1) 1)))

;;; used by fcinverse; doesn't work for radical functions.
(define (swapvars x y p)
  (vsubst x _$
    (vsubst y x
      (vsubst _$ y p))))

;; canonicalizers
(define (normalize x)
  (cond ((and math:phases (not (novalue? x)))
	 (display-diag 'normalizing:)
	 (newline-diag)
	 (math:write x *output-grammar*)))
  (let ((ans (normalize1 x)))
    (cond ((and math:phases (not (novalue? x)))
	   (display-diag 'yielding:)
	   (newline-diag)
	   (math:write ans *output-grammar*)))
    ans))
(define (normalize1 x)
  (cond ((bunch? x) (map normalize x))
	((symbol? x) (eval:error 'normalize-symbol?- x))
	((eqn? x)
	 (poly->eqn (unitcan (poly:square-and-num-cont-free
			      (alg:simplify (eqn->poly x))))))
	(else (expr:normalize x))))
(define (expr:normalize p)
  (if (expl? p) (set! p (expl->impl p)))
  (expr:norm-or-unitcan
   (poly:square-free-var
    (alg:simplify (if (impl? p) (alg:clear-leading-exts p) p))
    $)))
(define (extize p var)
  (cond ((bunch? p) (eval:error 'cannot-suchthat-a-vector p))
	(var
	 (let ((deps (var:depends var)))
	   (if (null? deps)
	       (set! deps (remove var (var:build-depends (list p)))))
	   (var:set-depends! var deps)
	   (var:set-pri! var (if (null? deps)
				 10	;must be a constant.
				 (+ 1 (apply max (map var:pri deps)))))
	   (var:set-atdef! var (vsubst $ var p))
	   (var:set-def! var p)
	   (set! var-news (cons var var-news))
	   (var->expl var)))
	((eqn? p) p)
	((expl? p) p)
	((rat? p) p)
	(else
	 (set! newextstr (chap:next-string newextstr))
	 (let ((v (defext (string->var (if (clambda? p)
					   (string-append "@" newextstr)
					   newextstr))
		    p)))
	   (set! var-news (cons v var-news))
	   (var->expl v)))))

;(trace normalize normalize1 extize unitcan
;       expr:norm-or-unitcan expr:normalize
;       alg:simplify alg:clear-leading-exts
;       poly:square-free-var poly:square-and-num-cont-free)

;; differentials

;; The sum over vars of dv*diff(p,v)
(define (total-diffn p vars)
  (if (null? vars) 0
      (poly:+ (poly:* (var->expl (var:differential (car vars)))
		      (poly:diff p (car vars)))
	      (total-diffn p (cdr vars)))))

(define (chain-rule v vd)
  (if (extrule v)
      (total-chain-exts (total-diffn (extrule v) (poly:vars (extrule v)))
			(var:funcs (extrule v)))
      (let ((functor (sexp->math (car (var:sexp v)))))
	(do ((pos 1 (+ 1 pos))
	     (al (cdr (func-arglist v)) (cdr al))
	     (sum 0 (app* $1*$2+$3
			  (apply deferop
				 (deferop _partial functor pos)
				 (cdr (func-arglist v)))
			  (total-differential (car al))
			  sum)))
	    ((null? al) (vsubst vd $ sum))))))

(define (total-chain-exts drule exts)
  (if (null? exts) drule
      (let ((ed (var:differential (car exts))))
	(define extrule1 (extrule (car exts)))
	(cond ((not extrule1)
	       (total-chain-exts drule (cdr exts)))
	      ((poly:find-var? extrule1 ed)
	       (total-chain-exts
		(poly:resultant drule extrule1 ed)
		(cdr exts)))
	      (else
	       (total-chain-exts
		(poly:resultant drule (chain-rule (car exts) ed) ed)
		(union (cdr exts) (poly:exts extrule1))))))))

(define (total-differential a)
  (cond
   ((bunch? a) (map total-differential a))
   ((eqn? a) (poly->eqn
	      (total-diffn (eqn->poly a) (poly:vars (eqn->poly a)))))
   (else (let ((aes (chainables a)))
	   (if (and (null? aes) (expl? a))
	       (total-diffn a (poly:vars a))
	       (let ((pa (licit->poleqn a)))
		 (total-chain-exts
		  (vsubst $ d$ (poly:resultant
				pa (total-diffn pa (poly:vars pa)) $))
		  aes)))))))


(define (diff a var)
  (cond
   ((bunch? a) (map (lambda (x) (diff x var)) a))
   ((eqn? a) (poly->eqn (diff (eqn->poly a) var)))
   (else (let ((td (total-differential a))
	       (vd (var->expl (var:differential var))))
	   (define td1 (app* $1/$2 td vd))
	   (define dpvs '())
	   (poly:for-each-var
	    (lambda (v) (if (and (not (eq? (car vd) v))
				 (var:differential? v))
			    (set! dpvs (adjoin v dpvs))))
	    td)
	   (reduce-init (lambda (e x) (poly:coeff e x 0))
			(poly:square-free-var td1 $)
			dpvs)))))

;; (trace total-differential total-chain-exts chain-rule total-diffn diff)

;;;; FINITE DIFFERENCES
;;; shift needs to go through extensions; which will create new
;;; extensions (yucc).	It is clear what to do for radicals, but other
;;; extensions will be hard to link up.  For instance y: {x|x^5+a+b+9+x}
;;; needs to yield the same number whether a or b is substituted first.
;; (define (shift p var)
;;   (vsubst var
;; 	  $2
;; 	  (poly:resultant (list $2 (list var -1 -1) 1)
;; 			  p
;; 			  var)))
;; (define (unsum p var)
;;   (app* $1-$2 p (shift p (expl->var var))))
;; (define (poly:fdiffn p vars)
;;   (if (null? vars) 0
;;     (poly:+ (poly:* (var->expl (var:finite-differential (car vars)))
;; 		    (unsum p (car vars)))
;; 	    (poly:fdiffn p (cdr vars)))))
;; (define (total-finite-differential e)
;;   (if (bunch? e)
;;       (map total-finite-differential e)
;;     (poly:fdiffn e (alg:vars e))))

;;;logical operations on licits
;(define (impl:not p)
;  (poly:+ (poly:* (licit->poleqn p)
;		  (var->expl (sexp->var (new-symbol "~")))) -1))

;(define (impl:and p . qs)
;  (cond ((bunch? p) (impl:and (append p qs)))))

(define (expl:t? e) (equal? e expl:t))
(define (ncexpt a pow)
  (cond ((not (or (integer? pow) (expl:t? pow)))
	 (math:error 'only-integers-and-t-allowed-for-ncexpt pow))
	((eqns? a) (math:error 'expt-of-equation?:- a))
	((not (bunch? a)) (fcexpt a pow))
	((expl:t? pow) (transpose a))
	(else (mtrx:expt a pow))))

;;;; Routines for square-free factoring
(define (poly:diff-coefs el n)
  (if (null? el)
      el
    (cons (poly:* n (car el))
	  (poly:diff-coefs (cdr el) (+ 1 n)))))
(define (poly:diff p var)
  (cond ((number? p) 0)
	((eq? (car p) var) (univ:norm0 var (poly:diff-coefs (cddr p) 1)))
	((var:> var (car p)) 0)
	(else (univ:norm0 (car p) (map-no-end-0s
				   (lambda (x) (poly:diff x var))
				   (cdr p))))))
;; (define (poly:diff-all p)
;;   (let ((ans 0))
;;     (do ((vars (poly:vars p) (cdr vars)))
;; 	((null? vars) ans)
;; 	(set! ans (poly:+ (poly:diff p (car vars)) ans)))))
