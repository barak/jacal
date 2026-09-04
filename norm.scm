;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1997, 1998, 2002, 2005, 2006, 2007, 2019, 2020, 2021, 2024, 2026 Aubrey Jaffer.
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

;;; used by fcinverse; doesn't work for radical functions.
;;; _$ is used as a temporary var
(define (swapvars x y p)
  (vsubst x _$
    (vsubst y x
      (vsubst _$ y p))))

(define (normalize x) (if (boolean? x) x (canorm x #f)))

;;; top-level normalize allows radicals in the denominator if their
;;; impl:total-degree is less
(define (canonicalize x) (if (boolean? x) x (canorm x #t)))

;;; CANORM accepts only licit arguments.
(define (canorm x can?)
  (phases-diag
   (if can? 'canonicalize 'normalize)
   (lambda (x)
     (cond ((bunch? x) (map (lambda (x) (canorm x can?)) x))
	   ((symbol? x) (eval:error 'normalize-symbol?- x))
	   ((eqn? x)
	    (poly->eqn (unitcan (poly:square-and-num-cont-free
				 (alg:simplify (eqn->poly x))))))
	   (can? (expr:canonicalize x))
	   (else (expr:numerads x))))
   x))

;;; push radicals into numerator.
(define (expr:numerads p)
  ;; (define p (univ:demote p0))
  (expr:norm-or-unitcan
   (poly:square-free-var (alg:simplify (alg:clear-leading-exts (licit->impl p)))
			 $)))
;;; Leave radicals in the denominator if the total degree is less,
;;; which screws up integration.
(define (expr:canorm poly)
  (expr:norm-or-unitcan
   (poly:square-free-var
    (alg:simplify (licit->impl poly))
    $)))

(define (expr:canonicalize poly)
  (if (expl? poly)
      (expr:canorm poly)
      (let ((p (licit->impl poly)))
	(define pnorm (expr:canorm p))
	(if (expl? pnorm) pnorm
	    (let ((pinv (expr:canorm (app* $1/$2 1 p))))
	      (if (rat:< (impl:total-degree pnorm)
			 (impl:total-degree pinv))
		  pnorm
		  (app* $1/$2 1 pinv)))))))

;; differentials

;; The sum over vars of dv*diff(p,v)
(define (total-diffn p vars)
  (if (null? vars) 0
      (poly:+ (poly:* (var->expl (var:differential (car vars)))
		      (poly:diff p (car vars)))
	      (total-diffn p (cdr vars)))))

(define (chain-rule v vd)
  (if (extrule v)
      (total-chain-exts (total-diffn (extrule v) (poly:variables (extrule v)))
			(poly:funcs (extrule v)))
      (let ((functor (seval (car (var:sexp v)) '())))
	(do ((pos 1 (+ 1 pos))
	     (al (var:arglist v) (cdr al))
	     (sum 0 (app* $1*$2+$3
			  (apply deferop
				 (deferop _partial functor pos)
				 (var:arglist v))
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
  ;; (define (tde a)
  ;;   (let ((aes (chainables a)))
  ;;     (if (and (null? aes) (expl? a))
  ;; 	  (total-diffn a (poly:vars a))
  ;; 	  (let ((pa (licit->poleqn a)))
  ;; 	    (define res (total-diffn pa (poly:vars pa)))
  ;; 	    (poly:coeff (total-chain-exts res aes) d$ 0)))))
  (define (td a)
    (let ((aes (chainables a)))
      (if (and (null? aes) (expl? a))
	  (total-diffn a (poly:vars a))
	  (let ((pa (licit->poleqn a)))
	    (define res
	      (vsubst $ d$ (poly:resultant
			    pa (total-diffn pa (poly:vars pa)) $)))
	    ;; (math:print (poly:degree res $) '--- res)
	    (total-chain-exts res aes)))))
  (cond ((bunch? a) (map total-differential a))
	((eqn? a)
	 ;; (poly->eqn (tde (eqn->poly a)))
	 (math:error 'total-differential 'equation 'not-allowed a) novalue)
	(else (td a))))

(define (diff a var)
  (cond
   ((number? a) 0)
   ((eqn? a) (math:error 'diff 'equation 'not-allowed a) novalue)
   ((bunch? a) (map (lambda (x) (diff x var)) a))
   ((var:constant? var) (math:error 'diff 'by-constant 'not-allowed var))
   (else (let ((td (total-differential a))
	       (vd (var:differential var)))
	   (define td1 (app* $1/$2 td (var->expl vd)))
	   (reduce-init (lambda (e x) (poly:coeff e x 0))
			(poly:square-free-var td1 $)
			(sort (remove vd (remove-if-not var:differential?
							 (poly:vars td)))
			       var:>))))))

(define (derivative a vrexp)
  (cond
   ((number? a) 0)
   ((eqn? a) (math:error 'derivative 'equation 'not-allowed a) novalue)
   ((bunch? a) (map (lambda (x) (derivative x vrexp)) a))
   ((licit:constant? vrexp) (math:error 'derivative 'by-constant 'not-allowed vrexp))
   (else (let ((td (total-differential a))
	       (vd (total-differential vrexp)))
	   (define td1 (app* $1/$2 td vd))
	   (reduce-init (lambda (e x) (poly:coeff e x 0))
			(poly:square-free-var td1 $)
			(sort
			 (set-difference
			  (remove-if-not var:differential? (poly:vars td))
			  (remove-if-not var:differential? (poly:vars vd)))
			 var:>))))))

;; (trace TOTAL-DIFFERENTIAL TOTAL-CHAIN-EXTS CHAIN-RULE TOTAL-DIFFN DIFF)

;;;logical operations on licits
;(define (impl:not p)
;  (poly:+ (poly:* (licit->poleqn p)
;		  (var->expl (sexp->var (new-symbol "~")))) -1))

;(define (impl:and p . qs)
;  (cond ((bunch? p) (impl:and (append p qs)))))

(define (expl:t? e) (math:equal? e expl:t))
(define (ncexpt a pow)
  (cond ((not (or (integer? pow) (expl:t? pow)))
	 (math:error 'only-integers-and-t-allowed-for-ncexpt pow))
	((eqns? a) (math:error 'expt-of-equation?:- a))
	((not (bunch? a)) (fcexpt a pow))
	((expl:t? pow) (transpose a))
	(else (mtrx:expt a pow))))

;;;; Routine for square-free factoring
(define (poly:diff p var)
  (define (diff-coeffs coeffs n)
    (if (null? coeffs)
	coeffs
	(cons (poly:* n (car coeffs))
	      (diff-coeffs (cdr coeffs) (+ 1 n)))))
  (cond ((number? p) 0)
	;; ((and (bunch? p) (display "!") #f))
	((eq? (car p) var)
	 (univ:norm0 var (diff-coeffs (cddr p) 1)))
	((var:> var (car p)) 0)
	(else (univ:norm0 (car p) (map-no-end-0s
				   (lambda (x) (poly:diff x var))
				   (cdr p))))))

;;; $=fc($1) --> $=fc^^-1($1)
(define (fcinverse fc)
  (extize #f (canonicalize (swapvars $1 $ (licit->impl fc)))))

;;; fc(fc(...fc($1)))
(define (fcexpt fc pow)
  (if (negative? pow)
      (fcexpt (fcinverse fc) (- pow))
      (ipow-by-squaring fc pow cidentity app*)))

;;;; RAPPLY dispatches to Scheme procedures or CAPPLY in the case of
;;;; (local) CLAMBDA? expressions.
(define (rapply fxpr args)
  (trace-diag 'rapply
	      (lambda (fxpr args)
		(cond ((rat:number? fxpr) fxpr)
		      ((expl:var? fxpr) =>
		       (lambda (fxprv)
			 (cond ((and (var:recurrence? fxprv)
				     (procedure? (var:def fxprv)))
				(apply (var:def fxprv) (map canonicalize args)))
			       (else (math:error 'RAPPLY 'wta fxpr)
				     novalue))))
		      ((clambda? fxpr) (capply fxpr args))
;;; not a builtin; must be a transcendental or unknown function.
;;; core transcendental function has definition and takes only one argument.
		      (else (math:error 'RAPPLY 'not-handled fxpr) novalue)))
	      fxpr args))

(define (mapply fxpr args)
  (trace-diag
   'mapply
   (lambda (fxpr args)
     (cond
      ((procedure? fxpr) (apply fxpr (map canonicalize args)))
      ((rat:number? fxpr) fxpr)
      ((expl:var? fxpr)
       (let ((var (expl->var fxpr)))
	 (cond ((var:recurrence? var)
		(math:error 'MAPPLY 'wta fxpr)
		novalue)
	       ((and (simple-lambdavar? var)
		     (<= (var:def var) (length args)))
		(canonicalize (list-ref args (+ -1 (var:def var)))))
	       ((procedure? (var:def var))
		(apply (var:def var) (map canonicalize args))
		;; (cond ((null? (var:arglist var))
		;;        (math:print 'APPLY1 (var:sexp var) (map normalize args))
		;;        (apply (var:def var) (map normalize args)))
		;;       (else
		;;        (math:print 'APPLY2 (var:sexp var) (var:arglist var) args)
		;;        (apply (var:def var)
		;; 	      (normalize (capply (var:arglist var) args)))))
		)
	       ((and (var:dffrule var) (symbol? (var:sexp var)))
;;; abstract transcendental function
		(case (length args)
		  ;; ((0) fxpr)
		  ((1) (tcall var (normalize (car args))))
		  ;; ((2) fxpr)
		  (else (bltn:error 'wna var args))))
	       ((var:func var)
;;; derived transcendental function call
		(mapply (var:func var) (capply (var:arglist var) args)))
	       (else (apply deferop fxpr args)))))
      ((clambda? fxpr) (capply fxpr args))
;;; not a builtin; must be a transcendental or unknown function.
;;; core transcendental function has definition and takes only one argument.
      (else (math:error 'MAPPLY 'not-handled fxpr) novalue)))
   fxpr args))

(define (app* fun . args) (mapply fun args))

;;; import recurrence symbol from "init.math"
(define (import-var str)
  (define pr (var-tab-lookup (string->symbol str) var-tab))
  (and pr (var->expl (cdr pr))))

(define %expPQ #f)
(define %tanPQ #f)
(define %tanhPQ #f)
(define (register-inits!)
  (cond ((and %expPQ %tanPQ %tanhPQ))
	(else
	 (set! %expPQ (import-var "%expPQ"))
	 (set! %tanPQ (import-var "%tanPQ"))
	 (set! %tanhPQ (import-var "%tanhPQ"))
	 )))

;;;; call transcendental function
;;; automatically simplifies exp(N*log(ARG)) to
;;; ARG^N for integer N; and tan(N*atan(ARG)) to polynomial of ARG.
;;; Simplifies %W(x*exp(x)) --> x.  %W(x)*exp(%W(x)) doesn't simplify.
;;; TCALL is called from TCALL, VAR:ELIM, ond MAPPLY.
(define (tcall trnv arg)
  (trace-diag
   'tcall
   (lambda (body args)
     (define trnv (expl->var body))
     (define arg (car args))
     (define fia (var:instances trnv))
     (cond
      ((math:assoc arg fia) => cdr)
      (else
       (let ((csym (list (var:sexp trnv) (cano->sexp arg #t))))
	 (define symv (sexp->var csym))
	 (define syml (var->expl symv))
	 (define apr (cons arg syml))
	 (define narg (most-nested-arg arg))
	 (define (updt val) (set-cdr! apr val) val)
	 (define (do-resi resi narg)
	   (cond
	    ((not (number? resi)) syml)
	    ((eqv? 1 resi) (updt narg))
	    (else
	     (case (var:sexp trnv)
	       ((exp) (updt (app* (rref %expPQ (abs resi))
				  (if (negative? resi) (app* $1/$2 1 narg) narg))))
	       ((tan) (updt (app* (rref %tanPQ (abs resi))
				  (app* $1*$2 (sign resi) narg))))
	       ((tanh) (updt (app* (rref %tanhPQ (abs resi))
				   (app* $1*$2 (sign resi) narg))))
	       (else syml)))))
	 (var:set-instances! trnv (cons apr fia))
;;; @1 = syml is the transcentental symbol, @2 is its argument.
	 (register-instance! symv (expr->impl (app* (var:dffrule trnv) syml arg))
;;; changing var:inverse to var:algrule segfaults on exp(%W(x))
			     (and (var:inverse trnv)
				  (clambda? (var:inverse trnv))
				  (app* (var:inverse trnv) syml arg)))
	 (cond
	  ((not narg) syml)
	  (else
;;; The nested argument has variables; reduce using DERIVATIVE().  If
;;; the derivative is 1, then the functions cancel, if the initial
;;; conditions are satisfied {tan(1+atan(x)); should not return x}.
;;; The first IC set (which is the last one in VAR:INSTANCES) of the
;;; containing function has integer CDR.  ARG should equal the CAR
;;; when NARG is equal to the CDR.
	   (register-inits!)
	   (let ((icpr (and (not (null? (var:instances trnv)))
			    (car (last-pair (var:instances trnv))))))
	     (cond
	      ((not icpr) syml)
	      ;; ((not (licit:variable? narg)) syml)
;;; if the narg has variables only in the denominator, then substitute ::@,
;;; solve it, and unsubstitute ::@.
	      ((and (eqv? 0 (cdr icpr))
		    (rat? narg)
		    (number? (cadr narg))
		    (not (number? (caddr narg))))
	       (let ((sub_$ (licit->poleqn (app* $1=$2 (list _$ 0 1) narg))))
		 (define ans (tcall trnv
				    (eliminate
				     (list (poly:most-nested-var narg))
				     (list (licit->poleqn arg) sub_$))))
		 ;; is normalize needed here?
		 (eliminate (list _$) (list (licit->poleqn ans) sub_$))))
;;; otherwise, simplify if the initial-condition is satisfied
;;; AND the derivative of the nested call is an integer.
;;; This does not always simplify triple-nested functions.
	      (else
	       (let ((iceqs (licits->poleqns
			     (list arg (app* $1=$2 (cdr icpr) narg)))))
		 (cond
		  ((math:equal?
		    (car icpr)
		    (normalize
		     (eliminate (list (poly:most-nested-var narg)) iceqs)))
		   (do-resi (normalize
			     (eliminate
			      (list symv)
			      (licits->poleqns (list (derivative syml narg)
						     (app* $1=$2 syml narg)))))
			    narg))
;;; didn't work, return original nested expression.
		  (else syml))))))))))))
   (var->expl trnv) (list arg)))
