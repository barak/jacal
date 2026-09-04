;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1995, 1997, 1998, 1999, 2002, 2004, 2005, 2006, 2007, 2010, 2019, 2020, 2023, 2024, 2026 Aubrey Jaffer.
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
;;; If the types are not disjoint, you WILL lose.

;;; The following types are mutually exclusive:
;;; SEXP, VARIABLE, EXPL, IMPL, EQN, BUNCH
;;; INTEGERs are EXPL
;;; An EXPR is an EXPL or IMPL
;;; A LICIT is an EXPL, IMPL, or EQN.
;;; VARIABLEs can only occur as part of EXPRS and EQNS.
;;; SYMBOLs can only occur in SEXP.
;;; BUNCHES can contain SYMBOLs, LICITs, and BUNCHEs.
;;; An EXPL, IMPL, or EQN, or BUNCH of these can be a lambda expression.

;;; A VAR is a vector which consists of:
;;; 0 var:sexp		- s-expression	;lambda vars have leading "@"
					;shadow vars have trailing ":"
;;; 1 var:pristr			;string from var:sexp
;;; 2 var:def		- poleq		;expr defining equation
;;;		     or	- integer	;simple lambda position
;;;		     or - procedure	;Scheme procedure
;;; 3 var:depends	- list of vars	;vars used in var:def
;;; 4 var:shadows	- var		;shadow copies of this lambda var or #f
;;; 5 var:diffcnt			;order of derivatives
;;; 6 var:pri		- integer	;ordering priority
;;;;		   the rest are for functions and applications
;;; 7 var:func				;function expr
;;; 8 var:arity				;highest @n in non-prim function
;;; 9 var:arglist			;list of argument exprs.
;;; 10 var:inverse
;;; 11 var:algrule
;;; 12 var:dffrule
;;; 13 var:recrule
;;; 14 var:instances			;list of (arg-expr. instance-sexp) of
					;this primitive transcendental function.
;;; 15 var:shade

(define (var:sexp v) (vector-ref v 0))

(define (var:pristr v) (vector-ref v 1))

(define (var:def v) (vector-ref v 2))
(define (var:set-def! v i) (vector-set! v 2 i)) ; v

(define (var:depends v) (vector-ref v 3))
(define (var:set-depends! v i) (vector-set! v 3 i) v)

(define (var:shadows v) (vector-ref v 4))
(define (var:set-shadows! v i) (vector-set! v 4 i))

(define (var:diffcnt var) (vector-ref var 5))

(define (var:pri var) (vector-ref var 6))
(define (var:set-pri! var n) (vector-set! var 6 n))

(define (var:func var) (vector-ref var 7))
(define (var:set-func! var f) (vector-set! var 7 f))

(define (var:arity var) (vector-ref var 8))
(define (var:set-arity! var n) (vector-set! var 8 n))

(define (var:arglist f) (vector-ref f 9))
(define (var:set-arglist! f i) (vector-set! f 9 i))

(define (var:inverse var) (vector-ref var 10))
(define (var:set-inverse! var i) (vector-set! var 10 i))

(define (var:algrule var) (vector-ref var 11))
(define (var:set-algrule! var i) (vector-set! var 11 i))

(define (var:dffrule var) (vector-ref var 12))
(define (var:set-dffrule! var i) (vector-set! var 12 i))

(define (var:recrule var) (vector-ref var 13))
(define (var:set-recrule! var i) (vector-set! var 13 i))

(define (var:instances var) (vector-ref var 14))
(define (var:set-instances! var i) (vector-set! var 14 i))

(define (var:shade var) (vector-ref var 15))
(define (var:set-shade! var shd) (vector-set! var 15 shd))

(define (make-var . args) (if (= 16 (length args))
			      (list->vector args)
			      (math:error 'wna 'make-var)))
(define (poly:var? obj) (and (vector? obj) (= 16 (vector-length obj))))

(define (var:> v1 v2)
  (cond ((eq? v1 v2) #f)
	((not (= (var:pri v1) (var:pri v2)))
	 (> (var:pri v1) (var:pri v2)))
	((not (string=? (var:pristr v1) (var:pristr v2)))
	 (string>? (var:pristr v1) (var:pristr v2)))
	((not (= (var:diffcnt v1) (var:diffcnt v2)))
	 (> (var:diffcnt v1) (var:diffcnt v2)))
	(else (math:error 'var:> 'spurious-match v1 v2) #f)))
(define (var:< v1 v2) (var:> v2 v1))

;; check that single (not bunch) POLY is constructed with monotonic var:>
(define (licits:vet poly)
  (define (pv v cs)
    (cond ((null? cs))
	  ((number? (car cs)) (pv v (cdr cs)))
	  ((var:> v (caar cs))
	   (pv (caar cs) (cdar cs))
	   (pv v (cdr cs)))
	  ((var:< v (caar cs)) (math:warn 'invalid-ordering v (var:pri v) '< (caar cs) (var:pri (caar cs)) poly))
	  (else (math:warn 'priority-mismatch v (var:pri v) '>< (caar cs) (var:pri (caar cs)) poly))))
  (cond ((not poly))
	((number? poly))
	((bunch? poly) (for-each licits:vet poly))
	((eqn? poly) (pv (cadr poly) (cddr poly)))
	(else (pv (car poly) (cdr poly))))
  poly)

(define (var:dump var)
  (sexp:print 'sexp: (var:sexp var))
  (sexp:print 'std: (var:pristr var))
  (math:print 'pri: (var:pri var))
  (math:print 'def: (var:def var))
  (math:print 'depends: (var:depends var))
  (math:print 'func: (var:func var))
  (math:print 'arglist: (var:arglist var))
  (math:print 'application?: (var:application? var))
  (math:print 'constant?: (var:constant? var))
  (math:print 'algrule: (var:algrule var))
  (math:print 'dffrule: (var:dffrule var))
  (math:print 'inverse: (var:inverse var))
  (math:print 'recrule: (var:recrule var))
  (math:print 'shade: (var:shade var))
  (math:print 'shadows: (var:shadows var))
  (sexp:print 'arity: (var:arity var))
  (sexp:print 'diffcnt: (var:diffcnt var))
  (sexp:print 'instances:)
  (for-each (lambda (pr) (math:print (car pr) '--> (cdr pr)))
	    (var:instances var)))

;;; MATH:EQUAL? compares lists like EQUAL?, but EQV? for everything
;;; else, making it safe for expressions with self-referencing vars.
(define (math:equal? a b)
  (cond ((eqv? a b))
	((and (pair? a) (pair? b))
	 (and (math:equal? (car a) (car b))
	      (math:equal? (cdr a) (cdr b))))
	(else #f)))

(define (math:assoc a alst)
  (define (asc alst)
    (cond ((null? alst) #f)
	  ((math:equal? a (caar alst)) (car alst))
	  (else (asc (cdr alst)))))
  (asc alst))

(define (math:member? a lst)
  (cond ((null? lst) #f)
	((math:equal? a (car lst)) #t)
	(else (math:member? a (cdr lst)))))

(define (math:adjoin a olst)
  (cond ((math:member? a olst) olst)
	(else (cons a olst))))

;; (define (math:union lst1 lst2)
;;   (define onion
;;     (lambda (lst1 lst2)
;;       (if (null? lst1)
;; 	  lst2
;; 	  (onion (cdr lst1) (math:adjoin (car lst1) lst2)))))
;;   (cond ((null? lst1) lst2)
;; 	((null? lst2) lst1)
;; 	((null? (cdr lst1)) (math:adjoin (car lst1) lst2))
;; 	((null? (cdr lst2)) (math:adjoin (car lst2) lst1))
;; 	((< (length lst2) (length lst1)) (onion (reverse lst2) lst1))
;; 	(else (onion (reverse lst1) lst2))))

;(define var-tab '())
;(define var-tab-lookup (predicate->asso equal?))
;(define var-tab-define (alist-associator equal?))
;(define var-tab-for-each alist-for-each)

(define var-tab (make-hash-table 43))
(define var-tab-lookup (predicate->hash-asso equal?))
(define var-tab-define (hash-associator equal?))
(define var-tab-undefine (hash-remover equal?))
(define var-tab-for-each hash-for-each)

(define (list-of-vars)
  (define vars '())
  (var-tab-for-each (lambda (k v)
		      (if (not (procedure? (var:def v)))
			  (set! vars (cons v vars))))
		    var-tab)
  vars)

(define (undefvar sexp)
  (var-tab-undefine var-tab sexp))

(define (sexp->var sexp)
  (let ((vcell (var-tab-lookup sexp var-tab)))
    (cond (vcell (cdr vcell))
	  (else
	   (let ((val (sexp->new-var sexp)))
	     (set! var-tab (var-tab-define var-tab sexp val))
	     val)))))
(define (string->var s) (sexp->var (string->symbol s)))

(define (poly:pri poly)
  (cond ((number? poly) 5)
	(($? (car poly))
	 (apply max (map poly:pri (cdr poly))))
	(else
	 (apply max (+ 10 (var:pri (car poly)))
		(map poly:pri (cdr poly))))))

(define (sexp->new-var sxp)
  (define base sxp)
  (define diffs 0)
  (define sexp (if (and (pair? sxp) (eq? 'lambda (car sxp))) (caddr sxp) sxp))
  (define func-arglist (cond ((pair? sxp)
			      (map (lambda (s) (seval s '())) sexp))
			     (else '())))
  (do () ((not (and (pair? base) (eq? 'differential (car base)))))
    (set! base (cadr base))
    (set! diffs (+ 1 diffs)))
  (let* ((stdstr (write-sexp-to-string sxp tps:std)))
    (define slen (string-length stdstr))
;;; predef is positive for lambdavars only
    (define predef (or (and (> slen 1)
			    (char=? #\@ (string-ref stdstr 0))
			    ;; (not (char=? #\^ (string-ref stdstr 1)))
			    (or (string->number (substring stdstr 1 slen))
				;; handle trailing ":" in shadow-var
				(string->number (substring stdstr 1 (- slen 1)))))
		       0))
    (define var
      (make-var
       sexp					      ; var:sexp
       stdstr					      ; var:pristr
       (and (positive? predef) predef)		      ; var:def
       '()					      ; var:depends
       (and (positive? predef) (make-list predef #f)) ; var:shadows
       diffs					      ; var:diffcnt
       ;; (+ (if (positive? predef) (+ 4000 predef) 3000) diffs)
       (+ predef diffs
	  (if (null? func-arglist)
	      0
	      (apply max (map poly:pri func-arglist))))	; var:pri    
       (if (null? func-arglist) #f (car func-arglist))	; var:func
       predef						; var:arity
       (if (null? func-arglist) '() (cdr func-arglist)) ; var:arglist
       #f						; var:inverse
       #f						; var:algrule
       #f						; var:dffrule
       #f						; var:recrule
       '()				; var:instances
       #f				; var:shade
       ))
;;;; fixup var:arity
    (cond ((and (zero? predef) (not (null? func-arglist)))
	   (var:set-arity! var (licit:deep-arity (cdr func-arglist)))))
    (cond ((lambdavardep? var)
	   (var:set-shadows! var (make-list (var:max-lambda-position var) #f))))
    var))

;;; called only from SEVAL (2 places).
(define (var:reset! var impl)
  (define stdstr (var:pristr var))
  (define slen (string-length stdstr))
  (for-each (lambda (inst)
	      (cond ((expl:var? (cdr inst))
		     (undefvar (var:sexp (cadr inst))))))
	    (var:instances var))
  (var:set-instances! var '())
  (var:set-dffrule! var impl)
  (var:set-pri! var (+ 2000 (var:diffcnt var)))
  (var:set-func! var (var->expl var))
  (var:set-arglist! var (list (list $1 0 1)))
  (var:set-depends! var (list $1))
  )

;;; called only from SEVAL (2 places).
(define (var:definverse! var form)
  (define deps (remove var (licit:depends form)))
  (define diform (total-differential form))
  (define impl (poly:coeff (eliminate (remove-if simple-lambdavar? deps)
				      (list form diform))
			   $ 0))
  (define stdstr (var:pristr var))
  (define slen (string-length stdstr))
  (for-each (lambda (inst)
	      (cond ((expl:var? (cdr inst))
		     (undefvar (var:sexp (cadr inst))))))
	    (var:instances var))
  (var:set-depends! var (list $1))
  (var:set-instances! var '())
  (var:set-dffrule! var impl)
;;; uncommenting this next line foils diff of %W tests.
  ;; (var:set-inverse! var form)
  (var:set-pri! var (+ 3000 (var:diffcnt var)))
  (var:set-func! var (var->expl var))
  (var:set-arglist! var (list (list $1 0 1))))

(define (var:constant? var)
  (cond (($? var) #t)
	((simple-lambdavar? var) #f)
	((and (var:func var) (null? (var:depends var))))
	((var:differential? var) #f)
	(else #f)))
(define (var:variable? var) (not (var:constant? var)))

(define (trifle? x)
  (or (null? x)
      (novalue? x)
      (boolean? x)
      (symbol? x)
      (string? x)
      (number? x)
      (rat:number? x)))

;; find non-constant vars
(define (poly:variables poly)
  (remove-if var:constant? (poly:vars poly)))

(define (licit:variable? licit) (poly:find-var-if? licit var:variable?))
(define (licit:constant? licit) (not (licit:variable? licit)))

;;; used by DEFEXT, EXTIZE, and REGISTER-INSTANCE!
(define (licit:depends licit)
  (let ((deps '()))
    (define (ld licit)
      (licit:for-each-var
       (lambda (v0)
	 (define v (var:nodiffs v0))
	 (cond (($? v))
	       ((memv v deps))
	       ((var:constant? v))
	       ((and (var:dffrule v)
		     (var:inverse v)
		     (not (var:application? v)))
		(math:error 'licit:depends v))
	       (else (set! deps (cons v deps))
		     (for-each (lambda (vv) (set! deps (adjoin vv deps)))
			       (var:depends v))
		     (ld (var:arglist v))
		     )))
       licit))
    (ld licit)
    (sort deps var:<)))

;;; all ARGS are exprs
(define (deferop . args)
;;; check if transcendental instance already exists before creating new var.
  ;; (define var (expl->var (car args)))
  ;; (define fia (var:instances var))
  (cond ;; ((and (null? (cddr args)) (math:assoc (cadr args) fia)) => cdr)
   (else
    (let ((form (map (lambda (arg) (cano->sexp arg #t)) args)))
      (define var (sexp->var form))
      (var:set-depends! var (if (pair? args) (licit:depends args) '()))
      (var->expl var)))))

(define (strimbol? v)
  (and (symbol? v) (eqv? #\" (string-ref (symbol->string v) 0))))
(define (var->string v)
  (define sexp (var:sexp v))
  (cond ((strimbol? sexp)
	 (substring (symbol->string sexp)
		    1 (+ -1 (string-length (symbol->string sexp)))))
	(else (math:error 'expected-string sexp))))

;;; only used in MAKE-SHADOW
(define (copy-vector v)
  (define iv (make-vector (vector-length v)))
  (do ((i (+ -1 (vector-length iv)) (+ -1 i)))
      ((negative? i) iv)
    (vector-set! iv i (vector-ref v i))))

;;; The shadowed variables are kept in a list in the var:shadow slot
;;; in the var.  This list has the bumped and shadowed transformations
;;; of the var for each argcount less than or equal
;;; (var:max-lambda-position var).

;;; For simple lambda variables (@1, @2, ...) the last shadowed
;;; variable is identical to the original except that its shadow slot
;;; is #f.

;;; used only by CAPPLY
(define (var:shadow v argcount)
  (define shadows
    (cond ((null? (var:shadows v)) (list #f))
	  (else (var:shadows v))))
  (let ((shadpair (memshad argcount shadows)))
    (if (not (car shadpair))
	(set-car! shadpair
		  (cond ((< argcount (var:min-lambda-position v))
			 (var:lambda-bump v (- argcount)))
			;; ((number? (var:def v)) (make-shadow v argcount))
			(else
			 (let ((vshad (make-shadow v argcount)))
			   (var:set-shade! v vshad)
			   vshad)))))
    (car shadpair)))
;;; used only by VAR:SHADOW
(define (memshad argcount shadlist)
  (cond ((= 1 argcount) shadlist)
	((null? (cdr shadlist)) shadlist)
	(else (memshad (+ -1 argcount) (cdr shadlist)))))
;;; used only by VAR:SHADOW
(define (make-shadow v argcount)
  (let ((nv (copy-vector v)))
    (var:set-shadows! nv #f)
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
		       (if (> i argcount)
			   (var:sexp (lambda-var (- i argcount) 0))
			   (string->symbol (string-append st ":"))))
		     s))
	       (var:sexp v))))
    (var:set-pri! nv (+ 1 (var:pri v)))	; avoid spurious var matches.
    (var:set-depends!
     nv (map (lambda (var)
	       (cond ((simple-lambdavar? var)
		      (var:shadow var argcount))
		     ((var:shade var))
		     (else var)))
	     (var:depends v)))
    (cond ((number? (var:def v))
	   (var:set-def! nv (licit:do-vars
			     (lambda (var)
			       (if (lambdavardep? var)
				   (if (eq? var v) nv
				       (var:shadow var argcount))
				   var))
			     (var:def v)))))
    ;; (math:print 'DEPENDS (var:depends nv))
    (cond ((pair? (var:arglist v))
	   (let ((argl (licit:do-vars
			(lambda (var)
			  (if (lambdavardep? var)
			      (var:shadow var argcount)
			      var))
			(var:arglist v))))
	     ;; (var:set-func! nv (car argl))
	     (var:set-arglist! nv argl)
	     ;; (var:set-arity! nv (licit:deep-arity (cons (var:func nv) (var:arglist nv))))
	     ;; (var:set-algrule! nv #f)
	     ;; (var:set-dffrule! nv #f)
	     (var:set-shade! nv #f)
	     )))
    nv))

;; radical-defs is the list of radical extension defining poleqns
(define (make-rad-var radicand exponent-reciprocal)
  (let ((e (univ:monomial -1 exponent-reciprocal $)))
    (set-car! (cdr e) radicand)
    (let ((v (defext (sexp->var (list '^ (cano->sexp radicand #t)
				      (list '/ 1 exponent-reciprocal)))
	       e)))
      (var:set-arglist! v (list radicand (make-rat 1 exponent-reciprocal)))
      (var:set-func! v _^)
      (set! radical-defs (cons (var:algrule v) radical-defs))
      v)))

;; used for radicals
(define (defext var impl)
  (define deps (remove var (licit:depends impl)))
  (define pro (poly:promote var impl))
  (var:set-depends! var deps)
  (var:set-pri! var (if (null? deps)
			1000		;must be a constant.
			(+ 1 (apply max (map var:pri deps)))))
  (var:set-algrule! var (vsubst var $ (univ:demote pro)))
  ;; (var:set-def! var (var:algrule var))
  ;; (cond ((or (simple-lambdavar? var) (lambdavardep? var))
  ;; 	 (var:set-shadows! var (make-list (var:max-lambda-position var) #f))))
  var)

;;; called from TCALL
(define (register-instance! var impl inv)
  (define deps (remove var (licit:depends impl)))
;;; don't change var:pri; it spoils existing licits;
  (var:set-inverse! var (and inv (poly:coeff inv $ 0)))
  ;; (var:set-algrule! var (and inv (poly:coeff inv $ 0)))
  ;; (var:set-dffrule! var (if (null? deps) #f (poly:coeff impl $ 0)))
  (var:set-dffrule! var (poly:coeff impl $ 0))
  (var:set-depends! var deps)
  (cond ((lambdavardep? var)
	 (var:set-shadows! var (make-list (var:max-lambda-position var) #f))))
  ;; this must be here for log() to get the same treatment as exp().
  (var:set-depends! var (remove var (licit:depends impl)))
;;   (if (and (poleqn:differential? impl) (not (poly:find-var? impl var)))
;; ;;; this diffeq is an integration on-ramp
;;       (set! on-ramps (cons impl on-ramps)))
  )

(define (register-recurrence! var expl)
  (define deps (remove-if-not simple-lambdavar? (licit:depends expl)))
  (var:set-depends! var deps)
  (for-each (lambda (inst)
	      (cond ((expl:var? (cdr inst))
		     (undefvar (var:sexp (cadr inst))))))
	    (var:instances var))
  (var:set-instances! var '()))

;;; turn impl into algebraic extension or labeled value.
(define (extize var impl)
  (cond ((bunch? impl) (eval:error 'cannot-suchthat-a-vector impl))
	(var
	 (let ((deps (remove var (licit:depends impl))))
	   (define pro (poly:promote var impl))
	   ;; (math:print 'EXTIZE-DEPENDS deps)
	   (var:set-depends! var deps)
	   (var:set-pri! var (if (null? deps)
				 1000	;must be a constant.
				 (+ 1 (apply max (map var:pri deps)))))
	   ;; (var:set-def! var impl)
	   ;; (var:set-def! var (poly:coeff impl $ 0))
	   (var:set-algrule! var (vsubst var $ (univ:demote pro)))
	   ;; (var:set-def! var (var:algrule! var))
	   (set! var-news (cons var var-news))
	   (var->expl var)))
	((eqn? impl) impl)
	((expl? impl) impl)
	((rat? impl) impl)
	(else
	 (set! newextstr (chap:next-string newextstr))
	 ;; (math:print 'EXTIZE-DEFEXT impl)
	 (let ((v (defext (string->var newextstr) impl)))
	   (set! var-news (cons v var-news))
	   (var->expl v)))))

(define (var:differential? v)
  (positive? (var:diffcnt v)))
(define (var:differential v)
  (sexp->var (list 'differential (var:sexp v))))
(define (poleqn:differential? expl)
  (poly:find-var-if? expl var:differential?))
(define (poly:radicalvar? poly)
  (poly:find-var-if? poly radicalvar?))

;;; returns first SIMPLE-SHADOWED-LAMBDAVAR? found.
(define (licit:shadowed? poly)
  (let lp ((deps (licit:depends poly)))
    (cond ((null? deps) #f)
	  ((simple-shadowed-lambdavar? (car deps)) (car deps))
	  (else (lp (cdr deps))))))

(define (var:recurrence? var)
  (define sxp (var:sexp var))
  (or (var:recrule var)
      (and (pair? sxp) (eq? 'rref (car sxp)))))

;; If poly depends on a transcendental function, return its argument.
;; used only by TCALL
(define (most-nested-arg poly)
  (define vars '())
  (poly:for-each-var
   (lambda (v)
     (and (var:dffrule v)
	  (var:application? v)
	  (set! vars (adjoin v vars))))
   poly)
  (set! vars (sort vars var:>))
  (cond ((null? vars) #f)
	(else (car (var:arglist (car vars))))))

(define (poly:most-nested-var poly)
  (define deps (licit:depends poly))
  (cond ((null? deps) #f)
	(else (car (last-pair deps)))))

;;; used only in DIFFARGS, which is used only in CAPPLY
(define (var:undiff v)
  (sexp->var (cadr (var:sexp v))))

(define (var:nodiffs v)
  (cond ((var:differential? v) (var:nodiffs (var:undiff v)))
	(else v)))

;;; used only by VAR:ELIM and DESCRIBE-VAR
(define (radicalvar? v)
  (let ((ve (var:sexp v)))
    (and (pair? ve) (eq? '^ (car ve)))))

;;; used by MAKE-SHADOW VAR:MIN-LAMBDA-POSITION VAR:MAX-LAMBDA-POSITION VAR:LAMBDA-BUMP
(define (simple-lambdavar? v)
  (and (number? (var:def v)) (positive? (var:def v))))

;;; used by CAPPLY, LICIT:SHADOWED?
(define (simple-shadowed-lambdavar? v)
  (and (simple-lambdavar? v) (not (var:shadows v))))

;;; used by CAPPLY DEFEXT REGISTER-INSTANCE! CLAMBDA?
(define (lambdavardep? var)
  (positive? (var:arity var)))

;;; used by MAKE-SHADOW PARTIAL JACOBI-MATRIX
(define (lambda-var i diff-depth)
  (if (zero? diff-depth)
      (let ((v (sexp->var
		(string->symbol
		 (string-append "@" (number->string i))))))
	(var:set-def! v i)
	(or (var:shadows v)		;so simple-shadowed-lambdavar? works
	    (var:set-shadows! v (make-list i #f)))
	v)
      (var:differential (lambda-var i (+ -1 diff-depth)))))

;;; the order doesn't matter.
(define (extrule e)
  (or (var:dffrule e) (var:algrule e)))

;;; IMPL is a data type consisting of a poly with major variable
;;; $.  The value of the IMPL is negative of the poly solved for $.
;;; Using this representation, if poly is square-free and has no
;;; content (gcd (coefficients) = 1), we can express any
;;; algebraic function or number uniquely, even those with no standard
;;; representation (order > 4 roots).

(define (expr? p)
  (or (number? p)
      (poly:poly? p)))
(define (impl? p) (and (pair? p)
		       (not (eqn? p))
		       ($? (car p))
		       (not (null? (cdr p)))))
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
(define (make-rat num denom) (list $ (poly:negate num) denom))
(define (rat:num p) (poly:negate (univ:demote (cadr p))))
(define (rat:denom p) (univ:demote (caddr p)))
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
(define (poly->eqn p)
  (cond ((and (number? p) (not (zero? p)))
	 (math:error 'singular-reduction "0 != " p)
	 novalue)
	(else (cons $= p))))
(define (polys->eqns p) (if (bunch? p) (map polys->eqns p) (poly->eqn p)))
(define (var->expl v) (list v 0 1))
(define (expl->impl p) (make-rat p 1))
(define (var->impl v) (make-rat (var->expl v) 1))
(define (expr->impl expr)
  (cond ((and (pair? expr) (eqv? (car expr) $)) expr)
	(else (list $ expr -1))))

;;; Two paradigms for doing algebra on equations and expressions:
;;; Polynomials as expressions and Polynomials as equations.
;;; Polynomials are used as expressions in GCD.
;;; Polynomials are used as equations in ELIMINATE.
;;;	licit->	polxpr	poleqn
;;;	eqn	expl	expl
;;;	expl	expl	impl
;;;	impl  expl($=0) impl
;;; After the operation is done, we need to convert back.  For
;;; Polynomials as expressions, the result is already expl.  For
;;; polynomials as equations:
;;;	poleqn->licit
;;;	expl	eqn
;;;	impl	expr
(define (licit->polxpr p)
  (cond ((eqn? p) (eqn->poly p))
	((impl? p) (cadr p))
	((expl? p) p)
	(else (math:error 'cannot-be-coerced-to-poly-eqn:- p))))
(define (licit->poleqn p)
  (cond ((eqn? p) (eqn->poly p))
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
  (cond ((eqn? p) (eqn->poly p))
	((expl? p) p)
	((and (impl? p) (poly:/? (rat:num p) (rat:denom p))))
	(else (math:error 'cannot-be-coerced-to-expl:- p))))
(define (licit->impl p)
  (cond ((eqn? p) (math:error 'value-expected-equation-found:- p))
	((impl? p) p)
	((expl? p) (expl->impl p))
	(else (math:error 'cannot-be-coerced-to-implicit:- p))))
(define (licits->impls p)
  (if (bunch? p) (map licits->impls p) (licit->impl p)))
(define (expl:var? p)
  (and (pair? p)
       (expl? p)
       (equal? (cdr p) '(0 1))))
(define (expl->var p)
  (cond ((expl:var? p)
	 (car p))
	(else (math:error 'expl->var 'not-a-simple-variable:- p))))
;;; used only in "builtin.scm"
(define (variables p)
  (cond ((expl:var? p)
	 (list (expl->var p)))
	((bunch? p) (map expl->var p))
	(else (math:error 'variables 'not-a-simple-variable:- p))))
(define (plicit->integer p)
  (cond ((integer? p) p)
	((not (rat:number? p)) (math:error 'not-an-integer- p))
	((rat:unit-denom? p) (* (rat:denom p) (rat:num p) -1))
	(else (math:error 'not-an-integer- p))))
(define (unit? x) (memv x '(1 -1)))
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
	((vector? e)
	 (and (not (poly:var? e))
	      (not (monomial? e))))
	(else #f)))

;;; A useful companion for ZERO?
(define (one? n) (eqv? 1 n))

;;; A MONOMIAL is a (Scheme) vector of length 3 which consists of:
;;; 0: integer coefficient
;;; 1: list of vars (sorted)
;;; 2: list of integer powers corresponding to vars
(define (make-monomial coeff vars pows)
  (cond ((not (number? coeff))
	 (math:error 'wta 'make-monomial coeff))
	((and (list? vars)
	      (list? pows)
	      (eqv? (length vars) (length pows)))
	 (vector coeff vars pows))
	(else (math:error 'wta 'make-monomial))))
(define (monomial? obj) (and (vector? obj) (= 3 (vector-length obj))))
(define (mono:coeff obj) (vector-ref obj 0))
(define (mono:vars obj) (vector-ref obj 1))
(define (mono:pows obj) (vector-ref obj 2))

;;; This implements what https://en.wikipedia.org/wiki/Monomial_order calls
;;; Graded reverse lexicographic order "grevlex"
(define (mono:> obj1 obj2)
  (define sgn (- (apply + (mono:pows obj1))
		 (apply + (mono:pows obj2))))
  (or (positive? sgn)
      (and (zero? sgn)
	   (not (null? (mono:vars obj1)))
	   (not (null? (mono:vars obj2)))
	   (revlex> (mono:vars obj1) (mono:pows obj1)
		    (mono:vars obj2) (mono:pows obj2)))))

(define (monomial->poly mon)
	(let lp ((vars (mono:vars mon))
		 (pows (mono:pows mon))
		 (prod (mono:coeff mon)))
	  (cond ((null? vars) prod)
		(else
		 (lp (cdr vars) (cdr pows)
		     (app* $1*$2 prod
			   (^ (list (car vars) 0 1) (car pows))))))))

(define (revlex> vars1 pows1 vars2 pows2)
  (cond ((var:> (car vars1) (car vars2)) #t)
	((var:> (car vars2) (car vars1)) #f)
	((> (car pows1) (car pows2)) #t)
	((> (car pows2) (car pows1)) #f)
	((null? (cdr vars1)) #f)
	(else (revlex> (cdr vars1) (cdr pows1) (cdr vars2) (cdr pows2)))))
