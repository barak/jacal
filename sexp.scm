;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1996, 1997 Aubrey Jaffer.
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


;;; Label values have been split into SEXP and MATH (canonical) types.
;;; Only one type is assigned initially, and the other is cached if it
;;; gets computed.

;;; Per discussions with RMS, assignments will remain; syntactically
;;; distinguished from variables.  Symbol assignments are expanded
;;; when the symbol is found in input expressions.

;;; Part extraction commands operate on SEXP type and are expanded in
;;; the same pass that expands symbol assignments.

;;; Part extraction commands can operate as locatives, so that an
;;; operation such as factor can be applied to a part of an
;;; expression, with the other parts copied into the resulting
;;; expression.

(require 'chapter-order)
(require 'with-file)
(require 'common-list-functions)
(require 'fluid-let)

;;; our local environments
;(define heqput! (alist-associator eq?))
;(define heqrem! (alist-remover eq?))
;(define hassq (predicate->asso eq?))
;(define (list-of-procedure-defsyms)
;  (define proc-defs '())
;  (alist-for-each (lambda (k v) (if (procedure? (var:def v))
;				    (set! proc-defs (cons k proc-defs))))
;		  var-tab)
;  proc-defs)

(define heqput! (hash-associator eq?))
(define heqrem! (hash-remover eq?))
(define hassq (predicate->hash-asso eq?))
(define (list-of-procedure-defsyms)
  (define proc-defs '())
  (hash-for-each (lambda (k v) (if (procedure? (var:def v))
				   (set! proc-defs (cons k proc-defs))))
		 var-tab)
  proc-defs)

(define (defsym sym value)
  (if (sexp? value) (defsym-sexp sym value) (defsym-cano sym value)))
(define (defsym-sexp sym value)
  (set! *symdefs* (heqput! *symdefs* sym (cons value #f))) value)
(define (defsym-cano sym value)
  (set! *symdefs* (heqput! *symdefs* sym (cons #f value))) value)
(define (undefsym sym)
  (set! *symdefs* (heqrem! *symdefs* sym))
  (var->expl (sexp->var sym)))

(define (pairs->alist protos)
  (do ((pr protos (cddr pr))
       (alst '() (cons (cons (car pr) (cadr pr)) alst)))
      ((or (null? pr) (null? (cdr pr)))
       (cond ((null? pr) alst)
	     (else (math:error 'defbltn 'odd-length-proto-list protos))))))

;; (define (defbltn syms . protos)
;;   (define alst (pairs->alist protos))
;;   (for-each (lambda (sym) (var:set-def! (sexp->var sym) alst))
;; 	    (if (list? syms) syms (list syms)))
;;   syms)

(define (defbltn syms val)
  (for-each (lambda (sym) (var:set-def! (sexp->var sym) val))
	    (if (list? syms) syms (list syms)))
  syms)

;;; hdns here is a list of lexically bound symbols as in lambda or suchthat.
;;; so it is really a list of things not to look up.
(define (symdef-lookup sym hdns)
  (cond ((null? hdns)
	 (let ((p (hassq sym *symdefs*)))
	   (and p (cdr p))))
	((eq? sym (car hdns)) #f)
	((symbol? (car hdns)) (symdef-lookup sym (cdr hdns)))
	((memq sym (car hdns)) #f)
	(else (symdef-lookup sym (cdr hdns)))))

(define (symdef-lookup-cano sym hdns)
  (let ((vals (symdef-lookup sym hdns)))
    (cond ((not vals) (var->expl (sexp->var sym)))
	  ((not (pair? vals)) vals)
	  ((cdr vals) (cdr vals))
	  (else
	   (set-cdr! vals (sexp->math (car vals)))
	   (cdr vals)))))

(define (symdef-lookup-sexp sym hdns)
  (let ((vals (symdef-lookup sym hdns)))
    (cond ((not vals) sym)
	  ((not (pair? vals)) vals)
	  ((car vals) (car vals))
	  (else
	   (set-car! vals (cano->sexp (cdr vals) horner))
	   (car vals)))))

(define (math:greet)
  (tran:display 'type)
  (write-sexp '(qed) *input-grammar*)
  (tran:display 'to-return-to-)
  (display 'scheme)
  (tran:display 'type-)
  (write-sexp '(help) *input-grammar*)
  (tran:display 'for-help.))

;;;now for the read-eval-print stuff
(define var-news '())
(define (math . batches)
  (set-handlers!)
  (for-each (lambda (file)
	      (batch (if (symbol? file) (symbol->string file) file)))
	    batches)
  (math:greet)
  (batch1)
  (cleanup-handlers!)
  'scheme)

(define (batch file)
  (fluid-let ((page-height page-height)
	      (page-width page-width)
	      (*input-grammar* *input-grammar*)
	      (*output-grammar* *output-grammar*)
	      (*echo-grammar* *echo-grammar*))
    (with-input-from-file file batch1)))

(define (batch1)
  (do ((math:exit-saved math:exit)
       (var-news-saved var-news)
       (math:prompt #f))
      ((call-with-current-continuation
	(lambda (math:exit-cnt)
	  (define obj #f)
	  (define cip (current-input-port))
	  (set! math:exit math:exit-cnt)
	  (newline)			;find unused var
	  (do () ((not (or (var-tab-lookup newlabelsym var-tab)
			   (hassq newlabelsym *symdefs*))))
	    (set! newlabelstr (chap:next-string newlabelstr))
	    (set! newlabelsym  (string->symbol newlabelstr)))
	  (set! math:prompt (string-append newlabelstr " : "))
	  (let loop ()
	    (define echoing (not (eq? (get-grammar 'null) *echo-grammar*)))
	    (set! var-news '())
	    (cond (echoing)
		  ((output-port? cip)
		   (let ((cip cip))
		     (display math:prompt cip)
		     (force-output cip)
		     (tok:bump-column (string-length math:prompt) cip)))
		  (else (display math:prompt)
			(force-output)
			(tok:bump-column (string-length math:prompt) cip)))
	    (set! obj (read-sexp *input-grammar*))
	    (tok:bump-column 0 cip)
	    (cond ((not obj) (loop))
		  ((eof-object? obj) (math:exit #t))
		  ((and (symbol? obj) (symdef-lookup obj '()))
		   (write-sexp (list 'define obj (symdef-lookup-sexp obj '()))
			       *output-grammar*)
		   (newline)
		   (loop))
		  (else
		   (set! linum 0)
		   (cond (echoing
			  (write-sexp obj *echo-grammar*)
			  (newline)))
		   (if (and (pair? obj) (eq? 'define (car obj)))
		       (let* ((var (cadr obj))
			      (val (define-label var (caddr obj) '())))
			 (out-new-vars var-news)
			 (newline)
			 (cond ((novalue? val)
				(define-label var var '())
				(eval-error 'no-value-to-set (cadr obj)))
			       ((eq? 'null (grammar-name *output-grammar*))
				(set! % val))
			       (else
				(set! % val)
				(write-sexp (list 'define var
						  (symdef-lookup-sexp var '()))
					    *output-grammar*)
				(newline))))
		       (let* ((var newlabelsym)
			      (val (define-label var obj '())))
			 (out-new-vars var-news)
			 (newline)
			 (cond ((novalue? val)
				(define-label var var '())
				(loop))
			       ((eq? 'null (grammar-name *output-grammar*))
				(set! % val))
			       (else
				(set! % val)
				(write-sexp (list 'define var
						  (symdef-lookup-sexp var '()))
					    *output-grammar*)
				(newline))))))))
	  #f))
       (set! math:exit math:exit-saved)
       (set! var-news var-news-saved))))

(define (out-new-vars var-news)
  (if (not (eq? 'null (grammar-name *output-grammar*)))
      (for-each (lambda (x)
		  (newline)
		  (write-sexp (list 'define
				    (var:sexp x)
				    (cano->sexp (vsubst $ x (extrule x))
						horner))
			      *output-grammar*))
		var-news)))

;;; $=fc($1) --> $=fc^^-1($1)
(define (fcinverse fc)
  (extize (normalize (swapvars $1 $ (licit->impl fc)))))

;;; fc(fc(...fc($1)))
(define (fcexpt fc pow)
  (if (negative? pow)
      (fcexpt (fcinverse fc) (- pow))
    (ipow-by-squaring fc pow cidentity app*)))

(define (rapply ob . arglist)
  (cond ((null? arglist) ob)
	((bunch? ob)
	 (apply rapply
		(list-ref ob (+ -1 (plicit->integer (car arglist))))
		(cdr arglist)))
	((expl? ob) (apply deferop _rapply ob arglist))
	(else (eval-error 'rapply 'wta ob))))

(define (sapply fun args)
  (cond ((procedure? fun) (apply fun args))
	((clambda? fun)
	 (capply fun args))
	((rat:number? fun) fun)		;(eval-error 'apply 'wta fun)
	(else (apply deferop fun args))))

(define (app* fun . args) (sapply fun args))

(define (seval-norm f hdns)
  (let ((ans (seval f hdns)))
    (if (sexp? ans) ans (normalize ans))))

(define (define-label label sexp hdns)
  (cond ((symbol? label)
	 (if (eq? label sexp)
	     (undefsym label)
	     (defsym label
	       (seval-norm sexp (cons label hdns)))))
	((not (pair? label))
	 (jacal:found-bug 'define-label label))
	((eqv? (car label) 'rapply)
	 (defsym-cano (cadr label)
	   (rlambda (cddr label)
		    (seval-norm sexp (cons (cdr label) hdns)))))
	(else				;must be capply
	 (defsym-cano (car label)
	   (clambda (variables (cdr label))
		    (seval-norm sexp (cons label hdns)))))))

(define (seval f hdns)
  (cond ((number? f)
	 (if (inexact? f) (eval-error 'inexact-number-to-eval:-))
	 (cond ((integer? f) f)
	       ((rational? f) (make-rat (numerator f) (denominator f)))))
	((vector? f) (map (lambda (x) (seval x hdns)) (vector->list f)))
	((symbol? f) (symdef-lookup-cano f hdns))
	((boolean? f) f)
	((null? f) f)
	((not (pair? f)) (eval-error 'eval 'wta f))
	((eq? 'lambda (car f))
	 (let ((vars (variables
		      (cond ((symbol? (cadr f)) (list (cadr f)))
			    ((vector? (cadr f)) (vector->list (cadr f)))
			    ((pair? (cadr f)) (cadr f))
			    (else (eval-error 'lambda 'bad-arglist f))))))
	   (clambda vars (seval (caddr f) (cons vars hdns)))))
	((eqv? (car f) 'suchthat)
	 (suchthat (sexp->var (cadr f))
		   (seval (caddr f) (cons (cadr f) hdns))))
	((eqv? (car f) 'define)
	 (eval-error 'nested-defines? f))
	(else
	 (let ((ff (seval (car f) hdns)))
	   (sapply (or (and (pair? ff)
			    (expl? ff)
			    (equal? (cdr ff) '(0 1))
			    (procedure? (var:def (car ff)))
			    (var:def (car ff)))
		       ff)
		   (map (lambda (x) (seval x hdns)) (cdr f)))))))
(define (sexp->math f) (seval f '()))

(define (bunch->sexp p horner)
;;; These routines convert LICITs or parts of LICITs to S-EXPRESSIONs
  (define (cmprs:+ res)
    (cond ((null? (cdr res)) (car res))
	  ((and (pair? (cadr res)) (eq? 'negate (caadr res)))
	   (cmprs:+ (cons (list '- (car res) (cadadr res)) (cddr res))))
	  ((and (pair? (car res)) (eq? '+ (caar res)))
	   (if (null? (cddr res)) (nconc (car res) (cdr res))
	       (cmprs:+ (cons (nconc (car res) (list (cadr res))) (cddr res)))))
	  ((null? (cddr res)) (cons '+ res))
	  (else (cmprs:+ (cons (list '+ (car res) (cadr res)) (cddr res))))))

  (define (cmprs:* mu mex)
    (cond ((pair? mu)
	   (cond ((eq? '* (car mu)) (nconc mu (list mex)))
		 ((eq? 'negate (car mu))
		  (list 'negate (cmprs:* (cadr mu) mex)))
		 (else (list '* mu mex))))
	  ((and (number? mu) (negative? mu))
	   (if (eqv? -1 mu)
	       (list 'negate mex)
	       (list 'negate (list '* (- mu) mex))))
	  (else (if (eqv? 1 mu) mex (list '* mu mex)))))

  (define (cmprs:^ var exp)
    (cond ((one? exp) var)
	  ((and (pair? var)
		(eq? '^ (car var)))
	   (list '^
		 (cadr var)
		 (if (and (pair? (caddr var))
			  (eq? '/ (caaddr var))
			  (one? (cadr (caddr var))))
		     (list '/ exp (caddr (caddr var)))
		     (cmprs:* exp (caddr var)))))
	  (else (list '^ var exp))))

  ;;POLY->SEXP converts a polynomial to SEXPRESSION.
  (define (poly->sexp p)
    (cond ((number? p) p)
	  (horner (coes->horner-sexp (var:sexp (car p)) 0 (cdr p)))
	  (else (cmprs:+ (coes->sexp (var:sexp (car p)) 0 (cdr p))))))
  (define (coes->horner-sexp var exp colist)
    (cond ((eqv? 0 (car colist)) (coes->horner-sexp var (+ 1 exp) (cdr colist)))
	  ((null? (cdr colist))
	   (if (zero? exp) (poly->sexp (car colist))
	       (cmprs:* (poly->sexp (car colist)) (cmprs:^ var exp))))
	  ((zero? exp)
	   (cmprs:+ (list (poly->sexp (car colist))
			  (coes->horner-sexp var 1 (cdr colist)))))
	  (else
	   (cmprs:*
	    (cmprs:+ (list (poly->sexp (car colist))
			   (coes->horner-sexp var 1 (cdr colist))))
	    (cmprs:^ var exp)))))
  (define (coes->sexp var exp colist)
    (cond ((null? colist) colist)
	  ((eqv? 0 (car colist)) (coes->sexp var (+ 1 exp) (cdr colist)))
	  ((zero? exp) (cons (poly->sexp (car colist))
			     (coes->sexp var (+ 1 exp) (cdr colist))))
	  ((eqv? 1 (car colist))
	   (cons (cmprs:^ var exp) (coes->sexp var (+ 1 exp) (cdr colist))))
	  (else (cons (cmprs:* (poly->sexp (car colist)) (cmprs:^ var exp))
		      (coes->sexp var (+ 1 exp) (cdr colist))))))
  ;;RAT->SEXP converts a rational polynomial to SEXPRESSION.
  (define (rat->sexp n d)
    (if (unit? d)
	(poly->sexp (poly:* n d))
	(list 'over (poly->sexp n) (poly->sexp d))))

  (define (impl:radical? p) (one? (length (or (memv 0 (cddr p)) '()))))
  ;;IMPOLY->SEXP converts an implicit polynomial to SEXPRESSION.
  (define (impoly->sexp p)
    (if (impl:radical? p)
	(list '=
	      (if (null? (cdddr p))
		  (var:sexp (car p))
		  ;;I cant exercise this clause:
		  (list '^ (var:sexp (car p)) (length (cddr p))))
	      (rat->sexp (cadr p) (univ:lc p)))
	(list '= 0 (poly->sexp p))))

  ;;IRIMPL->SEXP converts an irreducible implicit expression to SEXPRESSION.
  (define (irimpl->sexp p)
    (let ((dgr (poly:degree p $)))
      (cond ((zero? dgr) (math:warn 'not-canonical p) p)
	    ((one? dgr) (rat->sexp (rat:num p) (rat:denom p)))
	    (else (list 'suchthat (var:sexp (car p)) (impoly->sexp p))))))

  (define (ibunch->sexp p)
    (cond ((bunch? p) (list->vector (map ibunch->sexp p))) ;inefficient
	  ((symbol? p) p)
	  ((expl? p) (poly->sexp p))
	  ((impl? p)
	   (let ((dgr (poly:degree p $)))
	     (cond ((zero? dgr) (math:warn 'not-canonical p) p)
		   ((one? dgr) (rat->sexp (rat:num p) (rat:denom p)))
		   (else
		    (let ((fcts (map irimpl->sexp (univ:split-all p))))
		      (if (null? (cdr fcts)) (car fcts)
			  (cons 'or fcts)))))))
	  ((eqn? p) (list '= 0 (poly->sexp (eqn->poly p))))
	  (else (eval-error 'unknown 'type p))))
  (ibunch->sexp p))

(define (get-lambda-list poly)
  (do ((j (licits:max-lambda-position poly) (+ -1 j))
       (ll '()
	   (cons (string->symbol (string-append "@" (number->string j))) ll)))
      ((< j 1) ll)))

;;;CANO->SEXP converts expressions or equations to SEXPRESSIONS.
(define (cano->sexp p horner)
  (if (clambda? p)
      (list 'lambda (list->vector (get-lambda-list
				   (if (eqn? p) (eqn->poly p) p)))
	    (bunch->sexp p horner))
      (bunch->sexp p horner)))

(define (deferedmath->sexp args)
  (let ((form (map (lambda (arg) (bunch->sexp arg #t)) args)))
    (if (some clambda? (cdr args))
	(list 'lambda
	      (list->vector (get-lambda-list (cdr args)))
	      form)
	form)))

;;; Make a routine to flatten instances of `op', remove `idents', and
;;; () -> `ident'.

(define (sexp:+ . args)
  (set! args (remove 0 args))
  (case (length args)
    ((0) 0)
    ((1) (car args))
    (else (cons '+ args))))

(define (sexp:* . args)
  (set! args (remove 1 args))
  (case (length args)
    ((0) 1)
    ((1) (car args))
    (else (cons '* args))))

(define (*->or-eqns e1)
  (cond ((and (pair? e1) (eq? '* (car e1)))
	 (let ((trms (remove-if number? (cdr e1))))
	   (case (length trms)
	     ((0) 0)
	     ((1) (list '= 0 (car trms)))
	     (else (cons 'or (map (lambda (s1) (list '= 0 s1)) trms))))))
	(else (list '= 0 e1))))

(define (sexp:^ x n)
  (case n
    ((0) 1)
    ((1) x)
    (else (list '^ x n))))

(define (sexp:over x y)
  (cond ((eqv? 1 y) x)
	((eqv? 0 x) 0)
	((equal? x y) 1)
	(else (list 'over x y))))

(define (terms->factors-list terms)
  (define (doit term texp terms)
    (cond ((null? terms) (if (one? term) '() (list (list (list term) texp))))
	  ((equal? term (car terms))
	   (doit term (+ 1 texp) (cdr terms)))
	  (else (cons (list (list term) texp)
		      (doit (car terms) 1 (cdr terms))))))
  (doit (car terms) 1 (cdr terms)))

;;; FACTORS-LIST is a list of lists of a list of factors and exponent.
;;; FACT-EXPS is a list of lists of factor and exponent.
(define (factors-list->fact-exps factors-list)
  (apply append
	 (map (lambda (facts-exp)
		(map (lambda (fact) (list fact (cadr facts-exp)))
		     (car facts-exp))
		;;(if (and (= 1 (length facts-exp)) (number? (car facts-exp))) facts-exp)
		)
	      factors-list)))

(define (sexp:terms->product-of-powers terms)
  (define (doit term texp terms)
    (cond ((null? terms) (list (sexp:^ term texp)))
	  ((equal? term (car terms))
	   (doit term (+ 1 texp) (cdr terms)))
	  (else (cons (sexp:^ term texp)
		      (doit (car terms) 1 (cdr terms))))))
  (apply sexp:* (doit (car terms) 1 (cdr terms))))

(define (sexp:decompose-rationally func expr)
  (cond ((rat? expr) (sexp:over (func (num expr))
			     (func (denom expr))))
	(else (func expr))))

;;;	Copyright 1989, 1990, 1991, 1992, 1993, 1996, 1997 Aubrey Jaffer.
