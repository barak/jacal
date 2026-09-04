;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 1996, 1997, 1998, 1999, 2002, 2005, 2006, 2007, 2008, 2009, 2010, 2019, 2020, 2021, 2024, 2026 Aubrey Jaffer.
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

;;; This file is "sexp.scm", containing the s-expression-to-math
;;; converter "seval", the math-to-s-expression converter
;;; "bunch->sexp", and the read-eval-print loop "batch1".

;;;; Here is older documentation:

;;; Label values have been split into SEXP and MATH (canonical) types.
;;; Only one type is assigned initially, and the other is cached if it
;;; gets computed.

;;; Per discussions with RMS, assignments will remain, syntactically
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
(require 'hash-table)
(require 'tree)

(define heqput! (hash-associator eq?))
(define heqrem! (hash-remover eq?))
(define hassq (predicate->hash-asso eq?))
(define (list-of-procedure-defsyms)
  (define proc-defs '())
  (hash-for-each (lambda (k v)
		   (if (procedure? (var:def v))
		       (set! proc-defs (cons k proc-defs))))
		 var-tab)
  proc-defs)

(define (defsym sym value)
  (if (sexp? value) (defsym-sexp sym value) (defsym-cano sym value)))
(define (defsym-sexp sym value)
  (set! *symdefs* (heqput! *symdefs* sym (cons value #f))) value)
(define (defsym-cano sym value)
  (set! *symdefs* (heqput! *symdefs* sym (cons #f value))) value)
(define (defsym-both sym sexp value)
  (set! *symdefs* (heqput! *symdefs* sym (cons sexp value))) novalue)
(define (undefsym sym)
  (set! *symdefs* (heqrem! *symdefs* sym)))

;;;; not currently used
;; (define (pairs->alist protos)
;;   (do ((pr protos (cddr pr))
;;        (alst '() (cons (cons (car pr) (cadr pr)) alst)))
;;       ((or (null? pr) (null? (cdr pr)))
;;        (cond ((null? pr) alst)
;; 	     (else (math:error 'defbltn 'odd-length-proto-list protos))))))

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

;;; vet Scheme symbol
(define (vet-sym sym)
  (define str (symbol->string sym))
  (define sl (string-length str))
  ;; (if (char=? #\: (string-ref str (+ -1 sl))) (set! sl (+ -1 sl)))
  (cond ((not (eqv? #\@ (string-ref str 0))) sym)
	((= sl 1) sym)
	(else
	 (let ((idx (or (string->number (substring str 1 sl))
			;; handle trailing ":" in shadow-var
			(string->number (substring str 1 (- sl 1))))))
	   (cond ((and (integer? idx) (positive? idx))
		  (string->symbol (string-append "@" (number->string idx))))
		 (else (math:warn 'expected-argument-symbol str) sym))))))

(define (symdef-lookup-cano sym hdns)
  (let ((vals (symdef-lookup sym hdns)))
    (cond ((not vals) (var->expl (sexp->var (vet-sym sym))))
	  ((not (pair? vals)) vals)
	  ((cdr vals) (cdr vals))
	  (else
	   (set-cdr! vals (seval (car vals) '()))
	   (cdr vals)))))

;;; used only for echoing symbol definition in batch1
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
  (display ";")
  (tran:display 'to-return-to-)
  (display "Scheme")
  (tran:display 'type-)
  (write-sexp 'help *input-grammar*)
  (display ";")
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
  (cond ((file-exists? file)
	 (fluid-let ((page-height page-height)
		     (page-width page-width)
		     (*input-grammar* *input-grammar*)
		     (*output-grammar* *output-grammar*)
		     (*echo-grammar* *echo-grammar*))
	   (set! page-height #f)
	   (with-input-from-file file batch1)))
	(else
	 (math:warn 'file-not-found file)
	 novalue)))

;;; BATCH-QUIETLY is used only for loading "init.math"
(define (batch-quietly file)
  (cond ((file-exists? file)
	 (call-with-input-file file
	   (lambda (iprt)
	     (fluid-let ((page-height page-height))
	       (set! page-height #f)
	       (let loop ()
		 (define obj (read-sexp *input-grammar* 0 iprt))
		 (cond ((not obj) (loop))
		       ((eof-object? obj))
		       (else
			(case (and (pair? obj) (car obj))
			  ((define)
			   (let* ((dvar (cadr obj))
				  (val (define-label dvar (caddr obj))))
			     (cond ((novalue? val)
				    (define-label dvar dvar)
				    (eval:error 'no-value-to-set (cadr obj)))
				   (else (loop)))))
			  ((initialcondition inversefunction)
			   (seval obj '())
			   (loop))
			  ((satisfying)
			   (let* ((dvar (if (symbol? (cadr obj))
					    (cadr obj)
					    (caadr obj)))
				  (val (define-label dvar
					 (if (symbol? (cadr obj))
					     (caddr obj)
					     obj))))
			     (cond ((novalue? val)
				    (define-label dvar dvar)
				    (eval:error 'no-value-to-set (cadr obj)))
				   (else (loop)))))

			  (else (math:warn 'non-definition-in file ': obj))))))))))
	(else (math:warn 'file-not-found file))))

(define (saw-newline? ip)
  (let lp ((pc (and (char-ready? ip) (peek-char ip))))
    (cond (initial-prompt? (set! initial-prompt? #f) #t)
	  ((not pc) #f)
	  ((char=? pc #\newline) #t)
	  ((char-whitespace? pc)
	   (read-char ip)
	   (lp (and (char-ready? ip) (peek-char ip))))
	  (else #f))))

;;; batch1 calls SEVAL through DEFINE-LABEL
(define (batch1)
  (do ((math:exit-saved math:exit)
       (var-news-saved var-news))
      ((call-with-current-continuation
	(lambda (math:exit-cnt)
	  (define obj #f)
	  (define cip (current-input-port))
	  (set! math:exit math:exit-cnt)
	  (newline)			;find unused var
	  (do () ((not (or (var-tab-lookup newlabelsym var-tab)
			   (hassq newlabelsym *symdefs*))))
	    (set! newlabelstr (chap:next-string newlabelstr))
	    (set! newlabelsym (string->symbol newlabelstr)))
	  (let loop ()
	    (define echoing (not (eq? 'null (grammar-name *echo-grammar*))))
	    (set! diag:indent 0)
	    (set! var-news '())
	    (cond (echoing)
		  ((not (saw-newline? cip)))
		  ((output-port? cip)
		   (display (string-append "(" newlabelstr ") ") cip)
		   (force-output cip))
		  (else (display (string-append "(" newlabelstr ") "))
			(force-output)))
	    (set! obj (read-sexp *input-grammar* (+ 3 (string-length newlabelstr)) (current-input-port)))
	    (cond ((not obj) (loop))
		  ((eof-object? obj) (math:exit #t))
		  ((and (symbol? obj) (symdef-lookup obj '()))
		   (write-sexp (list 'define obj (symdef-lookup-sexp obj '()))
			       *output-grammar*)
		   (newline)
		   (loop))
		  (else
		   (reset-line-count!)
		   (cond (echoing
			  (write-sexp obj *echo-grammar*)
			  (newline)))
		   (cond ((and (pair? obj) (eq? 'define (car obj)))
			  (let* ((dvar (cadr obj))
				 (val (define-label dvar (caddr obj))))
			    (out-new-vars var-news)
			    (cond ((novalue? val)
				   (define-label dvar dvar)
				   (eval:error 'no-value-to-set (cadr obj)))
				  ((eq? 'null (grammar-name *output-grammar*))
				   (set! % val))
				  (else
				   (set! % novalue)
				   (write-sexp (list 'define dvar (caddr obj))
					       (get-grammar 'standard))))))
			 (else
			  (let* ((dvar newlabelsym)
				 (val (define-label dvar obj)))
			    (out-new-vars var-news)
			    (cond ((boolean? val)
				   (set! % novalue)
				   (write-sexp val *output-grammar*))
				  ((or (novalue? val)
				       (and (expl:var? val)
					    (memv (car val) var-news)))
				   (define-label dvar dvar)
				   (loop))
				  ((eq? 'null (grammar-name *output-grammar*))
				   (set! % val))
				  ((and (expl:var? val)
					(strimbol? (var:sexp (expl->var val))))
				   (set! % novalue)
				   (write-sexp (var:sexp (expl->var val))
					       *output-grammar*))
				  (else
				   (set! % val)
				   (write-sexp (list 'define dvar
						     (symdef-lookup-sexp dvar '()))
					       *output-grammar*)))))))))
	  #f))
       (set! math:exit math:exit-saved)
       (set! var-news var-news-saved))))

(define (out-new-vars var-news)
  (if (not (eq? 'null (grammar-name *output-grammar*)))
      (for-each (lambda (x)
		  (write-sexp (list 'define
				    (var:sexp x)
				    (cano->sexp (extrule x) horner))
			      *output-grammar*)
		  (newline))
		var-news)))

(define (define-label label sexp)
  (define (one-arg-check)
    (cond ((not (pair? (cdr sexp)))
	   (bltn:error (car sexp) 'wna))
	  ((pair? (cddr sexp))
	   (bltn:error (car sexp) 'wna))
	  (else #f)))
  (cond ((symbol? label)
;;; foo:foo; undefines foo
	 (cond ((eq? label sexp) (undefsym label)
		(var->expl (sexp->var label)))
	       ((and (pair? sexp) (symbol? (car sexp)))
		(case (car sexp)
		  ((horner)
		   (or (one-arg-check)
		       (fluid-let ((horner #t))
			 (define-label label (cadr sexp)))))
		  ((time)
		   (or (one-arg-check)
		       (report-run-time
			(lambda () (define-label label (cadr sexp))))))
		  ((factor)
		   (or (one-arg-check)
		       (let ((e1 (seval-norm-top (cadr sexp) '())))
			 (define (fctr e2)
			   (cond ((number? e2) (int:factor e2))
				 ((sexp:rat-number? e2)
				  (sexp:over (int:factor (num e2))
					     (int:factor (denom e2))))
				 (else (require 'hensel)
				       (rat:factor->sexp e2))))
			 (define e3
			   (cond ((eqn? e1) (*->or-eqns (fctr (eqn->poly e1))))
				 ((licit? e1) (fctr e1))
				 (else (bltn:error 'not-a-scalar-expression-or-equation:-- e1))))
			 (defsym-both label e3 e1)
			 e3)))
		  (else			;must be CAPPLY
;;; seval-norm-top here breaks 1/cos(z);
		   (let ((val (seval-norm-top sexp (list label))))
		     (cond ((boolean? val) val)
			   (else (defsym label val)))))))
	       (else (defsym label (seval-norm-top sexp (list label))))))
	((not (pair? label))
	 (jacal:found-bug 'define-label label))
	((eqv? (car label) 'rref)
	 (cond ((not (and (= 3 (length label))
			  (symbol? (cadr label))
			  (symbol? (caddr label))))
		(eval:error 'invalid 'rref label)
		novalue)
	       (else
		(let ((var (sexp->var (cadr label))))
		  (define alpha (sexp:alpha-convert (cddr label) sexp))
		  (defsym-cano (cadr label) (var->expl var))
		  (var:set-recrule! var (seval-norm-top alpha (list (cadr label))))
		  ;; (var:set-def! var (var:recrule var))
		  (let ((rule (seval-norm-top alpha (list (cadr label)))))
		    (register-recurrence! var rule))
		  (defsym-cano (cadr label) (var->expl var))))))
	(else				;must be CAPPLY
	 (defsym-cano (car label)
	   (seval-norm-top (list 'lambda (cdr label) sexp)
			   (list (car label)))))))

;;; substitute @1 .. @n for (list) vars in sexp
(define (sexp:alpha-convert vars sexp)
  (define len (length vars))
  (define (ac sxp)
    (cond ((list? sxp) (map ac sxp))
	  ((vector? sxp) (list->vector (map sxp (vector->list sxp))))
	  ((and (symbol? sxp) (memq sxp vars))
	   => (lambda (rst)
		(string->symbol
		 (string-append "@" (number->string (- len (length rst) -1))))))
	  (else sxp)))
  (ac sexp))

(define (sym:idx sym)
  (define str (symbol->string sym))
  (define len (string-length str))
  (and (> len 1)
       (string=? "@" (substring str 0 1))
       (string->number (substring str 1 len))))

(define (sexp:find-if-lambdavar? tree)
  (define (fnd? obj)
    (cond ((and (symbol? obj)
		(char=? #\@ (string-ref (symbol->string obj) 0))))
	  ((pair? obj) (or (fnd? (car obj)) (fnd? (cdr obj))))
	  (else #f)))
  (fnd? tree))

(define (sexp:find? obj tree)
  (define (fnd? tre)
    (cond ((equal? obj tre))
	  ((pair? tre) (or (fnd? (car tre)) (fnd? (cdr tre))))
	  (else #f)))
  (fnd? tree))

;;; sexp is a Scheme expression
;;; hdns are the variables bound by lambda or suchthat.
;;; seval returns a polynomial or implicit polynomial
(define (seval sexp hdns)
  ;; (sexp:print 'seval sexp hdns)
  (cond ((number? sexp)
	 (cond ((inexact? sexp) (eval:error 'inexact-number:- sexp))
	       ((integer? sexp) sexp)
	       ((rational? sexp) (make-rat (numerator sexp) (denominator sexp)))
	       (else (eval:error 'unknown-number-type:-) sexp)))
	((vector? sexp) (map (lambda (x) (seval x hdns)) (vector->list sexp)))
	((symbol? sexp) (symdef-lookup-cano sexp hdns))
	((boolean? sexp) sexp)
	((null? sexp) sexp)
	((not (pair? sexp)) (eval:error 'seval 'wta sexp))
	(else
	 (case (car sexp)
	   ((time factor horner)
	    (sexp:warn (car sexp) 'must-be-at-top-level) novalue)
	   ;; creates a polynomial function
	   ((lambda)
	    (cond
	     ((not (= 3 (length sexp))) (eval:error 'lambda 'bad-form sexp))
	     (else
	      (let ((vars (cond ((symbol? (cadr sexp)) (list (cadr sexp)))
				((vector? (cadr sexp)) (vector->list (cadr sexp)))
				((pair? (cadr sexp)) (cadr sexp))
				(else (eval:error 'lambda 'bad-arglist sexp)))))
		(seval (sexp:alpha-convert vars (caddr sexp)) hdns)))))
;;; creates an algebraic extension, different from a radical
;;; needs to be extended to handle algebraic extension of trn function
	   ((suchthat)
	    (extize
	     #f ;; (sexp->var (cadr sexp)) ; use the dependent var label
	     (normalize (vsubst $ (sexp->var (cadr sexp))
				(licit->polxpr
				 (seval (caddr sexp) (cons (cadr sexp) hdns)))))))
;;; creates a differential extension
	   ((satisfying)
	    (cond
	     ((not (= 3 (length sexp)))
	      (math:warn 'satisfying 'bad-form sexp)
	      novalue)
	     ((not (and (pair? (caddr sexp)) (eqv? '= (caaddr sexp))))
	      (math:warn 'satisfying 'requires-equation sexp)
	      novalue)
	     ((not (and (pair? (cadr sexp)) (= 2 (length (cadr sexp)))))
	      (math:warn 'function-arity-not-handled sexp)
	      novalue)
	     ((sexp:find-if-lambdavar? sexp)
	      (sexp:warn (var:sexp $1) 'not-allowed-in- sexp)
	      novalue)
	     ((not (sexp:find? 'differential sexp))
	      (sexp:warn 'not-a-differential-equation- sexp)
	      novalue)
	     (else
	      (let ((sym (caadr sexp)))
		(define var (sexp->var sym))
		(define varl (var->expl var))
;;;; modified sexp:alpha-convert; all (fun @2) get replaced by @1
		(let ((sexp2 (subst (var:sexp $1)
				    (list sym (var:sexp $2))
				    (sexp:alpha-convert (list (cadr sexp)
							      (cadadr sexp))
							(caddr sexp)))))
;;; (sexp:print sym sexp)
;;; (sexp:print (list (cadr sexp) (var:sexp $2)) sexp2)
		  (cond ((sexp:find? sym sexp2)
			 (sexp:warn 'inconsistent-references-to- (cadr sexp) 'in sexp)
			 novalue)
			(else
;;; (:: (function arg) differential-equation)
;;; (satisfying (fun @1) eqn)
;;; creates a differential extension; instantiated when applied below;
;;; extension function must take a single argument
;;; $2 is bound to function instance variable, $1 its argument
			 (var:reset! var (licit->poleqn (seval sexp2 hdns)))
			 varl)))))))
	   ((inversefunction)		; name ::~ transcendental
	    (cond
	     ((and (= 3 (length sexp))
		   (symbol? (cadr sexp))
		   (symbol? (caddr sexp)))
	      (let ((ovar (sexp->var (caddr sexp))))
		(define dffrule (var:dffrule ovar))
		(cond
		 ;; ((not (and dffrule
		 ;; 	    (not (var:func ovar))
		 ;; 	    (poleqn:differential? dffrule)))
		 ;;  (math:warn 'inversefunction 'expected-abstract-transcendental-var- ovar)
		 ;;  novalue)
		 (else
		  (let ((var (sexp->var (cadr sexp)))
			(ndef (swapvars (var:differential $2)
					(var:differential $1)
					(swapvars $2 $1 dffrule))))
		    (define varl (var->expl var))
		    (var:reset! var ndef)
		    (var:set-inverse! var (var->expl ovar))
		    (var:set-inverse! ovar varl)
		    varl)))))
	     ((and (= 3 (length sexp))	; %W(x) ::~ x=%W*exp(%W)
		   (pair? (cadr sexp)) (= 2 (length (cadr sexp)))
		   (symbol? (caadr sexp)) (symbol? (cadadr sexp))
		   (pair? (caddr sexp)) (eqv? '= (caaddr sexp)))
	      (let ((var (sexp->var (caadr sexp))))
		(define sexp2 (sexp:alpha-convert (cadr sexp) (caddr sexp)))
		(var:definverse! var (licit->poleqn (seval sexp2 hdns)))
		(var->expl var)))
	     ((and (= 3 (length sexp))	; %W ::~ @1*exp(@1)
		   (symbol? (cadr sexp))
		   (pair? (caddr sexp)))
	      (let ((var (sexp->var (cadr sexp))))
		(define sexp2 (list '= (var:sexp $2) (caddr sexp)))
		(var:definverse! var (licit->poleqn (seval sexp2 hdns)))
		(var->expl var)))
	     (else
	      (math:warn 'inversefunction 'bad-form sexp)
	      novalue)))
	   ((initialcondition)		; func(const1) ::= const2;
	    (cond
	     ((not (= 3 (length sexp)))
	      (math:warn 'initialcondition 'bad-form sexp)
	      novalue)
	     ((not (or (number? (caddr sexp))
		       (and (pair? (caddr sexp)) (not (eqv? '= (caaddr sexp))))))
	      (math:warn 'initialcondition 'requires-expression sexp)
	      novalue)
	     ((and (eq? 'rref (caadr sexp)) (= 3 (length (cadr sexp))))
	      (let ((sym (cadadr sexp)))
		(define var (sexp->var sym))
		(define arg (seval-norm-top (car (cddadr sexp)) '()))
		(define val (seval-norm-top (caddr sexp) '()))
		(define fia (var:instances var))
		(cond ((not (number? arg))
		       (math:warn 'initialcondition 'must-be-integer arg)
		       novalue)
		      ((assv arg fia) => (lambda (pr) (set-cdr! pr val)
						 novalue))
		      (else (var:set-instances! var (cons (cons arg val) fia))
			    novalue))))
	     ((not (and (pair? (cadr sexp)) (= 2 (length (cadr sexp)))))
	      (math:warn 'function-arity-not-handled sexp)
	      novalue)
	     (else
	      (let ((sym (caadr sexp)))
		(define var (sexp->var sym))
		(define arg (seval-norm-top (cadadr sexp) '()))
		(define val (seval-norm-top (caddr sexp) '()))
		(define fia (var:instances var))
		(cond ((math:assoc arg fia) => (lambda (pr) (set-cdr! pr val) val))
		      (else (var:set-instances! var (cons (cons arg val) fia))
			    novalue))))))
	   ((define) (eval:error 'nested-defines? sexp))
	   (else
;;; Make application of a differential-lambda define a transcendental instance.
;;; fxpr is a polynomial or implicit polynomial.
	    (mapply (seval (car sexp) hdns)
		    (map (lambda (x) (seval x hdns)) (cdr sexp))))))))

;; (define (seval-norm f hdns) (normalize (seval f hdns)))
(define (seval-norm-top f hdns)
  (define sf (seval f hdns))
  (cond ((boolean? sf) sf)
	((licit:shadowed? sf) =>
	 (lambda (v)
	   (math:warn 'seval-norm-top 'escaped-shadow-variable v)
	   sf))
	(else (canonicalize sf))))

(define (cano->sexp p horner)
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
      (cond ((zero? dgr) (math:warn 'not-canonical p) '?1)
	    ((one? dgr) (rat->sexp (rat:num p) (rat:denom p)))
	    (else (list 'suchthat (var:sexp (car p)) (impoly->sexp p))))))

  (define (icano->sexp p)
    (cond ((bunch? p) (list->vector (map icano->sexp p))) ;inefficient
	  ((boolean? p) p)
	  ((symbol? p) p)
	  ((expl? p) (poly->sexp p))
	  ((impl? p)
	   (let ((dgr (poly:degree p $)))
	     (cond ((zero? dgr) (math:warn 'not-canonical p) '?2)
		   ((one? dgr) (rat->sexp (rat:num p) (rat:denom p)))
		   (else
		    (let ((fcts (map irimpl->sexp (univ:split-all p))))
		      (if (null? (cdr fcts)) (car fcts)
			  (cons 'or fcts)))))))
	  ((eqn? p) (list '= 0 (poly->sexp (eqn->poly p))))
	  (else (eval:error 'unknown 'type p))))
  (icano->sexp p))

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

(define (sexp:rat-number? sexp)
  (and (pair? sexp)
       (memq (car sexp) '(/ over))
       (= 3 (length sexp))
       (number? (cadr sexp))
       (number? (caddr sexp))))

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
	((math:equal? x y) 1)
	(else (list 'over x y))))

(define (terms->factors-list terms)
  (define (doit term texp terms)
    (cond ((null? terms) (if (one? term) '() (list (list (list term) texp))))
	  ((math:equal? term (car terms))
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
	  ((math:equal? term (car terms))
	   (doit term (+ 1 texp) (cdr terms)))
	  (else (cons (sexp:^ term texp)
		      (doit (car terms) 1 (cdr terms))))))
  (apply sexp:* (doit (car terms) 1 (cdr terms))))
