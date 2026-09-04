;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 2020, 2026 Aubrey Jaffer.
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

(define diag:indent 0)

(define (elim-diag id proc vars eqns)
  (cond ((and math:elims id)
	 (do ((i diag:indent (+ -1 i))) ((zero? i)) (display #\space))
	 (set! diag:indent (modulo (+ 2 diag:indent) 31))
	 (display id) (display " ")
	 (math:write1 (map var->expl vars) (get-grammar 'standard))
	 (display " from: ")
	 (math:write eqns (get-grammar 'standard))	 
	 (let ((res (proc vars eqns)))
	   (set! diag:indent (modulo (+ -2 diag:indent) 31))
	   (do ((i diag:indent (+ -1 i))) ((zero? i)) (display #\space))
	   (display id) (display " ==> ")
	   (math:write res (get-grammar 'standard))
	   res))
	(else (proc vars eqns))))

(define (trace-diag id proc body args)
  (cond ((and math:trace id)
	 (do ((i diag:indent (+ -1 i))) ((zero? i)) (display #\space))
	 (set! diag:indent (modulo (+ 2 diag:indent) 31))
	 (display id) (display " ") (math:write1 body (get-grammar 'standard))
	 (display " to: ") (math:write args (get-grammar 'standard))
	 (let ((res (proc body args)))
	   (set! diag:indent (modulo (+ -2 diag:indent) 31))
	   (do ((i diag:indent (+ -1 i))) ((zero? i)) (display #\space))
	   (display id) (display " ==> ")
	   (math:write res (get-grammar 'standard))
	   res))
	(else (proc body args))))

(define (phases-diag id proc arg)
  (cond ((and math:phases id (not (trifle? arg)))
	 (do ((i diag:indent (+ -1 i))) ((zero? i)) (display #\space))
	 (set! diag:indent (modulo (+ 2 diag:indent) 31))
	 (display id) (display " ") (math:write arg (get-grammar 'standard))
	 (let ((res (proc arg)))
	   (set! diag:indent (modulo (+ -2 diag:indent) 31))
	   (do ((i diag:indent (+ -1 i))) ((zero? i)) (display #\space))
	   (display id) (display " ==> ")
	   (math:write res (get-grammar 'standard))
	   res))
	(else (proc arg))))

;;	Save our vicinity for finding non-scheme files.
(define jacal-vicinity (program-vicinity))
;;	Load the core files.
(slib:load (in-vicinity (program-vicinity) "toploads"))
;;	Initialize modes to something reasonable.
(slib:load (in-vicinity (program-vicinity) "modeinit"))

;;;; error and interrupt response for SCM.
;;; Put appropriate handlers for other systems here.

(define (impl:error str args)
  (force-output)
  (newline-diag)
  (perror (tran:translate 'error))
  (set-errno 0)
  (display-diag str)
  (display-diag (tran:translate 'last-expression-lost))
  (newline-diag)
  (display-diag args)
  (newline-diag)
  (force-output)
  (math:exit #f))			;return to math top level.

;;;; These are error handlers for SCM.
;;(define out-of-storage #f)
(define could-not-open #f)
(define arithmetic-error #f)
;;(define user-interrupt #f)
(define end-of-program #f)
;(define hang-up end-of-program)		;automatic

(define (set-handlers!)
;;  (set! out-of-storage (lambda args (impl:error "Out of storage" args)))
  (set! could-not-open (lambda args (impl:error "File not found" args)))
  (set! arithmetic-error (lambda args (impl:error "Arithmetic Error" args)))
;;  (set! user-interrupt (lambda args (impl:error "User Interrupt" args)))
  (set! end-of-program (lambda args (math:exit #t))))

(define (cleanup-handlers!)
;;  (set! out-of-storage #f)
  (set! could-not-open #f)
  (set! arithmetic-error #f)
;;  (set! user-interrupt #f)
  (set! end-of-program #f))

;;	Load functions defined in standard grammar.
(batch-quietly (in-vicinity (program-vicinity) "init.math"))

;; (trace normalize canorm extize unitcan
;;        expr:norm-or-unitcan expr:canonicalize expr:canorm
;;        alg:simplify alg:clear-leading-exts
;;        poly:square-free-var poly:square-and-num-cont-free)
