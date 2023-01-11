;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1989, 1990, 1991, 1992, 1993, 2020 Aubrey Jaffer.
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

;;	Save our vicinity for finding non-scheme files.
(define jacal-vicinity (program-vicinity))
;;	Load the core files.
(slib:load (in-vicinity (program-vicinity) "toploads"))
;;	Initialize modes to something reasonable.
(slib:load (in-vicinity (program-vicinity) "modeinit"))
;;	Load functions defined in standard grammar.
(batch-quietly (in-vicinity (program-vicinity) "init.math"))

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

;; (trace make-shadow memshad var:shadow simple-shadowed-lambdavar? capply ext:elim var:elim deferop)
