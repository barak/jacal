;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1992, 1993, 1996, 1997 Aubrey Jaffer.
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

(require 'alist)
(require-if 'compiling 'pretty-print)

;(require 'record)
;(define grammar-rtd
;  (make-record-type "grammar"
;		    '(name reader read-tab writer write-tab)))
;(define make-grammar (record-constructor grammar-rtd))
;(define grammar-name (record-accessor grammar-rtd 'name))
;(define grammar-reader (record-accessor grammar-rtd 'reader))
;(define grammar-read-tab (record-accessor grammar-rtd 'read-tab))
;(define grammar-writer (record-accessor grammar-rtd 'writer))
;(define grammar-write-tab (record-accessor grammar-rtd 'write-tab))

(define (make-grammar name reader read-tab writer write-tab)
  (cons (cons name reader)
	(cons read-tab (cons writer write-tab))))
(define grammar-name caar)
(define grammar-reader cdar)
(define grammar-read-tab cadr)
(define grammar-writer caddr)
(define grammar-write-tab cdddr)

(define *grammars* '())
(define grammar-associator (alist-associator eq?))
(define (defgrammar name grm)
  (set! *grammars* (grammar-associator *grammars* name grm)))
(define grammar-remover (alist-remover eq?))
(define (rem-grammar name grm)
  (set! *grammars* (grammar-remover *grammars* name grm)))
(define grammar-inquirer (alist-inquirer eq?))
(define (get-grammar name) (grammar-inquirer *grammars* name))
(define (list-of-grammars)
  (define grammars '())
  (alist-for-each (lambda (k v) (set! grammars (cons k grammars))) *grammars*)
  grammars)

(define (flush-input-whitespace port)
  (do ((chr (peek-char port) (peek-char port))
       (col 0 (case chr
		((#\space) (+ 1 col))
		((#\tab) (mod (+ 7 col) 8))
		((#\newline) 0)
		(else col))))
      ((or (eof-object? chr)
	   (not (char-whitespace? chr)))
       col)
    (read-char port)))

(defgrammar 'scheme
  (make-grammar 'scheme
		(lambda (grm) (read))
		#f
		(lambda (sexp grm) (write sexp) (force-output))
		#f))

(defgrammar 'null
  (make-grammar 'null
		(lambda (grm) (math:error 'cannot-read-null-grammar))
		#f
		(lambda (sexp grm) #t)
		#f))

;;; Establish autoload for PRETTY-PRINT.
(defgrammar 'schemepretty
  (let ((pploaded #f))
    (make-grammar 'schemepretty
		  (lambda (grm) (read))
		  #f
		  (lambda (sexp grm)
		    (or pploaded (begin (require 'pretty-print)
					(set! pploaded #t)))
		    (pretty-print sexp)
		    (force-output))
		  #f)))

(define (read-sexp grm icol)
  ((grammar-reader grm) grm icol))
(define (write-sexp sexp grm)
  ((grammar-writer grm) sexp grm))
(define (math:write e grm)
  (cond ((not (eq? 'null (grammar-name grm)))
	 (write-sexp (cano->sexp e horner) grm)
	 (newline)
	 (force-output))))

(define (write-diag obj) (write obj (current-error-port)))
(define (display-diag obj) (display obj (current-error-port)))
(define (newline-diag)
  (let ((cep (current-error-port)))
    (newline cep) (force-output cep)))

;;;; careful write for displaying internal stuff
(define (math:print . args)
  (define (print1 obj)
    (cond ((pair? obj)
	   (display-diag #\[)
	   (print1 (car obj))
	   (cond ((null? (cdr obj)))
		 ((list? (cdr obj))
		  (for-each (lambda (x) (display-diag #\space) (print1 x))
			    (cdr obj)))
		 (else (display-diag " . ") (print1 (cdr obj))))
	   (display-diag #\]))
	  ((poly:var? obj) (display-diag (var:sexp obj)))
	  (else (write-diag obj))))
  (define ans '())
  (for-each (lambda (obj)
	      (display-diag #\space)
	      (cond ((symbol? obj) (display-diag (tran:translate obj)))
		    (else (print1 obj) (set! ans obj))))
	    args)
  (newline-diag)
  ans)
(define (tran:translate sym)
  (let ((as (assq sym tran:translations)))
    (if as (cdr as) sym)))
(define (tran:display sym)
  (display (tran:translate sym)))
(define (math:warn . args)
  (newline)
  (force-output)
  (display-diag ";;;")
  (apply math:print args))
(define (math:error . args)
  (force-output)
  (apply math:warn args)
  (if math:debug (slib:error "") (math:exit #f)))
(define eval-error math:error)
(define (jacal:found-bug . args)
  (newline-diag)
  (display-diag
   "JACAL has reached a condition which should not be possible.
To help me to fix this bug, please send the text which follows this
message and a description of what you were doing to agj @ alum.mit.edu.
")
  (apply math:warn args))

(define (test ans fun . args)
  (let ((res (apply fun args)))
    (if (equal? ans res) #t (math:warn 'trouble-with fun))))

;;; outputs list of strings with as much per line as possible.
(define (block-write-strings lst)
  (let* ((column 5)
	 (width (- (get-page-width) column))
	 (ps (make-string column #\space)))
    (set! column width)
    (for-each (lambda (ap)
		(set! column (+ (string-length ap) column 1))
		(cond ((and (positive? width) (>= column width))
		       (newline)
		       (display ps)
		       (set! column (+ 1 (string-length ap))))
		      (else
		       (display " ")))
		(display ap))
	      lst)
    (newline)))

(define (get-page-height)
  (case page-height
    ((#f) 0)
    ((#t) (output-port-height (current-output-port)))
    (else page-height)))

(define (get-page-width)
  (case page-width
    ((#f) 0)
    ((#t) (output-port-width (current-output-port)))
    (else page-width)))

(define (paginate-file file)
  (call-with-input-file
      file
    (lambda (infile)
      (call-with-current-continuation
       (lambda (escape)
	 (let ((h (get-page-height))
	       (l 0))
	   (do ((c (read-char infile) (read-char infile)))
	       ((eof-object? c) novalue)
	     (display c)
	     (cond ((not (char=? #\newline c)))
		   ((zero? h))
		   ((< l h) (set! l (+ 1 l)))
		   ((do-more) (set! l 0))
		   (else (escape #f))))))))))

(define (do-more)
  (define helped #f)
  (tran:display 'more)
  (force-output)
  (let loop ((r (read-char)))
    (cond ((char=? #\space r) #t)
	  ((eof-object? r) #t)
	  ((char-whitespace? r) (loop (read-char)))
	  ((char-ci=? #\q r) #f)
	  (helped (loop (read-char)))
	  (else (tran:display 'q-to-quit-space-for-more:-)
		(force-output)
		(set! helped #t)
		(loop (read-char))))))
