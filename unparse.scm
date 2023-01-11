;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1992, 1993, 2020 Aubrey Jaffer.
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


;;; a LINE is a list of column positions and strings or characters
;;; which appear there on one horizontal line.

;;; a BOX is a list of pairs of line-numbers and LINEs.
;;; a PBOX consists of a BP (binding power) and a list of BOXes.

;;; a TEMPLATE is an HBOX.  An HBOX is a list of strings, symbols and
;;; VBOXes.  A VBOX is a list of pairs of a row position and an HBOX
;;; or a list of a character.

;;; A number of the form #dNBBB in a template is replaced with the BOX
;;; for argument N.  If the #dNBBB appears in a HBOX then it is glued
;;; horizontally to the other elements in the HBOX.  If the #dNBBB
;;; appears in a VBOX then it is glued vertically to the other
;;; elements in the VBOX.  BBB denotes the maximum binding power of
;;; the argument.  If the binding power of the argument is less than
;;; BBB, parenthesis will be added according to TEMPLATE:PARENTHESIS.

;;; Characters in HBOXes are expanded horizontally to fill any empty
;;; space in their line.  A list of a character VBOX is expanded
;;; vertically to fill any empty space in its containing HBOX.

;;; The symbol 'break will break the line if there is not enough
;;; horizontal room remaining.

;(require 'rev3-procedures)
(require 'common-list-functions)

(define unprs:lincnt 0)
(define unprs:linum 0)

;;; TEXT-WIDTH

(define text-width string-length)

;;; return the position of symbol #dNBBB

(define (arg-pos num)
  (quotient num 1000))

(define (arg-bp num)
  (modulo num 1000))

(define make-template cons)
(define template-bp car)
(define template-hbox cdr)

;;; Utility routines for dealing with BOX
;;; BOX is a list of (line-number-referred-to-center . LINE)
;;; LINE is a list of POSITIONs and STRINGs.
;(require 'record)
;(define pbox-type (make-record-type "box" '(bp lines)))
;(define make-pbox (record-constructor pbox-type))
;(define pbox-bp (record-accessor pbox-type 'bp))
;(define pbox-lines (record-accessor pbox-type 'lines))

(define make-pbox cons)
(define pbox-bp car)
(define pbox-lines cdr)

(define top-edge caar)

(define left-edge cadar)

(define (bottom-edge box)
  (caar (last-pair box)))

(define (right-edge box)
  (define redge 0)
  (for-each
   (lambda (line)
;;;     (define re 0)
;;;     (for-each (lambda (x)
;;;		 (cond ((number? x) (set! re x))
;;;		       ((string? x)
;;;			(set! re (+ (text-width x) re)))))
;;;	       (cdr line))
;;;     (set! redge (max re redge))
     (if (not (null? (cdr line)))
	 (let ((end (last line 2)))
	   (set! redge (max redge (+ (car end)
				     (if (string? (cadr end))
					 (text-width (cadr end))
					 0)))))))
   box)
  redge)

(define (shift-numbers! lst inc)
  (cond ((null? lst))
	(else (set-car! lst (+ inc (car lst)))
	      (shift-numbers! (cddr lst) inc))))

;;; routines for actually writing the 2d formatted output to a port.
;;; These can be replaced with code which does cursor positioning.

(define (flush-blanks box)
  (cond ((null? box) box)
	((> (top-edge box) (+ 1 unprs:linum))
	 ;; (newline)
	 (set! unprs:linum (+ -1 (top-edge box)))
	 (flush-blanks box))
	((null? (cdar box))
	 ;; (newline)
	 (set! unprs:linum (top-edge box))
	 (flush-blanks (cdr box)))
	(else box)))

(define (chunk-height box)
  (let ((base unprs:linum))
    (do ((b box (cdr b))
	 (unprs:linum base (top-edge b)))
	((or (null? b) (null? (cdar b)) (not (= (+ 1 unprs:linum) (top-edge b))))
	 (if (zero? (- unprs:linum base)) 1 (- unprs:linum base)))
      (set! unprs:linum (top-edge b)))))

(define (display-box box)
  (define h (get-page-height))
  (set! box (flush-blanks box))
  (set! unprs:linum (+ -1 (top-edge box)))
  (let loop ((box box))
    (cond ((null? box) box)
	  ((begin (set! box (flush-blanks box))
		  (< (+ unprs:lincnt (chunk-height box)) h))
	   (loop (display-chunk box)))
	  ((or (zero? h) (zero? unprs:lincnt)) (loop (display-chunk box)))
	  ((do-more)
	   (set! unprs:lincnt 0)
	   (loop (display-chunk box)))
	  (else #f))))

(define (display-chunk box)
  (do ((b box (cdr b)))
      ((or (null? b) (not (= (+ 1 unprs:linum) (top-edge b))) (null? (cdar b)))
;;;       (print (+ 1 unprs:linum) (and (not (null? b)) (top-edge b)) unprs:lincnt)
       b)
    (display-line 0 #\space (cdar b))
    (if (not (null? (cdr b)))
	(newline)
	(force-output))
    (set! unprs:linum (top-edge b))
    (set! unprs:lincnt (+ 1 unprs:lincnt))
    (and (null? (cdar b)) (not (zero? (get-page-height)))
	 (not (do-more)) (set! b '(())))))

(define (display-line hpos fillchr line)
  (cond ((null? line) hpos)
	((string? (cadr line))
	 (display (make-string (- (car line) hpos) fillchr))
	 (display (cadr line))
	 (display-line (+ (car line) (text-width (cadr line)))
		       fillchr
		       (cddr line)))
	((char? (cadr line))
	 (display (make-string (- (car line) hpos) fillchr))
	 (display-line (car line)
		       (cadr line)
		       (cddr line)))
	(else (slib:error 'display-line line))))

;;; Glue 2 boxes together horizontally, with the RIGHTBOX moved
;;; POSINC to the right

(define (hglue-lines! leftbox posinc rightbox)
  (cond ((null? rightbox) leftbox)
	((null? leftbox) '())
	((char? (car rightbox))
	 (cons (nconc (car leftbox) (list posinc (string (car rightbox))))
	       (hglue-lines! (cdr leftbox) posinc rightbox)))
	((< (top-edge leftbox) (top-edge rightbox))
	 (cons (car leftbox)
	       (hglue-lines! (cdr leftbox) posinc rightbox)))
	(else				;line up
	 (if (not (zero? posinc)) (shift-numbers! (cdar rightbox) posinc))
	 (cons (nconc (car leftbox) (cdar rightbox))
	       (hglue-lines! (cdr leftbox) posinc (cdr rightbox))))))

(define (hglue boxes hroom)
  (let ((arights (list (cond ((null? boxes) 0)
			     ((null? (car boxes)) 0)
			     ((char? (car (car boxes))) 1)
			     (else (right-edge (car boxes))))))
	(right -1) (top 0) (bottom 0))
    (do ((boxes boxes (cdr boxes))
	 (rights
	  arights
	  (begin (set-cdr!
		  rights
		  (if (null? (cdr boxes)) '()
		      (list (cond ((null? (cadr boxes)) 0)
				  ((char? (caadr boxes)) 1)
				  (else (right-edge (cadr boxes)))))))
		 (cdr rights)))
	 (redge 0 (+ redge (car rights))))
	((or (null? boxes) (and (positive? right) (> redge hroom)))
	 (if (<= redge hroom) (set! right -2)))
      (cond ((equal? (car boxes) '((0 0 break)))
	     (set! right redge))
	    ((char? (caar boxes)))
	    (else (set! bottom (max bottom (bottom-edge (car boxes))))
		  (set! top (min top (top-edge (car boxes)))))))
    (do ((boxes boxes (cdr boxes))
	 (rights arights (cdr rights))
	 (redge 0 (+ redge (car rights)))
	 (leftbox
	  (do ((i (+ -1 bottom) (+ -1 i))
	       (ans (list (list bottom)) (cons (list i) ans)))
	      ((< i top) ans))
	  (cond
	   ((equal? (car boxes) '((0 0 break)))
	    (cond ((= (+ redge (car rights)) right)
		   (set! arights #f)
		   (vformat! '((0 #d0000) (2 #d1000))
			     (list (make-pbox 200 (list leftbox))
				   (make-pbox 200 (cdr boxes)))
			     #f hroom))
		  (else leftbox)))
	   (else (hglue-lines! leftbox redge (car boxes))))))
	((or (not arights) (null? boxes)) leftbox))))

;;; Routines to format BOX from TEMPLATEs.

;;; HFORMAT returns a list of boxes which need to be HGLUEd together
;;; into a single box.

(define (hformat hbox args tps hroom)
  (if (null? hbox) '()
      (let ((item (car hbox)) (hr (- hroom (quotient hroom 20))))
	(cond
	 ((number? item)
	  (let ((argnum (arg-pos item)))
	    (if (> (length args) argnum)
		(let ((arg (list-ref args argnum)))
		  (if (<= (arg-bp item) (pbox-bp arg))
		      (nconc (pbox-lines arg)
			     (hformat (cdr hbox) args tps hroom))
		      (nconc
		       (hformat
			(template-hbox (cdr (assq 'template:parenthesis tps)))
			(list '() arg) tps hroom)
		       (hformat (cdr hbox) args tps hroom))))
		(hformat (cdr hbox) args tps hroom))))
	 ((pair? item)
	  (cons (vformat! item args tps hr)
		(hformat (cdr hbox) args tps hroom)))
	 ((vector? item)
	  (let ((vt (vector-ref item 0))
		(vl (cdr (vector->list item))))
	    (cond ((<= (length args) (arg-pos (find-if number? vl)))
		   (hformat (cdr hbox) args tps hroom))
		  ((eq? 'rest vt)
		   (nconc (hformat vl args tps hroom)
			  (hformat hbox (cdr args) tps (- hroom hr))))
		  ((eq? 'optional vt)
		   (nconc (hformat vl args tps hroom)
			  (hformat (cdr hbox) args tps hroom)))
		  (else (math:error 'hformat 'unknown-format item)))))
	 (else (cons (list (list 0 0 item))
		     (hformat (cdr hbox) args tps hroom)))))))

(define (vformat! vbox args tps hroom)
  (if (char? (car vbox))
      vbox
      (let* ((boxes (map (lambda (hbox)
			   (hglue (hformat (cdr hbox) args tps hroom)
				  hroom))
			 vbox))
	     (rights (map right-edge boxes))
	     (mostright (apply max rights)))
	(for-each
	 (lambda (box right)
	   (for-each
	    (lambda (line)
	      (cond ((null? (cdr line)))
		    ((and (char? (caddr line)) (null? (cdddr line)))
		     ;;terminate rubber line.
		     (nconc line (list mostright #\space)))
		    (else		;center justify line.
		     (let ((lst (cdr line))
			   (inc (quotient (- mostright right) 2)))
		       (set-car! lst (+ inc (car lst)))
		       (shift-numbers! (cddr lst) inc)))))
	    box))
	 boxes
	 rights)
	(vrenumber! vbox boxes)
	(apply nconc boxes))))

;;; the trick here is to make line 0 of hbox 0 still be line 0.  Push
;;; everything else up or down to make that happen.
;;; Here, vbox is used only for its hbox numbers.
(define (vrenumber! vbox boxes)
  (cond ((null? vbox) 0)
	((negative? (caar vbox))
	 (let ((topinc (+ (- (caar vbox)
			     (if (null? (cdr vbox)) 0 (caadr vbox)))
			  (- (vrenumber! (cdr vbox) (cdr boxes))
			     (bottom-edge (car boxes))))))
	   (or (zero? topinc)
	       (for-each
		(lambda (line)
		  (set-car! line (+ (car line) topinc)))
		(car boxes))))
	 (top-edge (car boxes)))
	(else
	 (or (zero? (caar vbox))
	     (for-each			;space down if no hbox 0
	      (lambda (line)
		(set-car! line (+ (car line) (caar vbox))))
	      (car boxes)))
	 (let ((topinc (bottom-edge (car boxes)))
	       (lastnum (caar vbox)))
	   (for-each
	    (lambda (hbox box)
	      (set! topinc (+ (- (car hbox) lastnum)
			      (- topinc (top-edge box))))
	      (set! lastnum (car hbox))
	      (for-each
	       (lambda (line)
		 (set-car! line (+ (car line) topinc)))
	       box)
	      (set! topinc (bottom-edge box)))
	    (cdr vbox)
	    (cdr boxes)))
	 (top-edge (car boxes)))))

;;; Driver for 2d output

(define (reset-line-count!)
  (set! unprs:lincnt 0))

(define (print-using-grammar exp grm)
  (template-print exp (grammar-write-tab grm)))

(define (template-print exp tps)
  (define owidth (get-page-width))
  (if (zero? owidth) (set! owidth 99999)) ;essentially infinite
  (display-box (hglue (pbox-lines (unparse exp tps owidth)) owidth)))

(define (unparse exp tps hroom)
  (cond ((symbol? exp)
	 (make-pbox 200 (list (list (list 0 0 (symbol->string exp))))))
	((number? exp)
	 (make-pbox 200 (list (list (list 0 0 (number->string exp))))))
	((list? exp)
	 (let* ((p (assq (car exp) tps)))
	   (unparse1 (if p (cdr p) (cdr (assq 'template:default tps)))
		     exp tps hroom)))
	((not (vector? exp))
	 (slib:error 'unparse 'not-s-expression exp))
	((zero? (vector-length exp))	;this special case should be eliminated
	 (make-pbox 200 (list (list (list 0 0 "[]")))))
	((and (vector? (vector-ref exp 0))
	      (assq 'template:matrix tps)
	      (let ((len (vector-length (vector-ref exp 0))))
		(every (lambda (r) (and (vector? r) (= (vector-length r) len)))
		       (cdr (vector->list exp)))))
	 (let ((hr (quotient hroom (vector-length exp)))
	       (template (cdr (assq 'template:matrix tps))))
	   (make-pbox
	    (template-bp template)
	    (hformat
	     (template-hbox template)
	     (map (lambda (obj)
		    (unparse1 (rubber-vbox (length obj)) obj tps hr))
		  ;transpose of exp
		  (apply map list (map vector->list
				       (vector->list exp))))
	     tps hroom))))
	(else (unparse1 (cdr (assq 'template:bunch tps))
			(vector->list exp) tps hroom))))

(define (unparse1 template exp tps hroom)
  (define hr (- hroom (quotient hroom 20)))
  (make-pbox (template-bp template)
	     (hformat (template-hbox template)
		      (map (lambda (exp)
			     (unparse exp tps hr))
			   exp)
		      tps hroom)))

(define (rubber-vbox len)
  (make-template 200 (list (rubvbox (- 1 len) len))))

(define (rubvbox i len)
  (if (>= i len) '()
      (cons (list i (+ (* 1000 (quotient (+ i len) 2)) 10))
	    (rubvbox (+ 2 i) len))))

;;;; Test code
;;; To test, load this and English.scm.  Do (test2d tps:2d).

(define (test2d tps)
  (for-each
   (lambda (b) (template-print b tps) (newline))
   '(
     (+ (* 3 a b) (* 2 (^ a 2) b) c (* d e))
     (sum (* (rapply a i) (^ x (+ -2 i))) i 0 inf)
     (= %gamma (limit (sum (- (over 1 n) (log m)) n 1 m) m inf))
     (^ (- (over 1 (^ (+ y x) 4)) (over 3 (^ (+ y x) 3))) 2)
     (^ x (over (+ (^ a 2) 1) a))
     (+ (- (over 2 (+ 2 x)) (over 2 (+ 1 x)))
	(over 1 (^ (+ 1 x) 2)))
     (over (* (+ (^ x 2) (* 2 x) 1) (+ -1 y))
	   (* 36 (+ 1 y)))
     (+ (* (^ %e (f x)) ((over (^ d 2) (^ dx 2)) (f x)))
	(* (^ %e (f x)) (^ ((over d dx) (f x)) 2)))
     (+ (integrate (f x) x a b) x)
     (integrate (^ e (^ x 2)) x)
     (limit (^ (f x) (g (+ 1 x))) x 0 minus)
     (+ (over y (^ (box x) 2)) x)
     (over (prod (^ (+ (^ x i) 1) (/ 5 2)) i 1 inf)
	   (+ (^ x 2) 1))
     (over 1 (+ 4 (over 1 (+ 3 (over 1 42)))))
     (over (* (factorial m) n (factorial (+ -1 n))) m)
     (- (* 16 (^ a 2))
	(* 2 (u 0 1) (at ((over d dx) (u x y)) (= x 0) (= y 1))))
     (+ (^ a (^ b c)) (^ (^ a b) c))
     )))

(define (textest)
  (for-each
   (lambda (b)
     (template-print b tps:2d) (newline)
     (template-print b tps:tex) (newline))
   '(
     (over (+ (^ a 2) c (* b x)) (+ 2 z))
     (+ (^ (+ a b) (/ 1 2)) (^ e (/ 1 2)))
     (arctan (arctan a))
     (over (arctan
	    (over (- (/ (* 2 (^ b (/ 1 4)) x) (^ a (/ 1 4)))
		     (sqrt 2))
		  (sqrt 2)))
	   (* (^ 2 (/ 3 2)) (^ a (/ 3 4)) (^ b (/ 1 4))))
     (over a (+ b c)))))

(define (mtest tps)
  (for-each
   (lambda (b) (template-print b tps) (newline))
   '(
     #((= x1 (negate (over 1 (sqrt 5))))
       (= x2 (negate (over 2 (sqrt 5)))))
     #(#((over 1 a) 0) #((negate (over b a)) 1))
     #(#((^ a 2) 0) #((+ (* a b) b) 1)))))
