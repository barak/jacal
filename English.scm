;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright (C) 1989, 1990, 1991, 1992, 1993, 1995, 1997 Aubrey Jaffer.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
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

(require 'fluid-let)

(define tran:translations
  '((last-expression-lost . "; Last expression lost.")
    (bad-arglist . "bad arglist ")
    (column-of-non-matrix?:-- . "Column of non-matrix?: ")
    (coordinate-out-of-range:-- . "Coordinate out of range: ")
    (funny-length-vectors . "funny length vectors ")
    (unequal-length-vectors . "unequal length vectors ")
    (did-not-verify: . "Did not verify:")
    (error . "ERROR")
    (expt-of-equation?:-- . "Expt of equation?: ")
    (inexact-number-to-eval:- . "Inexact number to eval: ")
    (non-rational-radicand:-- . "Non-rational radicand: ")
    (not-a-number . "Not a Number")
    (not-a-matrix:-- . "Not a matrix: ")
    (row-of-non-matrix?:-- . "Row of non-matrix?: ")
    (application . "application ")
    (argument . " argument ")
    (arguments . " arguments ")
    (bad-info-entry . "bad info entry")
    (bunch . "bunch")
    (can-not-be-read . "can not be read")
    (can-not-be-set . "can not be set")
    (cannot-be-coerced-to-expl:-- . "cannot be coerced to expl:")
    (cannot-be-coerced-to-expr:-- . "cannot be coerced to expr: ")
    (cannot-be-coerced-to-implicit:-- . "cannot be coerced to implicit: ")
    (cannot-be-coerced-to-poly-eqn:-- . "cannot be coerced to poly eqn: ")
    (cannot-extract-denominator- . "cannot extract denominator ")
    (cannot-extract-numerator- . "cannot extract numerator ")
    (cannot-read-null-grammar . "cannot read null grammar")
    (cannot-suchthat-a-vector . "cannot suchthat a vector")
    (cc-of . "cc of ")
    (column-vector . "column vector")
    (could-not-clear-denominator-of:- . "could not clear denominator of: ")
    (delimiter-expected--ignoring-rest . "delimiter expected-ignoring rest")
    (determinant-of-non-square-matrix . "determinant of non-square matrix")
    (differential- . "differential ")
    (does-not-appear-in- . " does not appear in ")
    (does-not-divide- . " does not divide ")
    (does-not-udivide- . " does not udivide ")
    (dotproduct-of-unequal-size-matrices:-- . "dotproduct of unequal size matrices: ")
    (elim-bunch?- . "elim bunch?")
    (eliminating-from-more-than-one-expression?- . "eliminating from more than one expression?")
    (eliminating: . "eliminating:")
    (elimination-type-not-handled . "elimination type not handled")
    (equation . "equation")
    (expected-boolean . "expected boolean")
    (expected-boolean-or-number . "expected boolean or number")
    (expected-simple-symbol . "expected simple symbol")
    (expression-missing . "expression missing")
    (extra-delimiter . "extra delimiter")
    (extra-separator . "extra separator")
    (false . "false")
    (flag . "flag")
    (for-help. . " for help.")
    (free-var-to-var:elim . "free var to var:elim")
    (from: . " from:")
    (function-of- . " function of ")
    (grammar . "grammar")
    (implicit-expression . "implicit expression")
    (is-not-defined . "is not defined")
    (matrix . "matrix")
    (matrix-product-of-unequal-size-matrices:-- . "matrix product of unequal size matrices: ")
    (mismatched-delimiter . "mismatched delimiter")
    (more . "--more--")
    (nested-labels? . "nested labels? ")
    (no-example-for . "no example for ")
    (no-value-to-set . "no value to set")
    (no-variables? . "no variables?")
    (non-integer-power?-- . "non-integer power? ")
    (normalize-symbol?- . "normalize symbol? ")
    (normalizing: . "normalizing:")
    (not-a-bunch? . "not a bunch?")
    (not-a-function? . "not a function?")
    (not-a-polynomial-equation . "not a polynomial equation")
    (not-a-polynomial? . "not a polynomial?")
    (not-a-simple-variable:- . "not a simple variable: ")
    (not-an-integer- . "not an integer ")
    (not-an-operator . "not an operator")
    (not-canonical . "not canonical ")
    (not-enough-equations . "not enough equations")
    (not-known . "not known")
    (not-s-expression . " not s-expression")
    (number . "number")
    (of- . " of ")
    (off . "off")
    (on . "on")
    (or- . " or ")
    (partial-with-respect-to? . "partial with respect to?")
    (polynomial . "polynomial")
    (q-to-quit-space-for-more:- . " q to quit, space for more: ")
    (radical . "radical")
    (rational-expression . "rational expression")
    (redefined-from- . " redefined from ")
    (redefined-to- . " redefined to ")
    (row-vector . "row vector")
    (to- . " to ")
    (to-return-to- . " to return to ")
    (trouble-with . "trouble with ")
    (true . "true")
    (type . "type ")
    (type- . ", type ")
    (unknown . "unknown")
    (value-expected-equation-found:-- . "value expected, equation found: ")
    (variable . "variable ")
    (wna . ": Wrong number of args ")
    (wta . ": Wrong type ")
    (yielding: . "yielding:")))

;;;; Here are the templates for 2 dimensional output

(define tps:2d
  '(
    (template:default 140 #d0140 "(" #d1010 #(rest ", " #d2010) ")")
    (template:bunch 140 "[" #d0010 #(rest ", " break #d1010) "]")
    (template:matrix 140 (#\[) #d0010 #(rest "  " #d1010) (#\]))
    (template:parenthesis 200 "(" #d1010 ")")
    (- 100 #d1100 " - " break #d2101 #(rest " - " break #d3101))
    (negate 100 "- " #d1100)
    (+ 100 #d1100 #(rest " + " break #d2101))
    (* 120 #d1120 #(rest " " #d2121))
    (/ 120 #d1120 "/" #d2121)
    (over 120 ((-1 #d1040)
	       (0 #\-)
	       (1 #d2040)))
    (^ 140 #d1141 ((-1 #d2100)))
    (= 80 #d1080 " = " break #d2080 #(rest " = " break #d3080))
    (differential 170 #d1170 "'")
    (partial 130 " " ((-1 "%")
		      (0 #\-)
		      (1 "%" #d2140)) " " #d1140)
    (suchthat 40 "{" #d1190 " | " #d2040 "}")
    (define 200 #d1120 ": " ((0 #d2010)))
    (rapply 200 #d1200 ((1 #d2030 #(rest "," #d3010))))
    (abs 200 (#\|) #d1010 (#\|))
    (box 200 ((-1 #\")
	      (0 (#\") #d1010 (#\"))
	      (1 #\")))
    (factorial 160 #d1160 "!")
    (integrate 120 ((-3 #(optional #d4090))
		    (-2 "/ ")
		    (-1 "! ")
		    (0 "! ")
		    (1 "! ")
		    (2 "/ ")
		    (3 #(optional #d3090)))
	       #d1090 "d" #d2120)
    (limit 90 ((0 "limit ")
	       (1 #d2090 "->" #d3090))
	   #d1090)
    (sum 90 ((-3 #(optional #d4090))
	     (-2 "====")
	     (-1 "\\   ")
	     (0 " >  ")
	     (1 "/   ")
	     (2 "====")
	     (3 #(optional #d2090 #(optional" = " #d3090))))
	 #d1090)
    (prod 90 ((-3 " " #(optional #d4090))
	      (-2 "/===/")
	      (-1 " ! ! ")
	      (0  " ! ! ")
	      (1  " ! ! ")
	      (2 #(optional #d2090 #(optional" = " #d3090))))
	  #d1090)
    (at 90 #d1090
	((-2 "!")
	 (-1 "!")
	 (0 "!")
	 (1 "!")
	 (2 "!"))
	((2 #d2010 #(rest ", " #d3010))))
    (help 100 "help;")
    (qed 100 "qed;")
    (% 200 "%")
    (ncmult 110 #d1109 " . " #d2109)
    (^^ 210 #d1211 "^^" #d2210)
    ))

(define tps:c
  '(
    (template:default 140 #d0140 "(" #d1010 #(rest ", " #d2010) ")")
    (template:bunch 140 "{" #d0010 #(rest ", " #d1010) "}")
    (template:parenthesis 200 "(" #d1010 ")")
    (= 80 #d1080 " == " break #d2080 #(rest "==" break #d3080))
    (- 100 #d1100 " - " break #d2101 #(rest " - " break #d3101))
    (+ 100 #d1100 #(rest " + " break #d2101))
    (* 120 #d1120 #(rest " * " #d2121))
    (negate 90 "- " #d1090)
    (/ 120 #d1120 "/" #d2121)
    (over 120 #d1120 "/" #d2121)
    (^ 140 "pow(" #d1141 ", " #d2100 ")")
    (rapply 200 #d1200 "[" #d2030 #(rest "," #d3010) "]")
    (box 200 ((-1 #\")
	      (0 (#\") #d1010 (#\"))
	      (1 #\")))
    (define 200 #d1120 " = " #d2010)
    (set 20 "set " #d1120 " " #d2010)
    (show 20 "show " #d1120)
    ))

(define tps:std
  '(
    (template:default 140 #d0140 "(" #d1010 #(rest ", " #d2010) ")")
    (template:bunch 140 "[" #d0010 #(rest ", " break #d1010) "]")
    (template:parenthesis 200 "(" #d1010 ")")
    (= 80 #d1080 " = " break #d2080 #(rest " = " break #d3080))
    (- 100 #d1100 " - " break #d2101 #(rest " - " break #d3101))
    (+ 100 #d1100 #(rest " + " break #d2101))
    (* 120 #d1120 #(rest " * " #d2121))
    (negate 90 "- " #d1090)
    (/ 120 #d1120 "/" #d2121)
    (over 120 #d1120 "/" #d2121)
    (^ 140 #d1141 "^" #d2140)
    (differential 170 #d1170 "'")
    (suchthat 40 "{" #d1190 " | " #d2040 "}")
    (rapply 200 #d1200 "[" #d2030 #(rest "," #d3010) "]")
    (box 200 ((-1 #\")
	      (0 (#\") #d1010 (#\"))
	      (1 #\")))
    (define 200 #d1120 ": " #d2010)
    (set 20 "set " #d1120 " " #d2010)
    (show 20 "show " #d1120)
    (factorial 160 #d1160 "!")
    (help 100 "help;")
    (qed 100 "qed;")
    (% 200 "%")
    (ncmult 110 #d1109 " . " #d2109)
    (^^ 210 #d1211 "^^" #d2210)
    ))
(define tps:tex
  '(
    (template:top 0 "$" #d1000 "$")
    (template:default 140 #d0140 "\\left(" #d1010
		      #(rest ", " #d2010) "\\right)")
    (template:bunch 140 "\\left[" #d0010 #(rest ", " break #d1010) "\\right]")
;;;    (template:matrix 140 "\\left({\matrix{" #d0010 #(rest "&" #d1010)
;;;		     (#\\)(#\c)(#\r) "}}\\right)")
    (template:parenthesis 200 "\\left(" #d1010 "\\right)")
    (= 80 #d1080 " = " break #d2080 #(rest " = " break #d3080))
    (- 100 #d1100 " - " break #d2101 #(rest " - " break #d3101))
    (+ 100 #d1100 #(rest " + " break #d2101))
    (* 120 #d1120 #(rest "\\," #d2121))
    (negate 90 "- " #d1100)
    (/ 120 #d1120 "/{" break #d2121 "}")
    (over 120 "{" #d1040 "}\\over{" break #d2041 "}")
    (^ 140 #d1141 "^{" #d2100 "}")
    (differential 170 "{" #d1170 "}'")
    (suchthat 40 "\\left\\{ " #d1190 " | " break #d2040 "\\right\\}")
    (rapply 200 #d1200 "\\left[" #d2030 #(rest "," break #d3010) "\\right]")
    (abs 200 "\\left|" #d1010 "\\right|")
;;;    (box 200 ((-1 #\")
;;;	      (0 (#\") #d1010 (#\"))
;;;	      (1 #\")))
    (define 200 #d1120 ": " #d2010)
    (set 20 "set " #d1120 " " #d2010)
    (show 20 "show " #d1120)
    (factorial 160 #d1160 "!")
    (help 100 "help;")
    (qed 100 "qed;")
    (% 200 "%")
    ))

;;;;The parse tables.
;;; Definitions accumulate in top-level variable *SYN-DEFS*.

;;;Syntax definitions for STANDARD GRAMMAR
;;; Begin by Ignoring whitespace characters.
(set! *syn-defs* *syn-ignore-whitespace*)

;;; Character classes
(prec:define-grammar (tok:char-group 70 #\^ list->string))
(prec:define-grammar (tok:char-group 49 #\* list->string))
(prec:define-grammar (tok:char-group 50 #\/ list->string))
(prec:define-grammar (tok:char-group 51 '(#\+ #\-) list->string))
(prec:define-grammar (tok:char-group 20 '(#\|) list->string))
(prec:define-grammar (tok:char-group 30 '(#\< #\> #\= #\: #\~) list->string))
(prec:define-grammar (tok:char-group 40
		      (string-append "." tok:decimal-digits)
		      (lambda (l) (if (equal? '(#\.) l)
				      #\.
				      (string->number (list->string l))))))
(prec:define-grammar (tok:char-group 41
		      (string-append tok:upper-case tok:lower-case "@%?_")
		      list->string))
(prec:define-grammar (tok:char-group
       (lambda (chr) (or (eqv? #\" chr) (eof-object? chr)))
       #\"
       (lambda (l)
	 (tok:read-char) (list->string (cdr l)))))

;;; Delimiters and Separators

;;; Delimiters used to be defined here, but now are defined
;;; dynamically by parse functions.
(prec:define-grammar (prec:delim #\])
		     ;;(prec:delim #\;)
		     ;;(prec:delim #\,)
		     ;;(prec:postfix #\$ (lambda (x) (write x)) 0)
		     )

;;;prefix operators
(prec:define-grammar (prec:prefix '+ #f 100))
(prec:define-grammar (prec:prefix '- 'negate 100))
(prec:define-grammar (prec:prefix "+/-" 'u+/- 100))
(prec:define-grammar (prec:prefix "-/+" 'u-/+ 100))
(prec:define-grammar (prec:prefix '(not ~) 'impl:not 70))
(prec:define-grammar (prec:prefix ":" 'settemplate! 20))

;;;nary operators
(prec:define-grammar (prec:nary '* '* 120))
(prec:define-grammar (prec:nary '+ '+ 100))
(prec:define-grammar (prec:nary '- '- 100))
(prec:define-grammar (prec:nary "+/-" 'b+/- 100))
(prec:define-grammar (prec:nary "-/+" 'b-/+ 100))
(prec:define-grammar (prec:nary '/ '/ 120))
(prec:define-grammar (prec:nary '(and #\&) '& 60))
(prec:define-grammar (prec:nary 'or 'or 50))
(prec:define-grammar (prec:nary "||" 'parallel 110))

;;;infix operators
;(prec:infix 'x 'crossproduct 111 110)
(prec:define-grammar (prec:infix #\. 'ncmult 110 109))
(prec:define-grammar (prec:infix '(^ **) '^ 140 139))
(prec:define-grammar (prec:infix '^^ '^^ 210 210))
(prec:define-grammar (prec:infix '(":=" ":") 'define 180 20))
(prec:define-grammar (prec:infix '= '= 80 80))
;(prec:define-grammar (prec:infix '(~= <>) 'make-not-equal 80 80))
(prec:define-grammar (prec:infix 'mod 'mod 70 70))
(prec:define-grammar (prec:infix ':: 'suchthat 190 40))
(prec:define-grammar (prec:infix "|" 'suchthat 190 40))

;;; I don't remember what I had in mind here.
;(prec:define-grammar (prec:infix "" '* 120 120)) ;null operator

;;;postfix operators
(prec:define-grammar (prec:postfix #\! 'factorial 160))
(prec:define-grammar (prec:postfix #\' 'differential 170))

;;;matchfix operators
(prec:define-grammar (prec:matchfix #\( identity #f #\)))
(prec:define-grammar (prec:matchfix #\[ vector #\, #\]))
(prec:define-grammar (prec:matchfix #\{ 'or #\, #\}
				   (prec:infix "|" 'suchthat 190 40)))
(prec:define-grammar (prec:matchfix #\\ 'lambda #\, #\;))
(prec:define-grammar (prec:matchfix "|" 'abs #f "|"))

;;;special operators
(prec:define-grammar (prec:inmatchfix #\( list #\, #\) 200))
(prec:define-grammar (prec:inmatchfix #\[ 'rapply #\, #\] 200))

;;;rest operator reads expressions up to next delimiter.
(prec:define-grammar (prec:prestfix 'set 'set 10))
(prec:define-grammar (prec:prestfix 'show 'show 10))

;;;miscellany
;(prec:define-grammar (prec:prefix 'load 'load 50))
(prec:define-grammar (prec:nofix '% '%))
(prec:define-grammar (prec:nofix 'help 'help))
(prec:define-grammar (prec:nofix 'qed 'qed))

(prec:define-grammar (prec:commentfix "/*"
		  (lambda (str)
		    (and str
			 (call-with-input-string
			  str (lambda (pt)
				(fluid-let ((*prec:port* pt))
				  (if (eq? (get-grammar 'null) *echo-grammar*)
				      (do ((i (string-length str) (+ -1 i)))
					  ((zero? i))
					(tok:read-char))
				      (do ((i (string-length str) (+ -1 i)))
					  ((zero? i))
					(display (tok:read-char)))))))))
		  "*/"))

(defgrammar 'standard
  (make-grammar
   'standard				;name
   (lambda (grm)			;reader
     (prec:parse (grammar-read-tab grm) #\; (current-input-port)))
   *syn-defs*				;read-tab
   print-using-grammar			;writer
   tps:std))				;write-tab

(defgrammar 'disp2d
  (make-grammar
   'disp2d				;name
   (lambda (grm)			;reader
     (prec:parse (grammar-read-tab grm) #\; (current-input-port)))
   *syn-defs*				;read-tab
   print-using-grammar			;writer
   tps:2d))				;write-tab

(set! *input-grammar* (get-grammar 'standard))
(set! *output-grammar* (get-grammar 'disp2d))

;;;; Syntax definitions for TEX GRAMMAR

;;; Begin by Ignoring whitespace characters.
(set! *syn-defs* *syn-ignore-whitespace*)

(prec:define-grammar (tok:char-group 40 tok:decimal-digits
			(lambda (l) (string->number (list->string l)))))
(prec:define-grammar (tok:char-group 41
		        (string-append tok:upper-case tok:lower-case)
			list->string))
(let ((seen1 #f))
  (prec:define-grammar (tok:char-group
			(lambda (chr)
			  (cond (seen1 (not (char-alphabetic? chr)))
				((not (char-alphabetic? chr))
				 (set! seen1 chr) #t)
				(else (set! seen1 #t) #f)))
			'(#\\)
			(lambda (l)
			  (cond ((char? seen1) (tok:read-char)
					       (set! l (list #\\ seen1))))
			  (set! seen1 #f)
			  (list->string l)))))

(prec:define-grammar (prec:commentfix #\$ #f #\$))

(prec:define-grammar (prec:delim #\,))
(prec:define-grammar (prec:delim #\;))
(prec:define-grammar (prec:prefix #\+ #f 100))
(prec:define-grammar (prec:prefix #\- 'negate 100))
(prec:define-grammar (prec:postfix #\! 'factorial 160))
(prec:define-grammar (prec:postfix #\' 'differential 170))
(prec:define-grammar (prec:infix #\: 'define 180 20))
(prec:define-grammar (prec:infix #\= '= 80 80))
(prec:define-grammar (prec:nary '(#\* "\\,") '* 120))
(prec:define-grammar (prec:nary #\+ '+ 100))
(prec:define-grammar (prec:nary #\- '- 100))
(prec:define-grammar (prec:nary #\/ '/ 120))
(prec:define-grammar (prec:nary "\\over" '/ 120))
(prec:define-grammar (prec:nary #\& vector 50))
(prec:define-grammar (prec:nary "\\cr" vector 49))

(prec:define-grammar (prec:commentfix
		      '("\\left" "\\right"
				 "\\big" "\\bigm" "\\bigl" "\\bigr"
				 "\\bigg" "\\biggm" "\\biggl" "\\biggr"
				 "\\Big" "\\Bigm" "\\Bigl" "\\Bigr"
				 "\\Bigg" "\\Biggm" "\\Biggl" "\\Biggr")
		      #f
		      #f))
(prec:define-grammar (prec:commentfix #\% #f #\newline))

(prec:define-grammar (prec:inmatchfix #\( #f #\, #\) 200))
(prec:define-grammar (prec:matchfix #\( #f #f #\)))
(prec:define-grammar (prec:matchfix #\{ #f #f #\}))
(prec:define-grammar (prec:matchfix "\\lbrace" #f #f "\\rbrace"))
(prec:define-grammar (prec:inmatchfix #\[ 'rapply #\, #\] 200))
(prec:define-grammar (prec:inmatchfix "\\lbrack" 'rapply #\, "\\rbrack" 200))
(prec:define-grammar (prec:matchfix #\[ vector #\, #\]))
(prec:define-grammar (prec:infix '(#\| "\\vert") 'suchthat 190 40))
(prec:define-grammar (prec:infix #\^ '^ 140 139))
(prec:define-grammar (prec:prefix "\\sqrt" (lambda (arg) `(^ ,arg (/ 1 2))) 100))
(prec:define-grammar (prec:prefix "\\frac" '/ 100)) ;prefix2
;(prec:define-grammar (prec:delim "\\of"))
;(prec:define-grammar (prec:prefix "\\root" (lambda (arg) `(^ ,arg (/ 1 2))) 100))

(prec:define-grammar (prec:prefix 'load 'load 50))
(prec:define-grammar (prec:nofix '% '%))
(prec:define-grammar (prec:nofix 'help 'help))
(prec:define-grammar (prec:nofix '(qed bye exit) 'qed))
(prec:define-grammar (prec:prestfix 'set 'set 10))
(prec:define-grammar (prec:prestfix 'show 'show 10))

(defgrammar 'tex
  (make-grammar
   'tex					;name
   (lambda (grm)			;reader
     (prec:parse (grammar-read-tab grm) #\; (current-input-port)))
   *syn-defs*				;read-tab
   print-using-grammar			;writer
   tps:tex))				;write-tab
