;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright (C) 1989, 1990, 1991, 1992, 1993, 1995, 1997, 1998, 2002, 2005, 2006, 2007, 2009, 2010, 2019, 2020, 2023, 2024, 2026 Aubrey Jaffer.
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

;;; tps:std is the output template for priority strings var:pristr.
;;; It was extracted from English.scm so that it is not dependent on
;;; the user's language, and so variables can be created before the
;;; rest of the grammar system is loaded.

(define tps:std
  '(
    ()					; symbol translations
    ((template:default 140 #d0140 "(" #d1010 #(rest "," #d2010) ")")
     (template:bunch 140 "[" #d0010 #(rest "," break #d1010) "]")
     (template:parenthesis 200 "(" #d1010 ")")
     )
    (= 80 #d1080 "=" break #d2080 #(rest "=" break #d3080))
    (- 100 #d1100 "-" break #d2101 #(rest "-" break #d3101))
    (+ 100 #d1100 #(rest "+" break #d2101))
    (* 120 #d1120 #(rest "*" #d2121))
    (negate 90 "-" #d1090)
    (/ 120 #d1120 "/" #d2121)
    (over 120 #d1120 "/" #d2121)
    (^ 140 #d1141 "^" #d2140)
    (differential 170 #d1170 "'")
    (suchthat 40 "{" #d1190 "|" #d2040 "}")
    (satisfying 40 "{" #d1190 "::" #d2040 "}")
    (rref 200 #d1200 "[" #d2030 #(rest "," #d3010) "]")
    (box 200 ((-1 #\")
	      (0 (#\") #d1010 (#\"))
	      (1 #\")))
    (define 200 #d1120 ":" #d2010)
    (set 20 "set " #d1120 " " #d2010)
    (show 20 "show " #d1120)
    (factorial 160 #d1160 "!")
    (ncmult 110 #d1109 " . " #d2109)
    (^^ 210 #d1211 "^^" #d2210)
    ))
