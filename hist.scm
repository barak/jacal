;; JACAL: Symbolic Mathematics System.        -*-scheme-*-
;; Copyright 1990, 1992, 1993 Aubrey Jaffer.
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


;;; This section deals with the user session protocol.

;;; The user can specify a line number prototype or the default can be
;;; used.  set prompt <legal identifier> sets the prototype.
;;; Subsequent unnumbered lines are assigned labels identical to the
;;; prototype except that the last field (consecutive alphabetic or
;;; consecutive numeric characters) is a unique field of the same type
;;; as the field.

;;; Equations can be referenced by their line numbers by operators
;;; which use equations.  Equations given as arguments to functions
;;; generate an error.  If the line number of an expression is given
;;; as an argument to an operator, @ = expression is the equation
;;; actually used.

;;; JACAL maintains a data base of equations and line numbers.  Each
;;; equation is associated with its line number.  The line number of
;;; each expression is associated with <line number> = expression.
;;; Each equation in the data base expresses a fact relating variables
;;; to each other.  Equations can be removed from the data base by the
;;; command dismiss(<line number>);.

;;; When a symbol which is a line number is used in an expression or
;;; as an argument to a function, the database is searched for an
;;; equation referencing that symbol.  One of those equations is used
;;; to eliminate variables or simplify the expression.
