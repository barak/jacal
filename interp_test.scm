(load "/usr/local/lib/slib/mitscheme.init")
(load "/usr/local/lib/slib/require.scm")

(load "interpolate.scm")

(define-syntax assert
  (syntax-rules ()
    ((assert expression)
     (if expression
	 (display "Success!\n")
	 (begin
	   (display "Failed assert:\n")
	   (display 'expression)
	   (display " => ")
	   (display expression))))))

(define-syntax assert-not
  (syntax-rules ()
    ((assert-not expression)
     (if (not expression)
	 (display "Success!\n")
	 (begin
	   (display "Failed assert:\n")
	   (display expression))))))

(define-syntax assert-equal
  (syntax-rules ()
    ((assert-equal expression1 expression2)
     (if (equal? expression1 expression2)
	 (display "Success!\n")
	 (begin
	   (display "Failed assert-equal:\n")
	   (display "\t")
	   (display expression1)
	   (newline)
	   (display "\tnot equal? to\n")
	   (display "\t")
	   (display expression2))))))

(define-syntax assert-not-equal
  (syntax-rules ()
    ((assert-not-equal expression1 expression2)
     (if (not (equal? expression1 expression2))
	 (display "Success!\n")
	 (begin
	   (display "Failed assert-not-equal:\n")
	   (display "\t")
	   (display expression1)
	   (newline)
	   (display "\tequal? to\n")
	   (display "\t")
	   (display expression2))))))

(assert-not (interp:check-hermite-corectness '()))

(assert (interp:check-hermite-corectness '((1))))

(assert (interp:check-hermite-corectness '((1 1) (2 2))))

(assert-not (interp:check-hermite-corectness '((1) (2 2))))

(assert-not (interp:check-hermite-corectness '((1 1) (2))))

(assert-equal (interp:how-many-identical '((1 2) (1 3) (1 4))) 3)

(assert-equal (interp:how-many-identical '((1 2) (2 3) (1 4))) 0)

(assert-equal (interp:how-many-identical '((1 2))) 1)

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(display "fib without memoization:\n")
(fib 30)
(display "fib with memoization:\n")
(set! fib (memon fib))
(fib 30)
