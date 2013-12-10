;;; I, Vytautas Jancauskas, place this code in the public domain (2010).

(require 'common-list-functions)	; butlast
(require 'combinatorics)		; for menon and factorial

;;; ***************************************************************************
;;; Lagrange interpolation
;;; ***************************************************************************

(define (interp:coeff x xj xs)
  (cond ((null? xs) 1)
	(else (app* $1*$2
		    (app* $1/$2
			  (app* $1-$2 x (car xs))
			  (app* $1-$2 xj (car xs)))
		    (interp:coeff x xj (cdr xs))))))

(define (interp:only-xs points j index)
  ;; From a given list of points remove the one with index j and only
  ;; return the car of each point.
  (cond ((null? points) '())
	((= j index) (interp:only-xs (cdr points) j (+ index 1)))
	(else (cons (caar points)
		    (interp:only-xs (cdr points) j (+ index 1))))))

(define (interp:interp1 all-points points x index)
  (cond ((null? points) 0)
	(else (app* $1*$2+$3
		    (cadar points)
		    (interp:coeff x (caar points)
				  (interp:only-xs all-points index 0))
		    (interp:interp1 all-points (cdr points) x (+ index 1))))))

;;; **************************************************************************
;;; Newtons method for polynomial interpolation
;;; **************************************************************************

;;; Return a list without it's last element.
(define (interp:all-but-last lst) (butlast lst 1))

;;; Return a list without it's first element.
(define interp:all-but-first cdr)

(define (interp:last lst) (car (last-pair lst)))

(define (interp:divided-differences points)
  ;; The recursive divided differences function from the definition of
  ;; Newton's method:
  ;;
  ;; f[x(k - j), x(k - j + 1), ..., x(k)] =
  ;;
  ;;    f[x(k - j + 1), ..., x(k)] - f[x(k - j), ..., x(k - 1)]
  ;;    -------------------------------------------------------
  ;;                         x(k) - x(k - j)
  ;;
  (let ((identical (interp:how-many-identical points)))
    (if (> identical 0)
	(app* $1/$2
	      (list-ref (car points) identical)
	      (factorial (- identical 1)))
	(app* $1/$2
	      (app* $1-$2
		    (interp:divided-differences (interp:all-but-first points))
		    (interp:divided-differences (interp:all-but-last points)))
	      (app* $1-$2 (car (interp:last points)) (caar points))))))

(set! interp:divided-differences (memon interp:divided-differences))

(define (interp:newton-multiply var points)
  (if (null? points)
      1
      (app* $1*$2
	    (app* $1-$2 var (caar points))
	    (interp:newton-multiply var (cdr points)))))

(define (interp:newton-interpolation var points)
  (if (null? points)
      0
      (app* $1*$2+$3
	    (interp:divided-differences points)
	    (interp:newton-multiply var (cdr points))
	    (interp:newton-interpolation var (cdr points)))))

;;; ***************************************************************************
;;; Hermite interpolation
;;; ***************************************************************************

(define (interp:duplicate points)
  ;; Duplicate each point in the point list n times.
  (define (simple-duplicate point n)
    (if (= n 0)
	'()
	(cons point (simple-duplicate point (- n 1)))))
  (if (null? points)
      '()
      (append (simple-duplicate (car points) (- (length (car points)) 1))
	      (interp:duplicate (cdr points)))))

(define (interp:how-many-identical points)
  ;; If points is a list of length n and contains n identical points then
  ;; return n, otherwise return 0.
  (define (all-identical points point)
    (cond ((null? points) #t)
	  ((not (equal? (car point) (caar points))) #f)
	  (else (all-identical (cdr points) point))))
  (let ((n (length points)))
    (if (and (> n 0) (all-identical points (car points)))
	n
	0)))

;;; ***************************************************************************
;;; Neville algorithm
;;; ***************************************************************************

(define (interp:neville var points)
  (if (= (length points) 1)
      (cadar points)
      (app*
       $1/$2
       (app* $1+$2
	     (app* $1*$2
		   (app* $1-$2
			 var
			 (car (interp:last points)))
		   (interp:neville var (interp:all-but-last points)))
	     (app* $1*$2
		   (app* $1-$2
			 (caar points)
			 var)
		   (interp:neville var (interp:all-but-first points))))
       (app* $1-$2 (caar points) (car (interp:last points))))))

;;; ***************************************************************************
;;; Taylor polynomial
;;; ***************************************************************************

(define (interp:taylor expr a n)
  (define $1-a (app* $1-$2 cidentity a))
  (define (taylor expr $1-a^i i)
    (cond ((> i n)
	   0)
	  (else
	   (app* $1*$2+$3
		 (app* $1/$2 (app* expr a) (factorial i))
		 $1-a^i
		 (taylor (diff expr $1) (app* $1*$2 $1-a^i $1-a) (+ i 1))))))
  (taylor expr 1 0))

;;; ***************************************************************************
;;; Main procedure and validity checking
;;; ***************************************************************************

(define (interp:overdefined? points)
  ;; Returns #t if points contains multiple definitions for the same argument,
  ;; returns #f otherwise.
  (define (overdefined? points xs)
    (cond ((null? points) #f)
	  ((member (caar points) xs) #t)
	  (else (overdefined? (cdr points) (cons (caar points) xs)))))
  (overdefined? points '()))

(define (interp:preprocess args)
  ;; Preprocess a set of points given by the interp built-in
  (if (matrix? (car args))
      (car args)
      args))

(define (interp:interp var points method)
  (let ((points (interp:preprocess points)))
    (cond ((interp:overdefined? points)
	   (math:error 'multiply-defined-points points))
	  ((null? points)
	   (math:error 'point-list-is-empty))
	  (else
	   (cond ((eq? method 'lagrange)
		  (interp:interp1 points points var 0))
		 ((eq? method 'newton)
		  (let ((dup-points (interp:duplicate points)))
		    (interp:newton-interpolation var dup-points)))
		 ((eq? method 'neville)
		  (interp:neville var points)))))))

;(trace interp:all-but-last)
;(trace interp:all-but-first)
;(trace interp:last)
;(trace interp:divided-differences)
;(trace interp:newton-multiply)
;(trace interp:newton-interpolation)
;(trace interp:interp)
;(trace interp:duplicate)