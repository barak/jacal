;;; I, Vytautas Jancauskas, place this code in the public domain (2010).

;;; Parts of the code below were adapted from:
;;; http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.98.8156

(define (make-matrix m n)
  (let ((matrix (make-vector m)))
    (define (populate i)
      (if (= i 0)
	  (vector-set! matrix i (make-vector n))
	  (begin
	    (vector-set! matrix i (make-vector n))
	    (populate (- i 1)))))
    (populate (- m 1))
    matrix))

(define (matrix-ref matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define (matrix-set! matrix i j val)
  (vector-set! (vector-ref matrix i) j val))

(define (calculate-c a m k)
  (let ((c (make-vector (+ k 1)))
	(big-a (make-matrix (+ m 1) (+ k 1))))
    (vector-set! c k 1)
    (vector-set! c 0 0)
    (do ((i 1 (+ i 1)))
	((>= i k))
      (matrix-set! big-a m i (vector-ref a (- (* k m) i))))
    (do ((i 1 (+ i 1)))
	((>= i (+ m 1)))
      (matrix-set! big-a i 0 1))
    (do ((i 1 (+ i 1)))
	((>= i k))
      (let ((sum1 0))
	(do ((l 1 (+ l 1)))
	    ((>= l m))
	  (do ((j 1 (+ j 1)))
	      ((>= j i))
	    (set! sum1 (app* $1+$2
			     sum1
			     (app* $1*$2
				   (vector-ref c (+ k (- i) j))
				   (matrix-ref big-a l j))))))
	(vector-set! c (- k i)
		     (app* $1/$2
			   (app* $1-$2
				 (vector-ref a (- (* k m) i))
				 sum1)
			   m))
	(do ((l (- m 1) (- l 1)))
	    ((<= l 0))
	  (let ((sum2 0))
	    (do ((j 0 (+ j 1)))
		((>= j i))
	      (set! sum2 (app* $1+$2
			       sum2
			       (app* $1*$2
				     (vector-ref c (+ k (- i) j))
				     (matrix-ref big-a l j)))))
	    (matrix-set! big-a l i (app* $1-$2
					 (matrix-ref big-a (+ l 1) i)
					 sum2))))))
    c))

(define (calculate-s c)
  (do ((i 1 (+ i 1)))
      ((or (>= i (vector-length c))
	   (not (equal? (vector-ref c i) 0))) i)))

(define (calculate-b a c m k)
  (let ((b (make-vector (+ m 1)))
	(big-b (make-matrix (+ m 1) (+ k 1)))
	(s (calculate-s c)))
    (do ((i 1 (+ i 1)))
	((>= i k))
      (matrix-set! big-b 1 i (vector-ref c i)))
    (do ((l 2 (+ l 1)))
	((>= l (+ m 1)))
      (do ((i 1 (+ i 1)))
	  ((>= i (+ k 1)))
	(let ((sum 0))
	  (do ((j (max 1 (- i (* k (- l 1)))) (+ j 1)))
	      ((>= j (+ (min k (+ (- i l) 1)) 1)))
	    (set! sum (app* $1+$2
			    sum
			    (app* $1*$2
				  (vector-ref c j)
				  (matrix-ref big-b (- l 1) (- i j))))))
	  (matrix-set! big-b l i sum))))
    (do ((l 1 (+ l 1)))
	((>= l m))
      (let ((sum 0))
	(do ((p 1 (+ p 1)))
	    ((>= p l))
	  (set! sum (app* $1+$2
			  sum
			  (app* $1*$2
				(vector-ref b p)
				(matrix-ref big-b p (* s l))))))
	(vector-set! b l (app* $1/$2
			       (app* $1-$2
				     (vector-ref a (* s l))
				     sum)
			       (poly-expt
				(vector-ref c s)
				l)))))
    (vector-set! b 0 (vector-ref a 0))
    (vector-set! b m 1)
    b))

(define (make-poly var args)
  (reduce (lambda (p c) (app* $1*$2+$3 p var c))
	  (reverse
	   (if (and (= (length args) 1) (bunch? (car args)))
	       (car args)
	       args))))

(define (poly-expt poly n)
  (if (= n 0)
      1
      (app* $1*$2
	    poly
	    (poly-expt poly (- n 1)))))

(define (all-divisors n)
  (let ((half (/ n 2)))
    (define (helper i)
      (cond ((> i half) '())
	    ((= (remainder n i) 0)
	     (cons i (helper (+ i 1))))
	    (else (helper (+ i 1)))))
    (helper 2)))

(define (compose f g var)
  (let ((g-poly (make-poly var g)))
    (define (helper coeffs)
      (if (null? coeffs)
	  0
	  (app* $1+$2
		(app* $1*$2
		      (car coeffs)
		      (poly-expt g-poly (- (length coeffs) 1)))
		(helper (cdr coeffs)))))
    (coeffs (helper (reverse f)) (expl->var var))))

(define (verify h f g var)
  (app* $1=$2
	(make-poly var (compose f g var))
	(make-poly var h)))

(define (decompose-1 coeffs var m k)
  (let* ((a (list->vector coeffs))
	 (c (calculate-c a m k))
	 (b (calculate-b a c m k)))
    (list (vector->list b) (vector->list c))))

(define (decompose coeffs var)
  (let* ((n (- (length coeffs) 1))
	 (divisors (reverse (all-divisors n))))
    (define (helper divs)
      (if (null? divs)
	  (list (make-poly var coeffs)
		(var->expl var))
	  (let* ((k (car divs))
		 (m (/ n k))
		 (result (decompose-1 coeffs var m k))
		 (f (car result))
		 (g (cadr result))
		 (verification (verify coeffs f g var)))
	    (if (eqv? 0 (cdr verification))
		(list (make-poly var f)
		      (make-poly var g))
		(helper (cdr divs))))))
    (helper divisors)))
