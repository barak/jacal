
(define (report-run-time thunk)
  (let* ((start (get-internal-run-time))
	 (ans (thunk)))
    (sexp:print '/* 'execution 'took
		(/ (- (get-internal-run-time) start)
		   internal-time-units-per-second)
		'sec '*/)
    ans))
