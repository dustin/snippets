; Prime stuff

(define
  divisible?
    (lambda (x y)
	(cond
	    ( (= y x) #t)
	    ( (< x 16383 )
		(zero? (modulo x y)))
	    ( else (
		(let ((d (/ x y)))
		(= d (round d))))))))

(define
	even?
		(lambda (x)
			(divisible? x 2)))

(define
  prime?
    (lambda (x)
      (cond
	((< x 2) #f)
	((= x 2) #t)
	((even? x) #f)
	(else
	  (let ((stopat (sqrt x))
		(done #f) (p #t))
	    (do ((i 3 (+ i 2)))
		( done p )
		(cond
		  ( (> i stopat)
			(set! done #t))
		  ( (divisible? x i)
			(set! done #t)
			(set! p #f))
		  ( else #f )))))
		)))

(define
  next-prime
   (lambda (x)
      (set! x (cond
	((< x 2) 2)
	((even? x) (+ x 1))
	((prime? x) (+ x 2))
	(else x)))
      (do
	((i x (+ i 2)))
	( (prime? i) i)
	)))

(define
	primes
	(lambda (start-n stop-n)
	    (let ((prms '()))
		(set! start-n
			(next-prime start-n))
		(do
			( (i start-n
			    (next-prime i)))
			( (> i stop-n) prms )
			(set! prms (append
			  prms (list i)))))))

(define
  sum
     (lambda (l)
	(let ((s 0))
	  (for-each (lambda (i)
		(set! s (+ s i) ) ) l )
		  s)))

(define
  deltas
  (lambda (l)
	(let ((lastn #f) (tmp '()))
	  (for-each (lambda(i)
				  (cond
					((number? lastn)
					 (set! tmp
					   (append tmp
							   (list (- i lastn)))))
					(else #t))
				  (set! lastn i)) l)
	  tmp)))
