; Find out whether a number is prime.

(define is-even?
	(lambda (x)
		(divisible? x 2)
	)
)

(define is-odd?
	(lambda (x)
		(not (is-even? x))
	)
)

(define divisible?
	(lambda (x y)
		(= 0 (modulo x y))
	)
)

; Find out whether a given number is prime
(define isprime
	(lambda (x)
		(cond
			( (= x 2) #t)
			( (is-even? x) #f)
			( else
				(let ( (done #f) (prime #t) )
					(do
						( (m 3 (+ m 2)) )

						; Loop until done
						(done)

						; The calculation
						(cond
							((= x m) (set! prime #t) (set! done #t))
							((> m (sqrt x)) (set! prime #t) (set! done #t))
							((divisible? x m) (set! prime #f) (set! done #t))
						)
					)
				; Return whether we're prime or not
				prime
				)
			)
		)
	)
)
