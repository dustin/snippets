; Prime number stuff

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
			( (< x 2) #f)
			( (= x 2) #t)
			( (is-even? x) #f)
			( else
				(let ( (done #f) (prime #t) (stoppoint (sqrt x)) )
					(do
						( (m 3 (+ m 2)) )

						; Loop until done
						(done)

						; The calculation
						(cond
							((= x m) (set! prime #t) (set! done #t))
							((> m stoppoint) (set! prime #t) (set! done #t))
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

; Find the next prime number
(define nextprime
	(lambda (x)
		(if (isprime x) (set! x (+ 1 x)) x )
		(cond
			((= x 1) (set! x 2))
			((= x 2) x)
			((is-even? x) (set! x (+ 1 x)))
		)

		(let ((nextp x)(done #f))

			(do
				( (p x (+ p 2)))

				(done)

				(cond
					((isprime p) (set! done #t) (set! nextp p))
				)
			)
			nextp
		)
	)
)

; Find all the prime numbers from x to y
(define listprimes
	(lambda (x y)
		(if (isprime x) x (set! x (nextprime x)) )
		(let ((plist '()))
			(do
				( (i x (nextprime i)))
				((> i y))

				(set! plist (append plist (list i)))
			)

			plist
		)
	)
)
