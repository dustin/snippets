; Prime stuff

; This will let us know if numbers are divisible
(define
  (divisible? x y)
  (if (integer? x)
	(zero? (modulo x y))
	(let ((d (/ x y)))
	  (= d (round d)))))

; Need a prime list to start with
(define primelist '(2 3 5 7 11 13 17))

; Get the last value in a list
(define
  (end-of-list l)
  (list-ref l (- (length l) 1)))

; lispme doesn't support (let loop ()), so we'll separate this...probably
; should anyway.
(define (prime-in-list? x stopat l)
  (if (null? l)
	#f ; If the list is null, return false
	(if (< stopat (car l))
	  ; We found a prime, add it to the list, and return true
	  (begin
		(set! primelist
		  (append primelist (list x)))
		#t)
	  (if (divisible? x (car l))
		#f ; If it's divisible by an existing prime, it's not prime
		(prime-in-list? x stopat (cdr l))))))

; Is a number prime?  Make use of the current known set of primes.
(define
  (learning-prime? x)
  (if (< x 2)
	#f ; no primes less than two
	(if (memv x primelist)
	  #t ; If it's in the primelist, it's prime
	  (if (> (end-of-list primelist) x)
		#f ; We've already checked through here
		(prime-in-list? x (sqrt x) primelist)))))

; Get the sublist where the first value is greater than the given number
(define (greater-values l n)
  (if (null? l)
	#f
	(if (> (car l) n)
	  l
	  (greater-values (cdr l) n))))

; Find the next prime after the given number
(define
  (next-prime x)
  ; If we've already calculated primes past this place, we can just look it
  ; up rather than trying to calculate it
  (if (> (end-of-list primelist) x )
	(car (greater-values primelist x))
	(begin
	  ; Set the initial value to something reasonable
	  (set! x
		(cond
		  ((< x 2) 2)
		  ((divisible? x 2) (+ x 1))
		  ((learning-prime? x) (+ x 2))
		  (else x)))
	  ; Find the next prime
	  (do
		((i x (+ i 2)))
		( (learning-prime? i) i)))))

; Get a sublist of a list where the values are between min-n and max-n
; (inclusive).
; This function assumes a sorted list.
(define
  (range-sublist inlist min-n max-n)
  (let loop ( (l inlist) (rv '() ))
	(if (null? l)
	  rv
	  (if (> (car l) max-n )
		rv
		(begin
		  (if (>= (car l) min-n)
			(set! rv (append rv (list (car l)))))
		  (loop (cdr l) rv)
		  )))))

; Calling this will teach it a bit more
(define
  (build-next-prime)
  (next-prime (end-of-list primelist))
  primelist)

; Build the primes all the way to the specified number.
(define
  (build-primes-to x)
  (do ( (i (end-of-list primelist)))
	( (>= (end-of-list primelist) x) primelist)
	(build-next-prime)))

; Similar to learning-prime? but doesn't learn
(define
  (prime? x)
  (if (< x 2)
	#f ; no primes less than two
	(if (memv x primelist)
	  #t ; If it's in the primelist, it's prime
	  (if (> (end-of-list primelist) x)
		#f ; We've already checked through here
		(let ((stopat (sqrt x)))
		  (build-primes-to stopat)
		  (let loop ((l primelist))
			(if (null? l)
			  #f ; If the list is null, return false
			  (if (< stopat (car l))
				; We found a prime, add it to the list, and return true
				#t
				(if (divisible? x (car l))
				  #f ; If it's divisible by an existing prime, it's not prime
				  (loop (cdr l)))))))))))

; Generate a list of primes between start-n and stop-n by generating all
; primes up to stop-n and extracting a sublist
(define
  (primes start-n stop-n)
  (range-sublist (build-primes-to stop-n) start-n stop-n))

; Calculate the sum of a list
(define
  (sum l)
	(if (null? l)
	  0
	  (+ (car l) (sum (cdr l)))))

; Calculate the deltas of the values within a list
(define
  (deltas l)
  (let ( (rv '()))
	(if (null? (cdr l))
	  '()
	  (append rv (list (- (cadr l) (car l))) (deltas (cdr l))))))

; Get the sum of the primes up to a given number
(define (primesum x)
  (sum (primes 1 x)))

; Get the next prime sum that's prime
(define (next-prime-sum x)
  (set! x (next-prime x))
  (do
	((i x (next-prime i)))
	 ( (prime? (primesum i)) i)))

(define
  (primesums x)
  (let loop ( (l (primes 1 x)) (rv '()))
	(if (null? l)
	  rv
	  (begin
		(if (prime? (primesum (car l)))
		  (set! rv (append rv (list (car l)))))
		(loop (cdr l) rv)))))
