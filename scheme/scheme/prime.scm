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

; Is a number prime?  Make use of the current known set of primes.
(define
  (learning-prime? x)
  (if (< x 2)
	#f ; no primes less than two
	(if (memv x primelist)
	  #t ; If it's in the primelist, it's prime
	  (let ((stopat (sqrt x)))
		(let loop ((l primelist))
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
				(loop (cdr l))))))))))

(define
  (next-prime x)
  ; Set the initial value to something reasonable
  (set! x
	(cond
	  ((< x 2) 2)
	  ((divisble? x 2) (+ x 1))
	  ((prime? x) (+ x 2))
	  (else x)))
  ; Find the next prime
  (do
	((i x (+ i 2)))
	( (prime? i) i)))

; Get the last value in a list
(define
  (last-in-list l)
  (car (list-tail l (- (length l) 1))))

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
  (next-prime (last-in-list primelist))
  primelist)

; Build the primes all the way to the specified number.
(define
  (build-primes-to x)
  (do ( (i (last-in-list primelist)))
	( (>= (last-in-list primelist) x) primelist)
	(build-next-prime)))

; Similar to learning-prime? but doesn't learn
(define
  (prime? x)
  (if (< x 2)
	#f ; no primes less than two
	(if (memv x primelist)
	  #t ; If it's in the primelist, it's prime
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
				(loop (cdr l))))))))))

(define
  (primes start-n stop-n)
  (range-sublist (build-primes-to stop-n) start-n stop-n))


; Inner loop test using another syntax
(define
  (ltest l)
  (letrec ( ( loop (lambda (myl)
				(if (null? myl)
				  '()
				  (loop (cdr myl))))))
	(loop l)))


(define
  (sum l)
	(if (null? l)
	  0
	  (+ (car l) (sum (cdr l)))))

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
