; string->binary

(define
	string->integers
		(lambda (s)
			(map (lambda (i)
				(char->integer i))
					(string->list s)
					)))

(define
  shiftright
  (lambda (s x)
	(do
	  ((i 0 (+ 1 i)))
	  ((>= i x) s)
	  (set! s (truncate (/ s 2)))
	  )))

(define
  bitset?
  (lambda (i b)
	(odd?
	  (shiftright i b))))

(define
  int->bits
  (lambda (in)
	(let ((rv ""))
	  (do
		((i 7 (- i 1)))
		((< i 0) rv)
		(set! rv (cond
		  ( (bitset? in i) (string-append rv "1"))
		  (else (string-append rv "0"))
		  ))))))

(define
  string->binary
  (lambda (s)
	(map
	  (lambda (i)
		(int->bits i))
	  (string->integers s))))
