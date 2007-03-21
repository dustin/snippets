; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: misclib.scm,v 1.7 2003/01/02 08:44:25 dustin Exp $

(module misclib
		(export
		  input-loop
		  conditional-input-loop
		  flatten-list))

; Loop on input, pass each line to function f.
; Optional argument should be either a port, or a filename.  If not
; provided, (current-input-port) will be used.
(define (input-loop f . other)
  ; Check argument types
  (if (not (procedure? f))
	(error "input-loop" "not a procedure" f))
  ; If a port was passed in, use it, otherwise use stdin
  (let ((p (if (null? other)
			 (current-input-port)
			 (car other))))
	; Figure out what it was, make it a port
	(set! p
	  (cond
		((input-port? p) p)
		((string? p) (open-input-file p))
		(else (error "input-loop" "not a port or filename" p))))
	; Make sure
	(if (not (input-port? p))
	  (error "input-loop" "couldn't turn argument into a port" (car other)))

	; Do the loop
	(let loop ((line (read-line p)))
	  (if (not (eof-object? line))
		(begin
		  (f line)
		  (loop (read-line p)))))))

; A conditional input loop (only loop on a line if (c line) is true)
(define (conditional-input-loop c f . other)
  (if (not (procedure? c))
	(error "conditional-input-loop"
		   "condition is not a function"
		   c))
  (let ((r (lambda (line)
			 (if (c line)
			   (f line)))))
	(if (null? other)
	  (input-loop r)
	  (input-loop r (car other)))))

; Take lists of lists of lists and make them into a single flat list
(define (flatten-list l)
  (cond
	((null? l) '())
	((pair? (car l))
	 (append
	   (flatten-list (car l))
	   (flatten-list (cdr l))))
	(else
	  (cons (car l)
			(flatten-list (cdr l))))))

; Create a list of numbers from from to to
(define (range from to)
  (if (> from to)
	'()
	(cons from (range (+ 1 from) to))))

; Get a list of all of the numbers between from and to (inclusive) where
; (proc n) is true
(define (filtered-range from to proc)
  (if (> from to)
	'()
	(if (proc from)
	  (cons from (filtered-range (+ 1 from) to proc))
	  (filtered-range (+ 1 from) to proc))))

; Perform the given procedure for every value from from to to
(define (do-range proc from to)
  (let loop ((i from))
			 (if (<= i to)
				(begin
				  (proc i)
				  (loop (+ 1 i)))
				#unspecified)))

; Perform the given operation on all of the numbers between from and to
; (inclusive) where (fproc n) is true 
(define (do-filtered-range proc from to fproc)
  (let loop ((i from))
			 (if (<= i to)
				(begin
				  (if (fproc i)
				  	(proc i))
				  (loop (+ 1 i)))
				#unspecified)))

; Example
(define (misclib-main args)
  (if (> (length args) 1)
	(input-loop print (cadr args))
	(input-loop print)))
