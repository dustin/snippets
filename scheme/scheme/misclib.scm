; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: misclib.scm,v 1.4 2002/12/28 06:58:12 dustin Exp $

(module misclib
		(export
		  input-loop
		  conditional-input-loop))

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

; Example
(define (misclib-main args)
  (if (> (length args) 1)
	(input-loop print (cadr args))
	(input-loop print)))
