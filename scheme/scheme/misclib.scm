; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: misclib.scm,v 1.2 2002/12/28 03:34:23 dustin Exp $

(module misclib
		(export input-loop))

; Loop on input, pass each line to function f
(define (input-loop f p)
  ; Check argument types
  (if (not (port? p))
	(error "input-loop" "not a port" p))
  (if (not (procedure? f))
	(error "input-loop" "not a procedure" f))

  ; Do the loop
  (let loop ((line (read-line p)))
	(if (not (eof-object? line))
	  (begin
		(f line)
		(loop (read-line p))))))
