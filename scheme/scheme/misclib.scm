; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: misclib.scm,v 1.1 2002/12/28 02:06:23 dustin Exp $

(module misclib
		(export input-loop))

; Loop on input, pass each line to function f
(define (input-loop f p)
  (if (not (port? p))
	(error "input-loop" "not a port" p))
  (if (not (procedure? f))
	(error "input-loop" "not a procedure" f))

  (let loop ((line (read-line p)))
	(if (not (eof-object? line))
	  (begin
		(f line)
		(loop (read-line p))))))
