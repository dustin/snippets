; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: tsToMs.scm,v 1.2 2003/01/23 08:15:36 dustin Exp $

(module parse-timing-log
	(import
	  (misclib "../misclib.scm")
	  (loglib "loglib.scm")
	  (stringlib "../stringlib.scm"))
	(main main))

; ----------------------------------------------------------------------
; Struct definitions
; ----------------------------------------------------------------------

; Process the file
(define (process-logfile)
  (let ((oldts 0.0) (thists 0.0))
	(conditional-input-loop
	  (lambda (line) (strstr line "w51-" 0))
	  (lambda (line)
		(set! thists (parse-2wire-date-withmillis line))
		(print
		  (- thists oldts)
		  " "
		  (substring line 20 (- (string-length line) 20)))
		(set! oldts thists)))))

(define (main args)
  (process-logfile))
