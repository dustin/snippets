; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: loglib.scm,v 1.3 2003/05/02 20:41:17 dustin Exp $

(module loglib
	  (import
		(stringlib "../stringlib.scm")
		(dates "../dates.scm"))
		(export
		  parse-2wire-date
		  parse-2wire-date-withmillis
		  approx-time))

; Parse the date out of the given line
; XXX:  This really needs to deal with daylight savings time better
(define (parse-2wire-date line)
  (+ (apply dates-seconds-for-time
		 (map string->integer
			  (string-split-chars
				(substring line 0 19)
				'(#\: #\space #\-)
				19)))
	 25200))

; Parse the date out of the given line (including milliseconds)
(define (parse-2wire-date-withmillis line)
  (+ (parse-2wire-date line)
	(/ (string->real (substring line 20 23)) 1000.0)))

; Truncate a date to the nearest minute
(define (approx-time x)
  (* 60 (truncate (/ x 60))))
