; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: loglib.scm,v 1.1 2002/12/28 06:24:38 dustin Exp $

(module loglib
	  (import
		(stringlib "../stringlib.scm")
		(dates "../dates.scm"))
		(export parse-2wire-date))

; Parse the date out of the given line
(define (parse-2wire-date line)
  (+ (apply dates-seconds-for-time
		 (map string->integer
			  (string-split-chars
				(substring line 0 19)
				'(#\: #\space #\-)
				19)))
	 28800))
