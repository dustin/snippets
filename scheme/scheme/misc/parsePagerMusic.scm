; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: parsePagerMusic.scm,v 1.1 2002/12/28 07:02:14 dustin Exp $

(module parse-pager-music
		(import
		  (misclib "../misclib.scm")
		  (stringlib "../stringlib.scm"))
		(main main))

; Take lists of lists of lists and make them into a single flat list
(define (flatten l)
  (let ((rv '()))
	(for-each (lambda (x)
				(set! rv (append rv
						 (if (pair? x)
						   (flatten x)
						   (list x)))))
			  l)
	rv))

; Quote something for database use.
(define (dbquote s)
	(list->string (flatten
					(map (lambda (x)
						   (if (char=? #\' x)
							 '(#\' #\')
							 x))
						 (string->list s)))))

; Process a single, split chunk of an alert
(define (process-one parts)
  (print "insert into newmusic(title, data) values('"
		 (dbquote (cadr parts))
		 "', '" (dbquote (car parts)) "');"))

; Perform all of the given replacements on the given string
(define (replace-all-stuffs str l)
  (if (pair? l)
	(if (> (length l) 1)
		(pregexp-replace (caar l) (replace-all-stuffs str (cdr l)) (cdar l))
		(pregexp-replace (caar l) str (cdar l)))
	str))

; Do all required preprocessing on the given alert line
(define (preprocess-line line)
  (replace-all-stuffs
	(string-remove-chars line '(#\"))
	'(("(?i:\\s*twowayAlerts.com\\s*)" . "")
	  ("(?i:\\s*2wayx.com\\s*)" . " 2WX"))))

(define (main args)
  (conditional-input-loop
	(lambda (line) (strstr line "D32801" 0))
	(lambda (line)
	  (let ((parts
			  (pregexp-match
				"\\[!A0\\]\\[D32801\\]!\\*\\s*(.*?)\\s*(\\;.*\\*!)"
				(preprocess-line line))))
		(if parts
		  (process-one parts)
		  (warning line " should have matched, but did not"))))))
