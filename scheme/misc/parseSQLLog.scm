; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: parseSQLLog.scm,v 1.9 2003/09/24 22:37:07 dustin Exp $

(module parse-sql-log
	(import
	  (misclib "../misclib.scm")
	  (loglib "loglib.scm")
	  (stringlib "../stringlib.scm"))
	(main main))

; Structure for holding log entries
(define-struct log-entry
			   server time calls calltime)

; Structure for holding stats
(define-struct stats
			   last-time total-calls total-time)

; Process each line
(define (get-log-entry line)
  (let ((rv (make-log-entry)))
	(log-entry-time-set! rv (parse-2wire-date line))
	(let ((parts (string-split-chars line '(#\space #\+) 12)))
	  (log-entry-server-set! rv (list-ref parts 7))
	  (let ((tparts (string-split
					  (list-ref parts 10) #\/ 3)))
		(log-entry-calls-set! rv
							  (string->integer (list-ref tparts 1)))
		(log-entry-calltime-set! rv
								 (string->integer
								   (substring (car tparts) 0
											  (- (string-length
												   (car tparts))
												 2))))))
	rv))

(define (myfloat->string f)
  (llong->string (flonum->llong f)))

(define (print-update rrdfile last-time total-calls total-time)
  (print "update " rrdfile " "
		 ; (car (string-split (real->string last-time) #\. 2))
		 ; last-time
		 (myfloat->string last-time)
		 ":" total-calls ":" total-time))

(define (print-log-entry e)
  (display "Log entry:  ")
  (print (log-entry-time e)
		 " (" (myfloat->string (approx-time (log-entry-time e)))
		 ") " (log-entry-calls e) "/" (log-entry-calltime e)))

(define (init-new-stats)
  (let ( (st (make-stats)))
	(stats-last-time-set! st 0.0)
	(stats-total-calls-set! st 0.0)
	(stats-total-time-set! st 0.0)
	st))

(define (process-line byserver rrdprefix line)
  (let ((le (get-log-entry line)))
	; Get the entry by server
	(let ((st (assoc (log-entry-server le) byserver)))
	  ; This will get a pair, I need the cadr of it.  If it doesn't, I
	  ; need to make one
	  (if st
		(set! st (cadr st))
		(begin
		  (set! st (init-new-stats))
		  (set! byserver (append (list (list (log-entry-server le) st))
								 byserver))))
	  (let ((t (approx-time (log-entry-time le))))
		(if (and
			  (not (= t (stats-last-time st)))
			  (not (zero? (stats-last-time st)))
			  (> (stats-total-calls st) 0))
		  (begin
			(print-update
			  (string-append rrdprefix "-" (log-entry-server le) ".rrd")
			  (stats-last-time st)
			  (stats-total-calls st)
			  (stats-total-time st))
			(stats-total-calls-set! st 0)
			(stats-total-time-set! st 0)))
		(if (> (log-entry-calls le) 0)
		  (begin
			(stats-last-time-set! st t)
			(stats-total-calls-set! st (+ 1 (stats-total-calls st)))
			(stats-total-time-set! st (+ (stats-total-time st)
										 (truncate
										   (/ (log-entry-calltime le)
											  (log-entry-calls le)))))))))
	byserver))

; Grab each line from stdin, pass it to process-line-to-rrd
(define (process-to-rrd rrdprefix)
  ; We're going to store an associative list by connection name where the
  ; value of each is a stats struct
  (let ( (byserver '()))
	(conditional-input-loop
	  ; Only give me lines where this is true
	  (lambda (line) (strstr line "database.DBManager.sql" 20))
	  ; Then do this to them
	  (lambda (line)
		(try
			(set! byserver (process-line byserver rrdprefix line))
			(lambda (escape proc mes obj)
			  (warning proc + ":" mes " -- " obj " -- broken line?")
			  (escape #f)))))))

; main, get the rrd file, and start processing stdin
(define (main args)
  (if (< (length args) 2)
	(error "main"
		   (string-append "Usage:  " (car args) " filename.rrd")
		   args))
  (process-to-rrd (cadr args)))
