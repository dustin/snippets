; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: parseTimingLog.scm,v 1.3 2002/12/28 21:11:03 dustin Exp $

(module parse-timing-log
	(import
	  (misclib "../misclib.scm")
	  (loglib "loglib.scm")
	  (stringlib "../stringlib.scm"))
	(main main))

; ----------------------------------------------------------------------
; Struct definitions
; ----------------------------------------------------------------------

; Individual log entry
(define-struct log-entry
			   timestamp nearest-ts serial type state)

; Complete timing info
(define-struct log-timing
			   start nearest-ts stop type serial)

; This maintains the stats per time block
(define-struct per-block
			   ts counts times total)

; ----------------------------------------------------------------------
; Globals.
; ----------------------------------------------------------------------

; The different types of logs
(define *log-types* '("HB" "BOOT" "KICK" "XMLRPC"))

; This is where we keep up with started transaction types
(define *eventcache* (make-hashtable))

; Here we track the current list of objects to print
(define *perblock* (make-hashtable))

; ----------------------------------------------------------------------
; Miscellaneous support routines.
; ----------------------------------------------------------------------

; Parse a log entry
(define (parse-log-entry line)
  (let ((rv (make-log-entry))
		(parts (string-split line #\space 12)))
	; Make sure the line split properly
	(if (< (length parts) 9)
	  (error "parse-log-entry" "Line didn't parse properly"
			 (list (length parts) line)))
	(log-entry-timestamp-set! rv (parse-2wire-date-withmillis line))
	(log-entry-nearest-ts-set! rv (approx-time (log-entry-timestamp rv)))
	(log-entry-serial-set! rv (list-ref parts 6))
	(log-entry-type-set! rv (list-ref parts 7))
	(log-entry-state-set! rv (list-ref parts 8))
	rv))

; Create a timing struct from two log entries
(define (get-timing start end)
  (let ((rv (make-log-timing)))
	(log-timing-start-set! rv (log-entry-timestamp start))
	(log-timing-stop-set! rv (log-entry-timestamp end))
	(log-timing-type-set! rv (log-entry-type start))
	(log-timing-serial-set! rv (log-entry-serial start))
	(log-timing-nearest-ts-set! rv (log-entry-nearest-ts start))
	rv))

; Print a log entry (debug)
(define (print-log-entry le)
  (print "Log entry - ts:"
		 (log-entry-timestamp le)
		 " near: " (log-entry-nearest-ts le)
		 " ser: " (log-entry-serial le)
		 " type: " (log-entry-type le)
		 " state: " (log-entry-state le)))

; Create a block for a per-block entry
(define (init-per-block ts)
  (let ((rv (make-per-block)))
	(per-block-ts-set! rv ts)
	(per-block-total-set! rv 0)
	(per-block-counts-set! rv (make-hashtable))
	(per-block-times-set! rv (make-hashtable))
	(for-each
	  (lambda (x)
		(hashtable-put! (per-block-times rv) x 0)
		(hashtable-put! (per-block-counts rv) x 0)
		(for-each
		  (lambda (y)
			(hashtable-put! (per-block-counts rv) (string-append x y) 0))
		  '("start" "end")))
	  *log-types*)
	rv))

; Get the block for the given timestamp.
; Will create a block if there isn't one for the given timestamp.
(define (get-block ts)
  (let ((rv (hashtable-get *perblock* (real->string ts))))
	(if (not rv)
	  (begin
		(set! rv (init-per-block ts))
		(hashtable-put! *perblock* (real->string ts) rv)))
	rv))

; Dump a hashtable
(define (dump-hash h)
  (hashtable-for-each h (lambda (k v) (print (cons k v)))))

; Record a log entry (has no time, but is a start or end)
(define (record-log-entry block t)
  (let ((key
		  (string-append
			(log-entry-type t)
			(log-entry-state t))))
	(hashtable-update!
	  (per-block-counts block)
	  key
	  (lambda (old) (+ 1 old))
	  0)))

; Record a timing block (has a timespan)
(define (record-log-timing block t)
  (let ((key (log-timing-type t)))
	(hashtable-update!
	  (per-block-counts block)
	  key
	  (lambda (old) (+ 1 old))
	  0)
	(hashtable-update!
	  (per-block-times block)
	  key
	  (lambda (old) (+
					  (- (log-timing-stop t)
						 (log-timing-start t))
					  old))
	  0)))

; Record a timestamp or log entry (polymorphic, uses the above two routines)
(define (record-timing t)
  (cond
	((log-entry? t)
	 (record-log-entry
	   (get-block (log-entry-nearest-ts t)) t))
	((log-timing? t)
	 (record-log-timing
	   (get-block (log-timing-nearest-ts t)) t))
	(else (error "record-timing"
				 "Don't know what to do with this item"
				 t))))

; Print the block header (regular rrdtool stuff and the columns)
(define (print-block-header filename)
  (for-each display '("update " filename " -t "))
  (display (string-join ":"
						(flatten-list
						  (map (lambda (x)
								 (map (lambda (y)
										(string-append x y))
									  '("time" "count" "start" "end")))
							   *log-types*)))))

; Print an individual block
(define (print-block block filename)
  (print-block-header filename)
  (display " ")
  (display (per-block-ts block))
  (display ":")
  (display
	(string-join
	  ":"
	  (flatten-list
		(map (lambda (x)
			   (list
				 (number->string (hashtable-get
				   (per-block-times block) x))
				 (number->string (hashtable-get
				   (per-block-counts block) x))
				 (map (lambda (y)
						(number->string
						  (hashtable-get
							(per-block-counts block) (string-append x y))))
						'("start" "end"))))
		*log-types*))))
  (newline))

; ----------------------------------------------------------------------
; Main Routines
; ----------------------------------------------------------------------

; Collect a log entry.
;
; For each entry, figure out the state.  If it's a start, record it in the
; event cache as a start.
;
; If it's an end, try to find the start to calculate the transaction time,
; and then remove the start from the event cache.
(define (collect le)
  (record-timing le)
  (let ((key (string-append
			   (log-entry-serial le)
			   " . "
			   (log-entry-type le))))
	(if (string=? (log-entry-state le) "start")
	  ; Update the hashtable, checks for duplicates
	  (hashtable-update!
		*eventcache*
		key
		(lambda (old)
		  (warning (string-append
					 "Duplicate start for " (log-entry-serial le)))
		  le)
		le)
	  (let ((start (hashtable-get *eventcache* key)))
		(if start
		  (begin
			(record-timing (get-timing start le))
			(hashtable-remove! *eventcache* key))
		  (warning (list "No start for end " key)))))))


; Dump out the results.
;
; Get the sorted list of collected blocks and print each block.
(define (print-output filename)
  (for-each
	(lambda (block) (print-block block filename))
	; Get the sorted list of blocks from the hashtable
	(sort (hashtable->list *perblock*)
		  ; How to sort blocks
		  (lambda (x y)
			(< (per-block-ts x)
			   (per-block-ts y))))))

; Process the file
; First, collect all of the log entries from lines that contain
; TransactionTiming (see collect).
; Second, print out the collected lines.
(define (process-to-rrd filename)
  (conditional-input-loop
	(lambda (line) (strstr line "TransactionTiming" 40))
	(lambda (line)
	  (let ((le (parse-log-entry line)))
		(collect le))))
  (print-output filename))


(define (main args)
  (if (< (length args) 2)
	(error "main"
		   (string-append "Usage:  " (car args) " filename.rrd")
		   args))
  (process-to-rrd (cadr args)))
