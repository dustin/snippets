; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: rtLogBackfill.scm,v 1.1 2003/04/14 20:52:47 dustin Exp $

; Input:
;
; 1048752554.35 BOOTSTRAP=3 DOWNLOAD_COMPLETE=3 HEARTBEAT_BOOT=15 HEARTBEAT_NEWIP=79 HEARTBEAT_PERIODIC=413 HEARTBEAT_SCHEDULED=19 KICKED=5 REQUEST_DOWNLOAD=2 
;
; Output:
;
; update thing -t BOOTSTRAP:DOWNLOAD_COMPLETE:HEARTBEAT_BOOT:HEARTBEAT_NEWIP:HEARTBEAT_PERIODIC:HEARTBEAT_SCHEDULED:KICKED:REQUEST_DOWNLOAD 1048752554.35:3:3:15:79:413:19:5:2


(module parse-timing-log
	(import
	  (misclib "../misclib.scm")
	  (stringlib "../stringlib.scm"))
	(main main))

; Extract a list of KEY=VALUEs into ks and vs
(define (extract-pairs src ks vs)
  (if (null? src)
	(cons ks vs)
	(let* ((s (string-split (car src) #\= 2)) (k (car s)) (v (cadr s)))
	  (extract-pairs (cdr src)
					 (append ks (cons k '()))
					 (append vs (cons v '()))))))

; Print the kv pairs as rrd input
(define (print-backfill l filename)
  (let ((pairs (extract-pairs (cdr l) '() '())))
	(print "update " filename " -t "
		   (string-join ":" (car pairs))
		   " " (car l) ":"
		   (string-join ":" (cdr pairs)))))

; Process input
; Split each line into its parts, then call the print function
(define (process-to-rrd filename)
  (input-loop
	(lambda (line)
	  (print-backfill
			(string-split line #\space 64) filename))))

(define (main args)
  (if (< (length args) 2)
	(error "main"
		   (string-append "Usage:  " (car args) " filename.rrd")
		   args))
  (process-to-rrd (cadr args)))
