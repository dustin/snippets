; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: amb.scm,v 1.1 2003/01/02 08:45:07 dustin Exp $

; McCarthy's ambiguous operator
(module amb (main main))

; Error handler
(define amb-fail '*) 
 
(define initialize-amb-fail
  (lambda ()
	(set! amb-fail
	  (lambda ()
		(error "amb-fail" "amb tree exhausted" ""))))) 
 
(initialize-amb-fail)

(define-macro
  amb
  (lambda alts...
	`(let ((+prev-amb-fail amb-fail))
	   (call/cc (lambda (+sk)
				  ,@(map (lambda (alt)
						   `(call/cc
							  (lambda (+fk)
								(set! amb-fail
								  (lambda ()
									(set! amb-fail +prev-amb-fail)
									(+fk 'fail)))
								(+sk ,alt))))
						 alts...)
				  (+prev-amb-fail)))))) 
(define (main args)
	)

