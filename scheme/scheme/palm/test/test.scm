; TestTaker source

; Constants
(define testdb "Test")

; Form IDs, etc...
(define question-fld 1001)
; Answer fields
(define answer-fld-base 2000)
(define answer-psh-base 3000)
(define answer1-fld 2001)
(define answer1-psh 3001)
(define answer2-fld 2002)
(define answer2-psh 3002)
(define answer3-fld 2003)
(define answer3-psh 3003)
(define answer4-fld 2004)
(define answer4-psh 3004)
; Buttons
(define add-btn 4001)

; Get a record
(define (get-record which)
  (map (lambda (x) (hb-getfield testdb which x) ) '(0 1 2 3 4 5 6)))

(define (set-select-button which)
  (map (lambda (x) (
	ctl-set-val x #f)) (list answer1-psh answer2-psh answer3-psh answer4-psh))
  (ctl-set-val (+ answer-psh-base which) #t))

; Run the actual test program
(define (test)
  (set-resdb "TestTaker")
  (frm-popup 1000
    (lambda (event . args)
      (case event
        ((menu) (frm-return 'bye))
		; When we open the form, populate it with the first record from the DB
        ((frm-open)
          (let ( (data (get-record 0)) )
            (fld-set-text question-fld (list-ref data 0 ))
            (fld-set-text answer1-fld (list-ref data 1 ))
            (fld-set-text answer2-fld (list-ref data 2 ))
            (fld-set-text answer3-fld (list-ref data 3 ))
            (fld-set-text answer4-fld (list-ref data 4 ))
			(set-select-button (string->object (list-ref data 5)))))
        (else #f)))))
