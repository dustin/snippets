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

;
; Utility stuff
;

; Vector item swap
(define (vswap v a b)
  (let ( (atmp (vector-ref v a)) (btmp (vector-ref v b) ) )
    (vector-set! v a btmp)(vector-set! v b atmp)))

; shuffle a list
(define (shuffle l)
  ; Convert the input to a vector so we can move stuff around
  (let ( (v (list->vector l)) (s (length l) ) )
    ; Perform one swap operation per list entry
    (do ( (i 0 (+ i 1)))
      ; If we're done, return a list version of our vector
      ( (>= i s) (vector->list v))
      ; Get a random offset in the list
      (let ( (r (random s)))
        ; swap'm
        (vswap v i r)))))

(define (gen-nlist from to)
 (let ( (thelist '()) )
  (do ( (i from (+ i 1)))
   ( (> i to) thelist)
   (set! thelist (append thelist (list i))))))

;
; Database stuff
;

; Get a record
(define (get-record which)
  (map (lambda (x) (hb-getfield testdb which x) ) (gen-nlist 0 6)))

; Show the correct answer (used in edit forms, etc...)
(define (set-select-button which)
  (map (lambda (x) (
	ctl-set-val x #f)) (list answer1-psh answer2-psh answer3-psh answer4-psh))
  (ctl-set-val (+ answer-psh-base which) #t))

; Find out how many records we have
(define (n-records)
 (car (hb-info testdb)))

; Get a random test
(define (random-test)
 (map (lambda (x) (get-record x) (gen-nlist 0 (n-records)))))

; Populate the textual fields in the current form
(define (populate-fields data)
	(fld-set-text question-fld (list-ref data 0 ))
	(fld-set-text answer1-fld (list-ref data 1 ))
	(fld-set-text answer2-fld (list-ref data 2 ))
	(fld-set-text answer3-fld (list-ref data 3 ))
	(fld-set-text answer4-fld (list-ref data 4 )))

; Run the actual test program
(define (test)
  (set-resdb "TestTaker")
  (let ( (testdata (random-test)) )
    ; Edit the first record
    (edit-record (list-ref testdata 0))))

; Edit a given record
(define (edit-record data)
  (frm-popup 1000
    (lambda (event . args)
      (case event
        ((menu) (frm-return 'bye))
		; When we open the form, populate it with the nth record from the DB
        ((frm-open)
			(populate-fields data)
			(set-select-button (string->object (list-ref data 5)))))
        (else #f))))
