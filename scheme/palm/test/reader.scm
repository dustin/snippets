; Data reader
(define dbname "Test")
(define start 1)

(define (ldisplay l)
 (map (lambda (x) (display x)(display " ")) l))

; This is for testing in scheme48
; (define (hb-addrecord name)
;   (set! start (+ 1 start))start)

; (define (hb-setfield db rec fld obj)
;  (ldisplay (list "Setting" rec fld obj))(newline))

(define (add-record record)
  (let ( (row (hb-addrecord dbname)) )
   (do ((i 0 (+ i 1)))
    ( (>= i (length record)) #t)
	(hb-setfield dbname row i (list-ref record i)))))

(define (readstuff file)
  (let ( (data '()))
   (define in (open-input-file file))
   (set! data (read in))
   (for-each (lambda (x) (add-record x)) data)))
