; Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
;
; $Id: quicksort.scm,v 1.1 2003/04/13 08:03:46 dustin Exp $

(module qsort (main main))

; quick sort (with optional compare)
(define (qsort l . opcomp)
  ; this is all argument handling and
  ; stuff
  (if (not (pair? l))
	l
    ; If there wasn't a comparison operation passed in, use >
    (let ((comp (if (pair? opcomp) (car opcomp) >)))
      ; If the argument isn't a function, use >
      (if (not (procedure? comp))
	    (set! comp  >))
      ; begin actual quicksort
      (append
        (qsort (filter (lambda (x) (comp (car l) x)) (cdr l)) comp)
	    (cons (car l) '())
	    (qsort (filter (lambda (x) (not (comp (car l) x))) (cdr l)) comp)))))

