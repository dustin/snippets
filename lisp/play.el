; $Id: play.el,v 1.2 2001/11/14 10:12:22 dustin Exp $

; (defun dustin-for-each (p l) "Dustin's for-each test."
;   (if l
;       (progn
; 	(funcall p (car l))
; 	(dustin-for-each p (cdr l))))
;   'Done)


; (if '() 'true 'false)

; (dustin-for-each 'print '(1 2 3))

; (symbol-function 'print)

; recursive
; (defun pow (x y) "Calculate x^y"
;   (if (< y 1)
;       1
;     (* x (pow x (- y 1)))))

; Iterative
(defun pow (x y) "Calculate x^y"
  (let ((rv 1.0))
    (loop for i from 1 to y do
	  (setq rv (* rv x)))
    rv))

(pow 2.0 512)



