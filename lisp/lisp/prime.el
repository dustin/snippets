; $Id: prime.el,v 1.1 2001/11/03 10:51:30 dustin Exp $

; Prime number generation in emacs lisp

(defvar primelist '() "Current list of known primes")
(setq primelist '(2 3 5 7 11 13 17))

(defun divisiblep (x y) "Return true if x is divisible by y"
  (zerop (% x y)))

(defun end-of-list (l) "Get the last element of a list"
  (nth (1- (safe-length l)) l))

(defun prime-by-listp (n s l)
  "Figure out if \"n\" is prime using the given list, not exceeding \"s\""
  (if l
      (if (< s (car l))
	  (setq primelist (append primelist (list n)))
	(if (divisiblep n (car l))
	    'nil
	  (prime-by-listp n s (cdr l))))))

(defun primep (n) "Is n prime?"
  (if (member n primelist)
      n
      (if (> (sqrt n) (end-of-list primelist))
	  (throw 'insufficientprimes "Prime list insufficient")
	(prime-by-listp n (sqrt n) primelist))))

(defun greater-values (n l)
  "Get all values in the list greater than the given number"
  (if l
      (if (> (car l) n)
	  l
	(greater-values n (cdr l)))))

(defun next-prime (x) "Get the next prime after \"n\""
  (if (> (end-of-list primelist) x)
      (car (greater-values x primelist))
    (progn
      (setq x
	    (cond
	     ( (< x 2) 2)
	     ( (divisiblep x 2) (+ 1 x))
	     ( (primep x) (+ x 2))
	     (t x)))
      (if (primep x)
	  x
	(progn
	  (loop for i from x by 2 until (primep i))
	  (car (greater-values x primelist)))))))

(defun build-next-prime () "Build the next prime"
  (next-prime (end-of-list primelist)))

(defun build-primes-to (x) "Build the prime list through \"x\""
  (loop until (> (end-of-list primelist) x) do
	(build-next-prime)))

; play with the stuff here

;; This is so lisp won't bitch
; (setq max-specpdl-size 8192)
; (setq max-lisp-eval-depth 8192)

; (build-primes-to 8192)
; (next-prime 101)
; (build-next-prime)
; (end-of-list primelist)
; (list-length primelist)
; primelist


