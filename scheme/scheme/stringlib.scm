; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: stringlib.scm,v 1.1 2002/12/27 23:24:52 dustin Exp $

(module stringlib
		(export
		  string-split-chars
		  string-split))

; Skip characters in s that contain one of the letters in l starting at
; position i
;
; Example:
; (skip-chars "   blah" '(#\space) 0) -> 3
(define (skip-chars s l i)
  (if (>= i (string-length s))
	(string-length s)
	(if (memq (string-ref s i) l)
	  (skip-chars s l (+ 1 i))
	  i)))

; Find the index of any of the given characters in the given string
; Example:
; (string-index-of-one "this is a test" '(#\space) 0) -> 4
(define (string-index-of-one s l i)
  (if (< i (string-length s))
	(if (memq (string-ref s i) l)
	  i
	  (string-index-of-one s l (+ 1 i)))
	-1))

; Recursively split a string on a set of characters
; Example:
; (string-split-chars-rec '() "this is a test" '(#\space) 0 2)
;	-> '("this" "is a test")
(define (string-split-chars-rec rv str l i limit)
  (if (and
		(< (length rv) (- limit 1))
		(< i (string-length str)))
	(begin
	  (let ((pos (string-index-of-one str l i)))
		(if (not (= pos -1))
		  (string-split-chars-rec
			(append rv (list (substring str i pos)))
			str l (skip-chars str l pos) limit)
		  (append rv (list (substring str i (string-length str)))))))
	(if (< i (string-length str))
	  (append rv (list (substring str i (string-length str))))
	  rv)))

; Split a string on the given set of characters
; Example:
; (string-split-chars "this is a test" '(#\space) 2) -> '("this" "is a test")
(define (string-split-chars s l limit)
  (string-split-chars-rec '() s l 0 limit))

; Split a string on a single character
(define (string-split s c limit)
  (string-split-chars s (list c) limit))
