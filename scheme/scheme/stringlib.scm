; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: stringlib.scm,v 1.2 2002/12/28 03:15:26 dustin Exp $

(module stringlib
		(export
		  string-split-chars
		  string-split
		  strstr
		  string-remove-chars))

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
; Return false if the character cannot be found.
; Example:
; (string-index-of-one "this is a test" '(#\space) 0) -> 4
(define (string-index-of-one s l i)
  (if (< i (string-length s))
	(if (memq (string-ref s i) l)
	  i
	  (string-index-of-one s l (+ 1 i)))
	#f))

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
		(if pos
		  (string-split-chars-rec
			(append rv (list (substring str i pos)))
			str l (skip-chars str l pos) limit)
		  (append rv (list (substring str i (string-length str)))))))
	(if (< i (string-length str))
	  (append rv (list (substring str i (string-length str))))
	  rv)))

; search for a needle in a haystack
; return false if the needle is not in the haystack
(define (strstr haystack needle offset)
  (if (> (+ offset (string-length needle)) (string-length haystack))
	#f
	(if (string=? needle (substring haystack offset
									(+ offset (string-length needle))))
	  offset
	  (strstr haystack needle (+ 1 offset)))))

; Remove any characters that appear in list l from string str
; Example
; (string-remove-chars "Blah blah blah  blah" '(#\space))
;    -> "Blahblahblahblah"
(define (string-remove-chars str l)
  (list->string
	(filter (lambda (c) (not (memq c l)))
			(string->list str))))

; Split a string on the given set of characters
; Example:
; (string-split-chars "this is a test" '(#\space) 2) -> '("this" "is a test")
(define (string-split-chars s l limit)
  (string-split-chars-rec '() s l 0 limit))

; Split a string on a single character
(define (string-split s c limit)
  (string-split-chars s (list c) limit))
