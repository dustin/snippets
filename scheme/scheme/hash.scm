; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: hash.scm,v 1.1 2003/01/02 08:27:12 dustin Exp $

; Generic hash table thing

(module hash)

; Crappy routine to compute a hash value for a string
(define (hash-string s)
  (let ((rv 0))
	(for-each
	  (lambda (c)
		(set! rv (abs (+ (* rv 31) (char->integer c)))))
	  (string->list s))
	rv))

; Hash an object
(define (hash o)
  (cond
	((string? o)
	 (hash-string o))
	((integer? o)
	 o)
	(else
	  (error "hash" "unhashable object" o))))

; Make my hashtbl
(define (make-hashtbl . args)
  (let ((size (if (null? args)
			  167
			  (car args))))
	(if (not (integer? size))
	  (error "make-hashtbl" "size isn't a number" size))
	(cons 'hashtbl
		  (make-vector size '()))))

; Is this thing a hashtbl?
(define (hashtbl? o)
  (and (pair? o) (eq? 'hashtbl (car o))))

(define (hashtbl-put! ht k v)
  (if (not (hashtbl? ht))
	(error "hashtbl-put!" "That's not a hashtbl" ht))

  (let* ((bucket-id (modulo (hash k) (vector-length (cdr ht)))))
	(vector-set! (cdr ht) bucket-id
				 (append (vector-ref (cdr ht) bucket-id)
						 (list (cons k v))))))

; Get the key/value pair for the given key (or #f)
(define (hashtbl-get-pair ht k)
  (if (not (hashtbl? ht))
	(error "hashtbl-get" "That's not a hashtbl" ht))

  (let* ((bucket-id (modulo (hash k) (vector-length (cdr ht))))
		 (bucket (vector-ref (cdr ht) bucket-id)))
	(assoc k bucket)))

; Get the value for the given key from the given hashtbl (or null)
(define (hashtbl-get ht k)
  (let ((rv (hashtbl-get-pair ht k)))
	(if rv
	  (cdr rv)
	  rv)))

; Update a hash value.
;  If the value exists for the given key, proc is called with the old value
;  If the value does not exist, the value at init is stored
(define (hashtbl-update! ht k proc init)
  (let ((pos (hashtbl-get-pair ht k)))
	(if pos
	  (set-cdr! pos (proc (cdr pos)))
	  (hashtbl-put! ht k init))))

; Remove a key from a hashtable
(define (hashtbl-remove! ht k)
  (let* ((bucket-id (modulo (hash k) (vector-length (cdr ht)))))
	(vector-set! (cdr ht) bucket-id
				 (filter (lambda (x)
						   (not (equal? (car x) k)))
						 (vector-ref (cdr ht) bucket-id)))))

