; Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
;
; $Id: hash.scm,v 1.3 2003/05/02 20:41:16 dustin Exp $

; Generic hash table thing

(module hash
		(export
		  hash
		  make-hashtbl
		  hashtbl?
		  hashtbl-put!
		  hashtbl-get-pair
		  hashtbl-get
		  hashtbl-update!
		  hashtbl-remove!
		  hashtbl-keys-values
		  hashtbl-keys
		  hashtbl-values))

; Crappy routine to compute a hash value for a string
(define (hash-string s)
  (abs (let loop ((l (string->list s)))
	(if (null? l)
	  0
	  (+ (char->integer (car l))
			  (* 31 (loop (cdr l))))))))

; Old string hasher.  Not compatible with the new one, but similar...I'm
; keeping this around because lispme doesn't support that let loop construct
(define (old-hash-string s)
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

; Verify a hashtbl is a hashtbl
(define (hashtbl-assert-ht ht whence)
  (if (not (hashtbl? ht))
	(error whence "That's not a hashtbl" ht)))

(define (hashtbl-put! ht k v)
  (hashtbl-assert-ht ht "hashtbl-put!")

  (let* ((bucket-id (modulo (hash k) (vector-length (cdr ht)))))
	(vector-set! (cdr ht) bucket-id
				 (append (vector-ref (cdr ht) bucket-id)
						 (list (cons k v))))))

; Get the key/value pair for the given key (or #f)
(define (hashtbl-get-pair ht k)
  (hashtbl-assert-ht ht "hashtbl-get-pair")

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

; Get all keys and values from the given hashtable
(define (hashtbl-keys-values ht)
  (hashtbl-assert-ht ht "hashtbl-keys-values")
  (apply append (vector->list (cdr ht))))

; Get the keys from the given hashtable
(define (hashtbl-keys ht)
  (map car (hashtbl-keys-values ht)))

; Get the keys from the given hashtable
(define (hashtbl-values ht)
  (map cdr (hashtbl-keys-values ht)))
