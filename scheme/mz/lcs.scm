; arch-tag: 378C1FBE-1DAD-11D9-97FD-000A957659CC
(load "utils.scm")

(define (vec2-set! v i j to)
  ; (vdisplay "Setting l[" i "][" j "] to " to)(newline)
  (vector-set! (vector-ref v i) j to))

(define (vec2-ref v i j)
  ; (vdisplay "Getting [" i "][" j "]")(newline)
  (vector-ref (vector-ref v i) j))

; Generic LCS matrix calculator.
;  a - first container
;  b - second container
;  sub - function to get an item from the container
;  len - function to get the length of a container
;
;  returns a vector of vectors
(define (commonSubseqs a b sub len)
  (let* ((m (len a)) (n (len b))
        (l (make-vector (+ 1 m) '())))
    (for-each (lambda (i) (vector-set! l i (make-vector (+ 1 n) 0)))
                (make-range 0 m))
    (for-each (lambda (i)
                (for-each (lambda (j)
                            (cond ((or (> i m) (> j n))
                                   (vec2-set! l i j 0))
                                  ((eq? (sub a i) (sub b j))
                                   (vec2-set! l i j
                                              (+ 1 (vec2-ref l (+ 1 i) (+ 1 j)))))
                                  (else (vec2-set! l i j
                                                   (max (vec2-ref l (+ 1 i) j)
                                                        (vec2-ref l i (+ 1 j)))))))
                          (reverse (make-range 0 (- n 1)))))
              (reverse (make-range 0 (- m 1))))
    l))

; Return a list of the common subsequence between two containers
;  a - first container
;  b - second container
;  sub - function to get an item from the container
;  len - function to get the length of a container
(define (lcs a b sub len)
  (let* (
         (l (commonSubseqs a b sub len))
         (m (vector-length l))
         (n (vector-length (vector-ref l 0))))
    (letrec ((loop (lambda (rv i j)
                     (if (or (>= i (- m 1)) (>= j (- n 1)))
                         (reverse rv)
                         (cond
                           ((eq? (sub a i) (sub b j))
                            (loop (cons (sub a i) rv) (+ 1 i) (+ 1 j)))
                           ((>= (vec2-ref l (+ 1 i) j) (vec2-ref l 1 (+ 1 j)))
                            (loop rv (+ 1 i) j))
                           (else
                            (loop rv i (+ 1 j))))))))
      (loop '() 0 0))))
