; Display more than one object to stdout
(define (vdisplay . args)
  (for-each display args))

; Prefix a string (s) with a given string (p)
; until the requested length (l) is met
(define (prefix-string s p l)
      (if (> l (string-length s))
          (prefix-string (string-append p s) p l)
      s))

; Function currying (partially applied function)
(define-macro curry
  (lambda (f . args)
    (let ((name (gensym)))
    `(lambda (,name) (,f ,@args ,name)))))

; Make a range of numbers
(define (make-range a b)
  (letrec ((loop (lambda (rv v)
                   (if (< v a) rv
                       (loop (cons v rv) (- v 1))))))
    (loop '() b)))

; partition a list into two lists based on the result of a function
(define (partition f lin)
  (letrec ((loop (lambda (l y n)
                   (if (null? l)
                       (list (reverse y) (reverse n))
                       (if (f (car l))
                           (loop (cdr l) (cons (car l) y) n)
                           (loop (cdr l) y (cons (car l) n)))))))
    (loop lin '() '())))

; Fold a list over a function
(define (fold f i l)
  (if (null? l) i
      (fold f (f i (car l)) (cdr l))))

; Fold a string over a function
(define (fold-string f i s)
  (fold f i (string->list s)))

; Map with an index
(define (mapi f lin)
  (reverse
   (letrec ((loop (lambda (rv i l)
                    (if (null? l) rv
                        (loop (cons (f i (car l)) rv) (+ i 1) (cdr l))))))
     (loop '() 0 lin))))

; My quicksort implementation
(define (qsort lin . opcomp)
  (let ((comp (if (pair? opcomp)
                  (car opcomp) >)))
    (if (and
         (not (procedure? comp)))
        (set! comp >))
    ; Begin actual quicksort
    (letrec ((qs (lambda (l)
                   (if (not (pair? l)) l
                       (let ((p (partition (lambda (x)
                                             (comp (car l) x)) (cdr l))))
                         (append (qs (car p))
                                 (cons (car l) '())
                                 (qs (cadr p))))))))
      (qs lin))))
