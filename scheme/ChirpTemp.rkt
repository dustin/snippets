#lang racket

(require htdp/gui)
(require racket/format)

(define temp-output
  (make-message (string-append "Press chirp for each cricket chirp you hear." (make-string 20 #\space))))

(define chirps '())

(define (zip a b)
  (letrec ((loop (lambda (ca cb out)
                   (if (or (null? ca) (null? cb))
                       out
                       (loop (cdr ca) (cdr cb) (cons (list (car ca) (car cb)) out))))))
    (reverse (loop a b '()))))

(define (deltas l)
  (map (lambda (x) (- (car x) (cadr x))) (zip l (cdr l))))

(define (avg l)
  (if (null? l)
      0
      (/ (foldl + 0 l) (length l))))

(define (first-n n l)
  (letrec ((loop (lambda (n l out)
                 (if (or (= 0 n) (null? l))
                     (reverse out)
                     (loop (- n 1) (cdr l) (cons (car l) out))))))
    (loop n l '())))

(define (period-to-c p)
  (if (= 0 p)
      0
      (+ (/ 25000 p) 3)))

(define (c2f c)
  (+ (/ (* 9 c) 5) 32))

(define (chirpped e)
  (set! chirps (first-n 10 (cons (current-inexact-milliseconds) chirps)))
  (let* ((period (avg (deltas chirps)))
         (temp (period-to-c period)))
    ;; (display period) (display " ") (displayln temp)
    (if (> period 0)
        (draw-message temp-output (string-append
                                   (~r temp #:precision 2) "C / "
                                   (~r (c2f temp) #:precision 2) "F"))
        #t)))

(define w
  (create-window
   (list
    (list temp-output)
    (list (make-button "Chirp" chirpped)
          (make-button "Quit" (lambda (e) (hide-window w)))))))


(show-window w)