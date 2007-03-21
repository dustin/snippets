; Date calculation stuff.
; Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
; $Id: dates.scm,v 1.3 2003/05/02 20:41:15 dustin Exp $

(module dates
		(export dates-seconds-for-time dates-decode))

;
; Constants used to perform calculations
;
; "The beginning of time!  1/1/1970"
(define dates-1970 0)
; "Second per normal year"
(define dates-seconds-per-year 31536000.0)
; "Average seconds per year"
(define dates-avg-sec-per-year 31557600.0)
; "Seconds per day (duh)"
(define dates-seconds-per-day 86400.0)
; "Average number of seconds per month."
(define dates-avg-sec-per-month (* dates-seconds-per-day (/ 365.0 12.0)))
; "Number of days in each month"
(define dates-days-by-month '(31 28 31 30 31 30 31 31 30 31 30 31))
; "Number of days in each month on a leap year."
(define dates-days-by-month-ly '(31 29 31 30 31 30 31 31 30 31 30 31))

; "The cumulative number of days by month in a normal year"
(define dates-monthday '(0 31 59 90 120 151 181 212 243 273 304 334))
; "The cumulative number of days by month in a leap year"
(define dates-monthday-ly '(0 31 60 91 121 152 182 213 244 274 305 335))

;
; The functions themselves
;

; "Is this year a leap year?"
(define (dates-leap-year-p year)
  (= 0 (modulo
		 (if (fixnum? year)
		   year
		   (flonum->fixnum year)) 4)))

; "Get the number of seconds at the beginning of the given year."
; Subtract one day from the ceiling of the number of years past 1970
; divided by four.  This allows us to calculate the number of leap days
; that have occurred
(define (dates-seconds-for-year year)
  (+ dates-1970
     (* dates-seconds-per-year (- year 1970))
     (* dates-seconds-per-day
		(- (ceiling (/ (- year 1970) 4.0))
		   (if (> year 1970) 1 0)))))

; "Get the number of seconds at the beginning of the month in the given year"
(define (dates-seconds-for-month year month)
  (+ (dates-seconds-for-year year)
     (* dates-seconds-per-day
	 	(list-ref
			(if (dates-leap-year-p year)
				dates-monthday-ly
				dates-monthday)
			(- month 1)))))

; "Get the number of seconds at the beginning of the given day."
(define (dates-seconds-for-day year month day)
  (+ (dates-seconds-for-month year month)
     (* dates-seconds-per-day (- day 1))))

; "Get the number of seconds at the beginning of the given hour."
(define (dates-seconds-for-hour year month day hour)
  (+ (dates-seconds-for-day year month day)
     (* 3600 hour)))

; "Get the number of seconds at the beginning of the given minute."
(define (dates-seconds-for-minute year month day hour minute)
  (+ (dates-seconds-for-hour year month day hour)
     (* 60 minute)))

; "Get the epoch time for the given time."
(define (dates-seconds-for-time year month day hour minute second)
  (+ (dates-seconds-for-minute year month day hour minute)
     second))

; "Get the number of seconds by month for the given year."
(define (dates-monsec year)
  (map (lambda (x) (* x dates-seconds-per-day))
	   (if (dates-leap-year-p year)
		 dates-monthday-ly
		 dates-monthday)))

; Get the position of the largest number in the given list where the element
; at the position is less than or equal to n.
(define (dates-lte-n l n)
  	(do ((x 0 (+ x 1)))
		 ((or (> (list-ref l x) n) (> x (length l)))
		  (- x 1))))

; "Decode the number of seconds (ts) into a time array."
(define (dates-decode ts)
  (let ((y 0.0) (m 0.0) (d 0.0) (h 0.0) (mn 0.0) (s ts))
    (set! y (+ 1970 (floor (/ s dates-avg-sec-per-year))))
    (set! s (- s (dates-seconds-for-year y)))
    (set! m (dates-lte-n (dates-monsec y) s))
    (set! s (- s (list-ref (dates-monsec y) m)))
    (set! d (floor (/ s dates-seconds-per-day)))
    (set! s (- s (* d dates-seconds-per-day)))
    (set! h (floor (/ s 3600)))
    (set! s (- s (* h 3600)))
    (set! mn (floor (/ s 60)))
    (set! s (- s (* mn 60)))
	; Make sure the return value contains only integers
	(map (lambda (x)
		   (if (flonum? x)
			 (flonum->fixnum x)
			 x))
		 (list y (+ 1 m) (+ 1 d) h mn s))))

; Tests

; (dates-seconds-for-time 2001 11 16 23 38 33)
; (dates-decode 1005953913.0)
; (dates-decode (dates-seconds-for-time 2001 1 1 0 0 0))

; (dates-seconds-for-year 1977)
; (dates-seconds-for-month 1977 10)
; (dates-seconds-for-day 1977 10 5)
; (dates-seconds-for-hour 1977 10 5 4)
; (dates-seconds-for-minute 1977 10 5 4 30)
; (dates-seconds-for-time 1977 10 5 4 30 15)
