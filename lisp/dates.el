; Date calculation stuff.
; Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
; $Id: dates.el,v 1.4 2001/11/17 11:17:53 dustin Exp $

;
; Constants used to perform calculations
;
(defconst dates-1970 0 "The beginning of time!  1/1/1970")
(defconst dates-seconds-per-year 31536000.0 "Second per normal year")
(defconst dates-avg-sec-per-year 31557600.0 "Average seconds per year")
(defconst dates-seconds-per-day 86400.0 "Seconds per day (duh)")
(defconst dates-avg-sec-per-month (* dates-seconds-per-day (/ 365 12))
  "Average number of seconds per month.")
(defconst dates-days-by-month '(31 28 31 30 31 30 31 31 30 31 30 31)
  "Number of days in each month")
(defconst dates-days-by-month-ly '(31 29 31 30 31 30 31 31 30 31 30 31)
  "Number of days in each month on a leap year.")

(defconst dates-monthday '(0 31 59 90 120 151 181 212 243 273 304 334)
  "The cumulative number of days by month in a normal year")
(defconst dates-monthday-ly '(0 31 60 91 121 152 182 213 244 274 305 335)
  "The cumulative number of days by month in a leap year")

;
; The functions themselves
;

(defun dates-leap-year-p (year) "Is this year a leap year?"
  (= 0 (% year 4)))

(defun dates-seconds-for-year (year)
  "Get the number of seconds at the beginning of the given year."
  (+ dates-1970
     (* dates-seconds-per-year (float (- year 1970)))
     (* dates-seconds-per-day (ceiling (/ (- year 1970) 4.0)))))

(defun dates-seconds-for-month (year month)
  "Get the number of seconds at the beginning of the month in the given year"
  (+ (dates-seconds-for-year year)
     (* dates-seconds-per-day (float (nth (- month 1)
					  (if (dates-leap-year-p year)
					    dates-monthday-ly
					    dates-monthday))))))

(defun dates-seconds-for-day (year month day)
  "Get the number of seconds at the beginning of the given day."
  (+ (dates-seconds-for-month year month)
     (* dates-seconds-per-day (float (- day 1)))))

(defun dates-seconds-for-hour (year month day hour)
  "Get the number of seconds at the beginning of the given hour."
  (+ (dates-seconds-for-day year month day)
     (* 3600 hour)))

(defun dates-seconds-for-minute (year month day hour minute)
  "Get the number of seconds at the beginning of the given minute."
  (+ (dates-seconds-for-hour year month day hour)
     (* 60 minute)))

(defun dates-seconds-for-time (year month day hour minute second)
  "Get the epoch time for the given time."
  (+ (dates-seconds-for-minute year month day hour minute)
     second))

(defun dates-monsec (year)
  "Get the number of seconds by month for the given year."
  (map 'list (lambda (x) (* x dates-seconds-per-day))
       (if (dates-leap-year-p year)
	   dates-monthday-ly
	 dates-monthday)))

(defun dates-lte-n (l n)
  "Get the position of the largest number in the given list where the element
at the position is less than or equal to  n."
  (let ((rv 0))
    (loop for i from 0 to (- (length l) 1) do
	  (if (<= (nth i l) n)
	      (setq rv i)))
    rv))

(defun dates-decode (ts) "Decode the number of seconds (ts) into a time array."
  (let ((y 0.0) (m 0.0) (d 0.0) (h 0.0) (mn 0.0) (s ts))
    (setq y (+ 1970 (floor (/ s dates-avg-sec-per-year))))
    (setq s (- s (dates-seconds-for-year y)))
    (setq m (dates-lte-n (dates-monsec y) s))
    (setq s (- s (nth m (dates-monsec y))))
    (setq d (floor (/ s dates-seconds-per-day)))
    (setq s (- s (* d dates-seconds-per-day)))
    (setq h (floor (/ s 3600)))
    (setq s (- s (* h 3600)))
    (setq mn (floor (/ s 60)))
    (setq s (- s (* mn 60)))
    (list y (1+ m) (1+ d) h mn s)))

; (dates-seconds-for-time 2001 11 16 23 38 33)
; (dates-decode 1005953913.0)
; (dates-decode (dates-seconds-for-time 2001 1 1 0 0 0))

(provide 'dates)

; Tests

; (dates-seconds-for-year 1977)
; (dates-seconds-for-month 1977 10)
; (dates-seconds-for-day 1977 10 5)
; (dates-seconds-for-hour 1977 10 5 4)
; (dates-seconds-for-minute 1977 10 5 4 30)
; (dates-seconds-for-time 1977 10 5 4 30 15)
