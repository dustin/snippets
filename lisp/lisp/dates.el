; Date calculation stuff.
; Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
; $Id: dates.el,v 1.3 2001/11/16 00:04:15 dustin Exp $

;
; Constants used to perform calculations
;
(defconst dates-1970 0 "The beginning of time!  1/1/1970")
(defconst dates-seconds-per-year 31536000 "Second per normal year")
(defconst dates-seconds-per-day 86400 "Seconds per day (duh)")
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

(provide 'dates)

; Tests

; (dates-seconds-for-year 1977)
; (dates-seconds-for-month 1977 10)
; (dates-seconds-for-day 1977 10 5)
; (dates-seconds-for-hour 1977 10 5 4)
; (dates-seconds-for-minute 1977 10 5 4 30)
; (dates-seconds-for-time 1977 10 5 4 30 15)

; (require 'cl)

; (let (( rv '(0)) (last 0))
;   (loop for i in dates-days-by-month-ly do
; 	(setq last (+ i last))
; 	(setq rv (append rv (list last))))
;   rv)

