; View views from my money database.
; $Id: moneyviews.el,v 1.1 2001/12/13 09:58:23 dustin Exp $

(require 'pg)

(defun get-money-data (v) "Get the data for the given view."
  (with-pg-connection con ("money" "dustin" "blah" "dhcp-104")
		      (let ((res (pg:exec con "select * from " v)))
			(list (pg:result res :attributes)
			      (pg:result res :tuples)))))

(defun shove-data-in (b s) "Shove data in a given buffer."
  (save-excursion
    (if (not (get-buffer b))
	(generate-new-buffer b))
    (set-buffer b)
    (insert s)))

(defun show-data (d) "Show the data from the postgres result."
  
(shove-data-in "*results*" (get-money-data "show_account_values"))
