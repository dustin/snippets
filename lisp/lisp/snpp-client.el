; SNPP Implementation in emacs LISP

; need common lisp stuff
(require 'cl)

(defvar snpp-responses '() "SNPP Responses go here.")
(defvar snpp-last-response '() "The most recent SNPP response.")
(defvar snpp-process 'nil "The actual SNPP connection.")

(defvar snpp-server 'nil "The SNPP server to which we'll be connecting.")
(defvar snpp-port 444 "The SNPP port on the server to which we'll connect.")

(defun snpp-read-filter (proc string) "This function processes all input."
  (loop for i in (split-string string "[\r\n]") do
	(setq snpp-responses
	      (append snpp-responses
		      (list (string-to-int (substring i 0 3))
			    (substring i 4))))))

(defun snpp-next-response () "Get the next response from the snpp-responses"
  (if snpp-responses
      (let ( (rv (list (car snpp-responses) (cadr snpp-responses))) )
	(setq snpp-responses (cddr snpp-responses))
	rv)
    (throw 'snpp-no-responses "There are no pending responses.")))

(defun snpp-check-status ()
  "Find out if the latest response indicated success."
  (progn
    (setq snpp-last-response (snpp-next-response))
    (if (or (< (car snpp-last-response) 200)
	     (>= (car snpp-last-response) 300))
	(throw 'snpp-error-response (cadr snpp-last-response))
      (cadr snpp-last-response))))

(defun snpp-init (hostname port)
  "Get an SNPP Connection to the given host and port"
  (progn
    (snpp-close)
    (setq snpp-process (open-network-stream "snpp" 'nil hostname port))
    (set-process-filter snpp-process 'snpp-read-filter)
    (accept-process-output snpp-process 5)
    (snpp-check-status)))

(defun snpp-cmd (string) "Send the given command to the snpp server."
  (progn
    (process-send-string snpp-process (concat string "\n"))
    (accept-process-output snpp-process 1)
    (snpp-check-status)))

(defun snpp-close () "Close and clean up."
  (if snpp-process
      (progn
	(setq snpp-responses '()) ; Out with the old
	(condition-case nil
	    (snpp-cmd "quit")
	  (error nil)) ; Ignore all errors while trying to close
	(delete-process snpp-process)
	(setq snpp-process 'nil))))

(defun snpp-send-message (host port user message)
  "Deliver a message to a user via SNPP at the given SNPP server."
  (progn
    (snpp-init host port)
    (snpp-cmd (concat "page " user))
    (snpp-cmd (concat "message " message))
    (snpp-cmd "send")
    (snpp-close)))

(defun snpp-interactive () "Interactive SNPP client."
  (interactive
   (let ((user (read-string "SNPP ID:  " nil 'snpp-history))
	 (message (read-string "Message:  " nil nil)))
     (snpp-send-message snpp-server snpp-port user message))))

(provide 'snpp-client)

;; This is how you send stuff
;; All in one:
; (snpp-send-message "dhcp-104" 1041 "dustin" "This is a test.")

;; test stuff
; snpp-responses


