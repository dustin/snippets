;; Humanize functions for elisp

;; (defvar human-i-magnitudes (mapcar (lambda (i) (expt 2 (* 10 i))) '(0 1 2 3 4 5 6)))

(defvar human-i-byte-labels '("b" "KiB" "MiB" "GiB" "TiB" "PiB" "EiB"))
(defvar human-byte-labels '("b" "KB" "MB" "GB" "TB" "PB" "EB"))

(defun humanize-bytes (b &optional base labels)
  "Humanize bytes in the given base with the given labels.

If no base or labels are specified, 1000/human-byte-labels are
used.  Another valid combination is 1024/human-i-byte-labels."
  (let ((base (if base base 1000)) (labels (if labels labels human-byte-labels)))
    (if (< b 10)
        (format "%dB", b)
      (let* ((e (floor (log b base)))
             (suffix (nth e labels))
             (val (floor (/ (+ 0.5 (* 10 (/ b (expt base e)))) 10)))
             (f (if (< val 10) "%.1f%s" "%.0f%s")))
        (format f val suffix)))))
