; This is sort of what's needed for Noelani's blog.

; Config
(setq template "~/tmp/template.shtml")
(setq outdir "~/tmp/")
(setq index "~/tmp/index.shtml")
(setq ddateformat "%Y/%m/%d %H:%M")
(setq fndateformat "%Y%m%d%H%M")

; Do the work
(defun make-blog-entry ()
  "Make a new blog entry."
  (interactive
   (let ((thedate (format-time-string ddateformat))
	 (newfile (concat (format-time-string fndateformat) ".shtml"))
	 (inf (find-file index))
	 (nef (find-file (concat outdir newfile))))
     ; Create the new file and fill in the template
     (switch-to-buffer nef)
     (insert-file-contents template nil nil nil 1)
     (goto-char (point-min))
     (while (re-search-forward "%THEDATE%" nil t)
       (replace-match thedate nil nil))
     (save-buffer nef)
     ; Update the index
     (switch-to-buffer inf)
     (goto-char (point-min))
     (search-forward "%LIVELIST%")
     (end-of-line)
     (insert "\n<!--#include virtual=\"" newfile "\"-->")
     (save-buffer inf)
     (switch-to-buffer nef)
     nil
     )))
