; This is sort of what's needed for Noelani's blog.

; Config
(defvar blog-template "~/tmp/template.shtml" "The new blog entry template.")
(defvar blog-outdir "~/tmp/" "The blog directory.")
(defvar blog-index "~/tmp/index.shtml" "The index file.")
(defvar blog-ddateformat "%Y/%m/%d %H:%M" "Visible timestamp format.")
(defvar blog-fndateformat "%Y%m%d%H%M" "Filename timestamp format.")

; Add an entry to an index.
(defun blog-add-index (ixpath newfile)
  "Add a reference to the new file in the given index."
  (let ((inf (find-file ixpath)))
    (switch-to-buffer inf)
    (goto-char (point-min))
    (search-forward "%LIVELIST%")
    (end-of-line)
    (insert "\n<!--#include virtual=\"" newfile "\"-->")
    (save-buffer inf)))

; Do the work
(defun blog-new-entry () "Make a new blog entry."
   (let* ((thedate (format-time-string blog-ddateformat))
	 (newfile (concat (format-time-string blog-fndateformat) ".shtml"))
	 (inf (find-file blog-index))
	 (nef (find-file (concat blog-outdir newfile))))
     ; Create the new file and fill in the template
     (switch-to-buffer nef)
     (insert-file-contents blog-template nil nil nil 1)
     (goto-char (point-min))
     (while (re-search-forward "%THEDATE%" nil t)
       (replace-match thedate nil nil))
     (save-buffer nef)
     ; Update the index
     (blog-add-index blog-index newfile)
     (switch-to-buffer nef)
     nil))

(defun blog-make-entry ()
  "Make a new blog entry."
  (interactive
   (blog-new-entry)))
