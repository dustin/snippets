; An attempt at a language generator in lisp

(setq place '(rlist "Santa Clara" "the USA" "California"))
(setq what '(ilist (rlist "baddest" "swankest") " mofo"))
(setq statement '(rlist
		    (ilist "I'm the " what " in all of " place)))

(defun language-expand (w) "lisp implementation of a dada engine."
  (cond
   ( (symbolp w)
     (language-expand (symbol-value w)))
   ( (stringp w) w)
   ( (listp w)
     (if (equal (car w) 'ilist)
	 (apply 'concat (map 'list 'language-expand (cdr w)))
       (language-expand (nth (random (length (cdr w))) (cdr w)))))))

; (language-expand 'statement)
