; Analyze db load test logs

; The sum of the parts
(define
	sum
	(lambda (l)
		(let ((x 0))
			(for-each ( lambda(v) (set! x (+ x v ))) l)
			x)))

; Average value in a list
(define
	avg
	(lambda (l)
		(/ (sum l) (length l))))

; Make a number array out of the elements in a string
(define
	string->nlist
	(lambda (s)
		(let ( (sa (string->list s)) (spaces '() ) (slist '() ) (l 0) (r 1) )
			(set! r (length sa))
			; Find the spaces
			(do
				( (i 0 (+ i 1)))
				( (>= i r))
				(cond
					((char=? #\space (string-ref s i))
						(set! spaces (append spaces (list i))))
				))
			; Pull out the parts
			(for-each (lambda (n)
					(set! slist (append slist (list (substring s l n))))
					(set! l (+ n 1))
				) spaces)
			(set! slist (append slist (list (substring s l r))))
			; Turn them into numbers
			(map (lambda(x) (string->number x)) slist))))

; Append a character to a string
(define
	string-append-char
		(lambda (s c)
			(list->string (append (string->list s) (list c )))))

; Read a line from a file
(define
	read-line
		(lambda (file)
			(let ((line "") (current-char #\a ))
				(do
					( (i 0 (+ i 1)))

					((or
						(eof-object? current-char)
						(char=? current-char #\newline)) line)

					(set! current-char (read-char file))
					(cond
						((eof-object? current-char) #f)
						((not (char=? current-char #\newline))
							(set! line
								(string-append-char line current-char))))))))

; Get the afterquery column out of the line
(define
	afterQuery
		(lambda (line)
			(let ( (l (string->nlist line)))
				 (list-ref l 6))))

; Show the results from the analysis
(define
	showResults
		(lambda (aq)
				(avg aq)))


; Process a file
(define
	process-file
		(lambda (filename)
			(let ( (aq '() ) (line "# crap!") )
				(define in (open-input-file filename))
				(do
					( (i 0 (+ i 1)))
					((= 0 (string-length line)) (showResults aq))
					(set! line (read-line in))
					(cond
						((char=? #\# (string-ref line 0)) #f)
						( else
							(set! aq (append aq (list (afterQuery line))))))))))
