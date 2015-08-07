; failed-to-list.el
;
; Function to convert a new fails file from tc into suite list files.
; 
; Usage:
;  invoke with the fail file in a buffer loaded from the destination
;  directory.
(defun failed-to-list ()
  (interactive)
  (beginning-of-buffer)
  (let (start suite end-suite test-name tests my-cursor)
    (catch 'done
      (while t
	(cond
	 ((search-forward "====" nil t)
	  )
	 (t
	  ;(prompt-for-insert "b1")
	  (throw 'done t))
	 )
	;(prompt-for-insert "b2")
	(beginning-of-line)
	(previous-line 1)
	(setq start (point))
	(end-of-line)
	(setq suite (buffer-substring start (point)))
	(beginning-of-line)
	(next-line 2)
	(setq start (point))
	(cond
	 ((search-forward "====" nil t)
	  (beginning-of-line)
	  (previous-line 2)
	  ;(prompt-for-insert "b3")
	  )
	 (t
	  (end-of-buffer)
	  ;(prompt-for-insert "b4")
	  )
	 )
	(setq end-suite (point))
	(goto-char start)

	;(prompt-for-insert "b5")
	(while (not (= (point) end-suite))
	  ;(prompt-for-insert "b6")
	  (back-to-indentation)
	  (setq start (point))
	  (lforward-word 1)
	  (setq test-name (buffer-substring start (point)))
	  (push test-name tests)
	  (beginning-of-line)
	  (next-line 1)
	  ;(prompt-for-insert "b7")
	  )
	(save-excursion
	  (setq tests (reverse tests))
	  (find-file (concat suite ".lst"))
	  ;(prompt-for-insert "b8")
	  (erase-buffer)
	  (while tests
	    (insert (concat (car tests) "\n"))
	    (setq tests (cdr tests))
	    ;(prompt-for-insert "b9")
	    )
	  (save-buffer)
	  ;(prompt-for-insert "b10")
	  )
	;(prompt-for-insert "b11")
	)
      )
    )
  )