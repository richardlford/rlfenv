(defun zap-char ()
  "Delete the current character, saving it in ZAPPED-CHAR"
  (interactive)
       (setq ZAPPED-CHAR (following-char))
       (delete-char 1)
)

(defun zap-word ()
  "Deletes to end of current word, saving it it ZAPPED-WORD"
  (interactive)
  (let ( (start (point))
	 (end (save-excursion
		(forward-word 1)
		(point))))
    (setq ZAPPED-WORD (buffer-substring start end))
    (delete-region start end)
))

(defun zap-line ()
  "Deletes current line, saving it it ZAPPED-LINE"
  (interactive)
  (let ( (start (save-excursion
		  (beginning-of-line)
		  (point)))
	 (end (save-excursion
		(forward-line)
		(point))))
       (setq ZAPPED-LINE (buffer-substring start end))
       (delete-region start end)
))

(defun unzap-char ()
  "Inserts ZAPPED-CHAR at point"
  (interactive)
  (insert ZAPPED-CHAR)
  (backward-char)
)

(defun unzap-word ()
  "Inserts ZAPPED-WORD at point"
  (interactive)
  (insert ZAPPED-WORD)
)

(defun unzap-line ()
  "Inserts ZAPPED-LINE at point"
  (interactive)
  (insert ZAPPED-LINE)
  (forward-line -1)
)
