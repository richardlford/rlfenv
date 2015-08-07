(defvar check-max-length-hook-limit
   80 "The limit to check against in a buffer")

(defun max-line-length ()
  (interactive "")
  (save-excursion
    (widen)
    (beginning-of-buffer)
    (let ((max-len 0) n2 n3)
      (while (not (eobp))
	(setq n2 (point))
	(end-of-line)
	(setq n3 (- (point) n2))
	(if (> n3 max-len)
	    (setq max-len n3))
	(if (not (eobp))
	    (forward-char))
	)
      ;(prompt-for-insert "b1")
      max-len
      )))

(defun check-max-length-hook ()
  (interactive "")
  (let ((mll (max-line-length)))
    (cond
     ((>  mll check-max-length-hook-limit)
      (beginning-of-buffer)
      (next-overlength-line)
      (recursive-edit)
      ))))

(defun next-overlength-line ()
  "Move to the next line that is overlength"
  (interactive "")
  (let (n2 n3)
    (catch 'done
      (while (not (eobp))
	(setq n2 (point))
	(end-of-line)
	(setq n3 (- (point) n2))
	(cond
	 ((> n3 check-max-length-hook-limit)
	  (message "over-length (%s chars) line found" n3)
	  (beginning-of-line)
	  (forward-char check-max-length-hook-limit)
	  (throw 'done t)
	  ))
	(if (not (eobp))
	    (forward-char))
	)
      (message "No over-length line found")
      )))
