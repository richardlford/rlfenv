;;; tina-to-csv.el
;;;
;;; Convert tina 123 txt file to csv file.

(defun tina-extract-columns (begcol endcol)
  (beginning-of-line)
  (move-to-column begcol)
  (let ((beg (point)))
    (if (> endcol 0)
	(move-to-column endcol)
      (end-of-line))
    (fume-trim-string (buffer-substring beg (point)))))

(defun tina-to-list ()
  (let (caption pfx last4 intn tlist)
    (beginning-of-buffer)
    (while (not (eobp))
      (setq caption (tina-extract-columns 2 42))
      (setq pfx (tina-extract-columns 43 45))
      (setq last4 (tina-extract-columns 46 50))
      (setq intn (tina-extract-columns 59 0))
      (push (list caption (concat pfx last4) intn) tlist)
      (beginning-of-line)
      (next-line 1)
      )
    (setq tlist (reverse tlist))))

(defun tina-to-csv ()
  "When called, must be in the frame with two windows.
Window1 should show buffer with tina txt file, with the
introductory comments deleted (i.e. only data lines).
Window2 should have two lines: 1. the first line of an 
exported outlook windows csv which has the titles of
the fields. 2. A second line that has a prototype
of the new wanted line where $ is to replaced by
the tina pattern (with no space between the digits),
% is to be replaced by the full international pattern,
and # is to replaced by the caption (mainly city)."
  (interactive "")
  (let (tlist pattern caption tnum intn etnry)
    (beginning-of-buffer)
    (setq tlist (tina-to-list))

    (other-window 1)
    (beginning-of-buffer)
    (next-line 1)
    (let ((beg (point)))
      (next-line 1)
      (setq pattern (buffer-substring beg (point)))
      (delete-region beg (point)))

    (end-of-buffer)

    (while tlist
      (multiple-value-setq (caption tnum intn) (pop tlist))

      (insert pattern)
      (previous-line 1)

      ;;(prompt-for-insert "b1")
      
      (replace-string "$" tnum)

      ;;(prompt-for-insert "b1")

      (beginning-of-line)
      (replace-string "%" intn)

      ;;(prompt-for-insert "b1")

      (beginning-of-line)
      (replace-string "#" caption)
      
      ;;(prompt-for-insert "b1")

      (end-of-buffer)
      )
    )
  )
