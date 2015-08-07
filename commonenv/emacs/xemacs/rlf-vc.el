;; rlf-vc.el
;; My modification to version control system.
(defun vc-toggle-read-only (&optional verbose)
  "Change read-only status of current buffer, perhaps via version control.
If the buffer is visiting a file registered with version control,
then check the file in or out.  Otherwise, just change the read-only flag
of the buffer.  With prefix argument, ask for version number."
  (interactive "P")
  (let* ((file (buffer-file-name))
	 (bk (vc-backend file)))
    (cond
     ((eq bk 'CVS)
      (cond
       (buffer-read-only
	(message "Running 'cedit %s'" file)
	(vc-do-command nil nil "cedit" file 'WORKFILE)
	(toggle-read-only)
	(switch-to-buffer-other-frame "*vc*")
	)
       (t
	(toggle-read-only)
	(message "Buffer set to readonly (assuming you did a commit/unedit of %s."
		 file))
       )
      )
     (bk
      (vc-next-action verbose))
     (t
      (toggle-read-only)))))
