;;; etags-rlf.el --- My extensions to the tag facilities.

(defun tags-prompt-for-insert (regexp &optional file-list-form)
  "Search through all files listed in tags table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue-save].

See documentation of variable `tag-table-alist'."
  (interactive "sTags search (regexp): ")
  (if (and (equal regexp "")
           (eq (car tags-loop-scan) 'with-caps-disable-folding)
           (null tags-loop-operate))
      ;; Continue last tags-search as if by M-,.
      (tags-loop-continue-save nil)
    (setq tags-loop-scan `(with-caps-disable-folding ,regexp
                            (re-search-forward ,regexp nil t))
          tags-loop-operate '(prompt-for-insert
			      "In tags recursive edit. Use top-level to exit loop."))
    (tags-loop-continue-save (or file-list-form t))))

(defun tags-loop-do-key (regexp key &optional file-list-form)
  "Search through all files listed in tags table for match for REGEXP.
For each match found do the command bound to key. The command is done with
point in front of the match, so as part of its action it should 
position after the match.

See documentation of variable `tag-table-alist'."
  (interactive "sTags loop do key: search (regexp): \nkKey bound to command to do")
  ;;(prompt-for-insert "b1")
  ;;(prompt-for-insert "b1b")
  (if (and (equal regexp "")
           (eq (car tags-loop-scan) 'with-caps-disable-folding)
           (null tags-loop-operate))
      ;; Continue last tags-search as if by M-,.
      (progn
	;;(prompt-for-insert "b1c")
	(tags-loop-continue-save nil)
	;;(prompt-for-insert "b1d")
	)
    ;;(prompt-for-insert "b2")
    (setq tags-loop-scan `(with-caps-disable-folding ,regexp
                            (if (re-search-forward ,regexp nil t)
                              ;; When we find a match, move back
                              ;; to the beginning of it so perform-replace
                              ;; will see it.
                              (progn (goto-char (match-beginning 0)) t))))
    ;;(prompt-for-insert "b3")
    (setq tags-loop-operate `(progn (execute-kbd-macro ,key) t))
    ;;(prompt-for-insert "b4")
    (tags-loop-continue-save (or file-list-form t))
    ;;(prompt-for-insert "b5")
    ))

(defun tags-loop-continue-save (&optional first-time)
  "Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument to begin such a command (the
argument is passed to `next-file', which see).
Two variables control the processing we do on each file:
the value of `tags-loop-scan' is a form to be executed on each file
to see if it is interesting (it returns non-nil if so)
and `tags-loop-operate' is a form to execute to operate on an interesting file
If the latter returns non-nil, we exit; otherwise we scan the next file.
We save any files we modify."
  (interactive)
  (let (new tlo
        (messaged nil))
    ;;(prompt-for-insert "tlc1")
    (while
        (progn
          ;; Scan files quickly for the first or next interesting one.
          (while (or first-time
                     (save-restriction
                       (widen)
                       (not (eval tags-loop-scan))))
	    ;; If previous file was modified, save it.
	    (or first-time
		(not (buffer-modified-p))
		(save-buffer))
            (setq new (next-file first-time
				 tags-search-nuke-uninteresting-buffers))
            ;; If NEW is non-nil, we got a temp buffer,
            ;; and NEW is the file name.
            (if (or messaged
                    (and (not first-time)
                         (> (device-baud-rate) search-slow-speed)
                         (setq messaged t)))
                (message "Scanning file %s..." (or new buffer-file-name)))
            (setq first-time nil)
            (goto-char (point-min)))

	  ;;(prompt-for-insert "tlc2")
          ;; If we visited it in a temp buffer, visit it now for real.
          (if (and new tags-search-nuke-uninteresting-buffers)
              (let ((pos (point)))
                (erase-buffer)
                (set-buffer (find-file-noselect new))
                (widen)
                (goto-char pos)))

          (switch-to-buffer (current-buffer))

          ;; Now operate on the file.
          ;; If value is non-nil, continue to scan the next file.
          (setq tlo (eval tags-loop-operate))
	  ;;(prompt-for-insert "tlc5")
	  tlo
	  ))
    (and messaged
         (null tags-loop-operate)
         (message "Scanning file %s...found" buffer-file-name))))
