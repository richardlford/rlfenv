;; convert-comments.el
;;
;; Convert /* ... */ style comments to // comments.
;; Remove leading blanks, and stars at the beginning of 
;; lines. /* ... */ style comments which have non-whitespace
;; things following the */ are not converted.

(defun convert-comment-delete-leading-stuff (col end-comment)
  "Delete leading spaces and stars, indent to column col,
then insert //.
But do not delete anything after end-comment."
  (interactive)
  (let* ((start (point))
	 (endline (save-excursion (end-of-line) (point)))
	 (end-part (if (< end-comment endline)
		       end-comment
		     endline))
	 (end-leader (save-excursion
		       (re-search-forward "\\s-*\\**")
		       (point)))
	 (end-delete (if (< end-part end-leader)
			 end-part
		       end-leader))
	 )
    ;;(prompt-for-insert "a0")
    (if (< (point) end-delete)
	(delete-region start end-delete))
    (indent-to-column col)
    (insert "// ")
    )
  )

(defun convert-next-comment ()
  "Convert a /* ... */ comment to a // comment. When called,
point is between the leading / and *."
  (interactive)
  (let* ((start (point))
	 (end-comment
	  (save-excursion
	    ;; Defend against missing end of comment
	    (if (search-forward "*/" nil t)
		(point-marker)
	      nil)))
	 (col (1- (current-column)))
	 end-line-point
	 all-white
	 )
    
    ;;(prompt-for-insert "b0")
    ;; Check to see if there is anything after the */ on the line.
    ;; If so, then do not convert the comment.
    (if end-comment
	(progn
	  (save-excursion
	    (goto-char end-comment)
	    (end-of-line)
	    (setq end-line-point (point))
	    (goto-char end-comment)
	    (re-search-forward "\\s-*")
	    (setq all-white (looking-at "\n")))
	  (cond
	   (all-white
	    (save-excursion
	      (goto-char end-comment)
	      ;; Delete the */ and trailing white space
	      (delete-region (- end-comment 2) end-line-point)
	      )
	    
	    ;; Delete the leading /. 
	    (delete-backward-char 1)
	    (convert-comment-delete-leading-stuff col end-comment)
	    (forward-line 1)
	    ;;(prompt-for-insert "b1")
	    (while (<= (point) end-comment)
	      ;;(prompt-for-insert "b2")
	      (convert-comment-delete-leading-stuff col end-comment)
	      (forward-line 1)
	      ;;(prompt-for-insert "b3")
	      )
	    ))
	  (goto-char end-comment)
	  )
      (end-of-buffer)
      )
    )
  )

(defun next-comment-string-or-char ()
  (interactive)
  (re-search-forward "\\(/\\*\\|'\\|\"\\|//\\)" nil 'noerr)
  )

(defun next-comment ()
  "Position between / and * of next /* comment. Return nil
if there is no more comment"
  (interactive)
  (if (next-comment-string-or-char)
      (progn
	(backward-char 1)
	(while (or (looking-at "'")
		   (looking-at "\"")
		   (looking-at "/")
		   )
	  (cond
	   ((looking-at "/")
	    ;; It was //. Skip to end of line
	    (end-of-line))
	   (t
	    (forward-sexp))
	   )
	  (next-comment-string-or-char)
	  (backward-char 1)
	  )
	(if (looking-at "*")
	    t
	  nil)
	)
    nil)
  )

(defun convert-comments ()
  "Convert all the comments in a buffer"
  (interactive)
  (beginning-of-buffer)
  (while (next-comment)
    ;;(prompt-for-insert "ccs0")
    (convert-next-comment))
  )

(defun convert-one-comment ()
  "Convert next /* comment to // comment"
  (interactive)
  (if (next-comment)
      (convert-next-comment))
  )
