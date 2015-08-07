(defun spflip ()
  "Flip from $r0.1 to $r0.12 as the stack pointer. When this
function is called it is just after $r0.1. This function
will switch change $r0.1 to $r0.12 and $r0.12 to $r0.1.
Other $r0.1x will be unchanged for x some digit."
  (interactive)
  (beginning-of-line)
  (while (search-forward "$r0.1" nil t)
    (cond
     ((not (looking-at "[0-13-9]"))
      (cond
       ((looking-at "2")
	(delete-char 1))
       (t
	(insert "2")))
      ))
    )
  (save-buffer)
  )

      
