(defun prompt-for-insert (prompt) (interactive)
  (let ((start (copy-marker (point)))
	end)
    (message prompt)
    (recursive-edit)
    (buffer-substring start (point))
    )
)