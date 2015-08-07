;;; do-months.el
;;; Approximate calendar

(defun do-months ()
  (interactive "")
  (setq mm 101)
  (while (<= mm 112)
    (setq dd 101)
    (while (<= dd 131)
      (insert 
       (format "%s/%s: \n" 
	       (substring (format "%2d" mm) 1 3)
	       (substring (format "%2d" dd) 1 3)))
      (setq dd (1+ dd))
      )
    (setq mm (1+ mm))
    )
  )
