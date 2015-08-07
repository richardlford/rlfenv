;; lword.el - Long word movement (with _)

(defun lforward-word (arg)
  (interactive "_p")
  (forward-sexp arg)
  )

(defun lbackward-word (arg)
  (interactive "_p")
  (backward-sexp arg)
  )


