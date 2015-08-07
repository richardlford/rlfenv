;template.el
; This file has functions related to "templates".  
; A template is an sexp ending with zapp (which stands for zap previous).
; zapp is an abbrev which expands to delete the sexp that it ends as well as 
; the character that caused the expansion.
; forward-template and backward-template scan forward or backward for zapp and then 
; position after it.  Since zapp when typed and followed by a delimiter deletes itself, 
; the word zappi is an abbrev that expands to zapp.  Of course, then typing
; zappi is tricky.  zapp_fun is the hook that is invoked when zapp is expanded.
(defun forward-template ()
  (interactive)
  (word-search-forward "zapp"))

(defun forward-template-limit (limit)
  (word-search-forward "zapp" limit t))

(defun backward-template ()
  (interactive)
  (backward-char 1)
  (word-search-backward "zapp")
  (forward-word 1)
  ;(prompt-for-insert "bt:")
  )

(defun zapp_fun ()
  (interactive)
  ;(prompt-for-insert "zf1")
  (kill-sexp -1)
  ;(prompt-for-insert "zf2")
  (setq unread-command-char ?\177)
  ;(prompt-for-insert "zf3")
  )

(defun backward-template-abbrev ()
  (interactive)
  ;(prompt-for-insert "bta:")
  (backward-template) (zapp_fun))
(defun backward-xxxx-abbrev ()
  (interactive)
  (setq unread-command-char ?\177))

