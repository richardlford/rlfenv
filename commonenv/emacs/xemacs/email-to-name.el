; Convert email string of the form "first.middle.last@company.com" to 
; "First Middle Last".
(defun email-to-name (email)
  "Convert email string of the form first.middle.last@company.com to 'First Middle Last'"
  (interactive "semail")
  (let
      ((n (length email))
       (result email)
       )
    (setq result (replace-in-string result "@.*" ""))
    (setq result (replace-in-string result "\\." " "))
    (upcase-initials result)))
