(defun suffix (s) 
  "Find trailing suffix after a dot.  For example,
   (suffix \"a.b.c\") is \".c\".  If no suffix, then
   return \"\"."
   (if (string-match ".*\\.\\(.*\\)" s)
       (substring s (1- (match-beginning 1)))
     "")
   )


