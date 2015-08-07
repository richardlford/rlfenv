(defun filename (s) 
  "Find trailing filename in a pathname.  For example,
   (filename \"a/b/c\") is \"c\".  If no filename, then
   return \"\"."
   (if (string-match ".*/\\(.*\\)" s)
       (substring s (match-beginning 1))
     "")
   )
