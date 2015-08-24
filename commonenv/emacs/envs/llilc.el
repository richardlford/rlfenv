(setq shell-prompt-pattern "^\$ ")
(setenv "WORK" (concat (getenv "HOME") "/work"))
(setenv "WORKROOT" (getenv "WORK"))
(setenv "OLDPATH" (getenv "PATH"))
(setenv "PATH" (string-subst-char ?/ ?\\
				  (concat
				   (getenv "RLFENV") "/bin;"
				   (getenv "PATH") ";"
				   "."
				   )))
(setenv "NEWPATH" (getenv "Path"))
(setenv "SHELL" (concat (getenv "DataDrive") "/cygwin64/bin/bash"))
(setenv "PS1" "$ ")
;;(setenv "SHELL" "c:/windows/system32/cmd.exe")
(setenv "CVSREAD" "1")

(setenv "ENV" (concat (getenv "HOME") "/environ.ksh"))
(progn
  (setenv "WHICHENV" "Cg")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]"))))
