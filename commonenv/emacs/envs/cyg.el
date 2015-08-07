(setq shell-prompt-pattern "^\$ ")
(setenv "WORK" (concat (getenv "HOME") "/work"))
(setenv "WORKROOT" (getenv "WORK"))
(setenv "OLDPATH" (getenv "PATH"))
(setenv "PATH" (string-subst-char ?/ ?\\
				  (concat
				   (getenv "HOME") "/bin;"
				   "c:/cygwin/bin;"
				   "c:/cygwin/usr/X11R6/bin;"
				   (getenv "PATH") ";"
				   "/cygdrive/c/xemacs/XEmacs-21.5.29/i586-pc-win32;"
				   ;; "/cygdrive/c/Program Files (x86)/GnuWin32/bin;"
				   "."
				   )))
(setenv "NEWPATH" (getenv "PATH"))
;;(setenv "SHELL" "c:/cygwin/bin/bash")
(setenv "SHELL" "c:/windows/system32/cmd.exe")
(setenv "CVSREAD" "1")

(setenv "ENV" (concat (getenv "HOME") "/environ.ksh"))
(progn
  (setenv "WHICHENV" "Cg")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]"))))
