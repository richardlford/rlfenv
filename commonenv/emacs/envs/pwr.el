(setq shell-prompt-pattern "^\$ ")
(setenv "WORK" (concat (getenv "HOME") "/work"))
(setenv "WORKROOT" (getenv "WORK"))
(setenv "OLDPATH" (getenv "PATH"))
(setenv "PATH" (string-subst-char ?/ ?\\
				  (concat
				   (getenv "HOME") "/bin;"
				   (getenv "PATH") ";"
;;				   "c:/cygwin/bin;"
;;				   "c:/cygwin/usr/X11R6/bin;"
				   "/cygdrive/c/User/xemacs/XEmacs-21.4.6/i586-pc-win32;"
				   "."
				   )))
(setenv "NEWPATH" (getenv "PATH"))
;;(setenv "SHELL" "c:/Windows/POWERSHELL/common/ksh.bat")
(setenv "CVSREAD" "1")
(global-set-key [(f8)] 'myshell-powershell)
(global-set-key [(shift f8)] 'myshell-shift-powershell)
(global-set-key [(meta f8)] 'myshell-meta-powershell)
(global-set-key [(control f8)] 'myshell-ctrl-powershell)

;;(setenv "ENV" (concat (getenv "HOME") "/environ.ksh"))
(progn
  (setenv "WHICHENV" "PW")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]"))))
