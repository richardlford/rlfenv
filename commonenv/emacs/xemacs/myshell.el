;;; -*-Emacs-Lisp-*- General command interpreter in a window stuff
;;;
;;; This is just my customized interface to shell, which
;;; is what was formerly cmushell.emac
(require 'shell)

(defun myshell ()
  "Run an inferior shell, with I/O through buffer *myshell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, 
 just switch to buffer *myshell*.
Program used comes from variable explicit-shell-file-name,
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file ~/.emacs_SHELLNAME exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in myshell-mode, giving commands for sending input
and controlling the subjobs of the shell.  See myshell-mode.
See also variable shell-prompt-pattern.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-arguments'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive)
  (myshell-buffer "myshell"))

; Here are for other cmu shells.
(defun myshell-ctrl ()
  "Run an inferior shell, with I/O through buffer *myshell-ctrl*."
  (interactive)
  (myshell-buffer "myshell-ctrl"))
(defun myshell-shift ()
  "Run an inferior shell, with I/O through buffer *myshell-shift*."
  (interactive)
  (myshell-buffer "myshell-shift"))
(defun myshell-meta ()
  "Run an inferior shell, with I/O through buffer *myshell-meta*."
  (interactive)
  (myshell-buffer "myshell-meta"))


(defun myshell-buffer (buffer-name)
  "Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, 
 just switch to buffer `*shell*'.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive "B")
  (let (buffer)
    (let* ((prog (or explicit-shell-file-name
		     (getenv "ESHELL")
		     (getenv "SHELL")
		     "/bin/sh"))		     
	   (name (file-name-nondirectory prog))
	   (which-env (concat "_" (or (getenv "WHICHENV") "")))
	   (startfile (concat (getenv "rlfenv") "/.emacs_" name which-env))
	   (xargs-name (intern-soft (concat "explicit-" name "-args"))))
      ;(prompt-for-insert "in myshell-buffer")
      (setq buffer (set-buffer (apply 'make-comint buffer-name prog
				      (if (file-exists-p startfile)
					  startfile)
				      (if (and xargs-name
					       (boundp xargs-name))
					  (symbol-value xargs-name)
					'("-i" "-L")))))
      (shell-mode)
      (switch-to-buffer buffer)
      )))

(defun myshell-sua ()
  "Run an inferior shell, with I/O through buffer *myshell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, 
 just switch to buffer *myshell*.
Program used comes from variable explicit-shell-file-name,
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file ~/.emacs_SHELLNAME exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in myshell-mode, giving commands for sending input
and controlling the subjobs of the shell.  See myshell-mode.
See also variable shell-prompt-pattern.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-arguments'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive)
  (myshell-buffer-sua "myshell"))

; Here are for other cmu shells.
(defun myshell-ctrl-sua ()
  "Run an inferior shell, with I/O through buffer *myshell-ctrl*."
  (interactive)
  (myshell-buffer-sua "myshell-ctrl"))
(defun myshell-shift-sua ()
  "Run an inferior shell, with I/O through buffer *myshell-shift*."
  (interactive)
  (myshell-buffer-sua "myshell-shift"))
(defun myshell-meta-sua ()
  "Run an inferior shell, with I/O through buffer *myshell-meta*."
  (interactive)
  (myshell-buffer-sua "myshell-meta"))

(defun myshell-buffer-sua (buffer-name)
  "Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, 
 just switch to buffer `*shell*'.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive "B")
  (let (buffer)
    (let* ((prog (or explicit-shell-file-name
		     (getenv "ESHELL")
		     (getenv "SHELL")
		     "/bin/sh"))		     
	   (name (file-name-nondirectory prog))
	   (which-env (concat "_" (or (getenv "WHICHENV") "")))
	   (startfile (concat "~/.emacs_" name) which-env)
	   (xargs-name (intern-soft (concat "explicit-" name "-args"))))
      (setq buffer (set-buffer (apply 'make-comint buffer-name prog
				      (if (file-exists-p startfile)
					  startfile)
				      (if (and xargs-name
					       (boundp xargs-name))
					  (symbol-value xargs-name)
					'("-i" "-l")))))
      (shell-mode)
      (switch-to-buffer buffer)
      )))

(defun myshell-powershell ()
  "Run an inferior shell, with I/O through buffer *myshell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, 
 just switch to buffer *myshell*.
Program used comes from variable explicit-shell-file-name,
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file ~/.emacs_SHELLNAME exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in myshell-mode, giving commands for sending input
and controlling the subjobs of the shell.  See myshell-mode.
See also variable shell-prompt-pattern.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-arguments'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive)
  (myshell-buffer-powershell "mypowershell"))

; Here are for other powershell shells.
(defun myshell-ctrl-powershell ()
  "Run an inferior shell, with I/O through buffer *mypowershell-ctrl*."
  (interactive)
  (myshell-buffer-powershell "mypowershell-ctrl"))
(defun myshell-shift-powershell ()
  "Run an inferior shell, with I/O through buffer *mypowershell-shift*."
  (interactive)
  (myshell-buffer-powershell "mypowershell-shift"))
(defun myshell-meta-powershell ()
  "Run an inferior shell, with I/O through buffer *mypowershell-meta*."
  (interactive)
  (myshell-buffer-powershell "mypowershell-meta"))

(defun myshell-f6-powershell ()
  "Run an inferior shell, with I/O through buffer *mypowershell-f6*."
  (interactive)
  (myshell-buffer-powershell "mypowershell-f6"))
(defun myshell-f6-ctrl-powershell ()
  "Run an inferior shell, with I/O through buffer *mypowershell-f6-ctrl*."
  (interactive)
  (myshell-buffer-powershell "mypowershell-f6-ctrl"))
(defun myshell-f6-shift-powershell ()
  "Run an inferior shell, with I/O through buffer *mypowershell-f6-shift*."
  (interactive)
  (myshell-buffer-powershell "mypowershell-f6-shift"))
(defun myshell-f6-meta-powershell ()
  "Run an inferior shell, with I/O through buffer *mypowershell-f6-meta*."
  (interactive)
  (myshell-buffer-powershell "mypowershell-f6-meta"))


(defun powershell-gen-window-width-string ()
  (let ( (ww (window-width))
	 )
    ;;(if (< ww 200)
    ;;   (setq ww 200)
    ;;  )
    (concat  "$a = (Get-Host).UI.RawUI\n" 
	     "$b = $a.WindowSize\n"
	     "$b.Width = " (number-to-string  ww) "\n"
	     "$a.BufferSize = $b\n"
	     "$a.WindowSize = $b")
    ))
  

(defvar powershell-prompt-pattern  "PS [^#$%>]+>" 
  "Regexp for powershell prompt.  This isn't really used, because I couldn't figure out how to get it to work."
  )

(defgroup powershell nil
  "Running shell from within Emacs buffers."
  :group 'processes
  )


(defcustom powershell-need-rawui-resize t
  "set when powershell needs to be resized"
  :group 'powershell
)

(defun myshell-buffer-powershell (buffer-name)
  "Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, 
 just switch to buffer `*shell*'.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive "B")
  (let (buffer)
    (let* ((prog "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
	   (name (file-name-nondirectory prog))
	   (which-env (concat "_" (or (getenv "WHICHENV") "")))
	   (startfile (concat (getenv "rlfenv") "/.emacs_" name which-env))
	   )
      ;(prompt-for-insert "in myshell-buffer-powershell")
      (setq buffer (set-buffer (apply 'make-comint buffer-name prog
				      (if (file-exists-p startfile)
					  startfile)
				      '("-Command" "-")
				      )))
      (shell-mode)
      (switch-to-buffer buffer)
      (let (
	    (proc (get-buffer-process buffer))
	    )
    
	;; This sets up the powershell RawUI screen width. By default,
	;; the powershell v1.0 assumes terminal width of 80 chars.
	;;This means input gets wrapped at the 80th column.  We reset the
	;; width of the PS terminal to the window width. 
	(add-hook 'window-size-change-functions 'powershell-window-size-changed)
	
	(powershell-window-size-changed)
	
	;; ask for initial prompt
	(if
	    (<= (point) 1)
	    (comint-simple-send proc "prompt")
	  )
	)

      ;; hook the kill-buffer action so we can kill the inferior process?
      (add-hook 'kill-buffer-hook 'powershell-delete-process)
      
      ;; wrap the comint-input-sender with a PS version
      ;; must do this after launching the shell! 
      (make-local-variable 'comint-input-sender)
      (setq comint-input-sender 'powershell-simple-send)

      (make-local-variable 'comint-prompt-regexp)
      (setq comint-prompt-regexp powershell-prompt-pattern)
      (make-local-variable 'paragraph-separate)
      (setq paragraph-separate "xxxxxxxxxxxxxxxxxxxxxxxxxxxx")
 
      ;; set a preoutput filter for powershell.  This will trim newlines after the prompt.
      (add-hook 'comint-preoutput-filter-functions 'powershell-preoutput-filter-for-prompt)

      (add-hook 'comint-output-filter-functions 'powershell-postoutput-filter)
      
      ;;(run-hooks 'powershell-launch-hook)

      )))


(defun powershell-window-size-changed (&optional frame)
  ; do not actually resize here. instead just set a flag.
  (setq powershell-need-rawui-resize t)
)

(defun powershell-postoutput-filter (string)
  ;;(prompt-for-insert "in powershell-postoutput-filter")
  (if (and (string-match  powershell-prompt-pattern  string)
	   (string-equal (substring string -1) "\n")
	   (string-equal (buffer-substring (1- (point)) (point)) "\n")
	   )
      (delete-backward-char 1)
    )
  )

(defun powershell-delete-process (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and (processp proc)
       (delete-process proc))
  )



;; This function trims the newline from the prompt that we
;; get back from powershell.  It is set into the preoutput
;; filters, so the newline is trimmed before being put into
;; the output buffer.
(defun powershell-preoutput-filter-for-prompt (string)
  (prompt-for-insert "In powershell-preoutput-filter-for-prompt")
   (if
       ; not sure why, but I have not succeeded in using a variable here???  
       ;(string-match  powershell-prompt-pattern  string)

       (string-match  "PS [^#$%>]+>" string)
       (substring string 0 -1)
     
     string

     )
   )

(defun powershell-simple-send (proc string)
  "Override of the comint-simple-send function, specific for powershell.
This just sends STRING, plus the prompt command. Normally powershell is in
noninteractive model when run as an inferior shell with stdin/stdout
redirected, which is the case when running as a shell within emacs.
This function insures we get and display the prompt. "
  ; resize if necessary. We do this by sending a resize string to the shell,
  ; before sending the actual command to the shell. 
  (if powershell-need-rawui-resize
      (and
       (comint-simple-send proc (powershell-gen-window-width-string))
       (setq powershell-need-rawui-resize nil)
       )
    )
  (comint-simple-send proc string)
  (comint-simple-send proc "prompt")
)
