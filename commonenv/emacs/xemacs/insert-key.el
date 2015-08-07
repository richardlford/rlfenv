(defun my-key-symbol (keypath)
  "Get symbol whose name is the keypath's name, without the
   [#<keypress-event ] stuff."
  (let* ((keypathname (prin1-to-string keypath))
	 (kpnl (length keypathname))
	 (l2 (length "[#<keypress-event "))
	 (l3 (length ">]"))
	 (str (substring keypathname l2 (- kpnl l3))))
    ;;(prompt-for-insert "b0")
    (intern str)))
  
(defun insert-key (keypath)
  "Prompt for keypath then insert its command as a string."
  (interactive "*kKey: ")
  (let ((defn (key-binding keypath)))
    (insert (prin1-to-string defn))))
(defun insert-key-function (keypath)
  "Prompt for keypath then insert its command as a string."
  (interactive "*kKey: ")
  (let ((defn (key-binding keypath)))
    (insert (concat "(" (prin1-to-string defn) ")"))))
(defun insert-keypath (keypath)
  "Prompt for keypath then insert its command as a string."
  (interactive "*kKey: ")
  (insert (prin1-to-string keypath)))

(defun bind-last-kbd-macro (keypath)
  "Prompt for keypath then bind last keyboard macro to that.
   Also save the definition for later loading."
  (interactive "kKey: ")
  (let* ((keysymbol (my-key-symbol keypath))
	 (keystr (symbol-name keysymbol))
	 macrofile)
    (name-last-kbd-macro keysymbol)
    (global-set-key keypath keysymbol)
    (save-excursion
      (save-window-excursion
	(setq macrofile (expand-file-name (concat "~/emacs/kbd-macros/" keystr ".el")))
	(find-file macrofile)
	(end-of-buffer)
	(insert-kbd-macro keysymbol t)
	(if (buffer-modified-p) (save-buffer))
	))
    ))

(defun insert-macro ()
  "Insert the last keyboard macro as lisp code"
  (interactive)
  (name-last-kbd-macro 'xxxxxxxxxxxxx)
  (insert-kbd-macro 'xxxxxxxxxxxxx)
  )
(defun bind-insert-last-kbd-macro (keypath)
  "Prompt for keypath then bind last keyboard macro to that"
  (interactive "kKey: ")
  (let ((keysymbol (intern (prin1-to-string keypath)))
	(mks (my-key-symbol keypath)))
    ;;(prompt-for-insert "b1")
    (name-last-kbd-macro mks)
    (global-set-key keypath mks)
    (insert-kbd-macro mks t)
    ))

(defun load-my-kbd-macros ()
  (interactive)
  (let ((kbd-files (directory-files (concat home-dir "emacs/kbd-macros") t ".el$" nil t))
	file)
    (while kbd-files
      (setq file (car kbd-files))
      (setq kbd-files (cdr kbd-files))
      ;;(prompt-for-insert "b1")
      (load file)
      )))
