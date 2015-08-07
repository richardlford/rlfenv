;;; My setup for xemacs.
;;(set-glyph-image nontext-pointer-glyph [autodetect :data "top_left_arrow"])
;;(set-glyph-image text-pointer-glyph [autodetect :data "top_left_arrow"])


(defun toggle-debug-on-error ()
  "Toggle Debug on Error"
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "Debug-on-error: %s" debug-on-error))

(defun show-buffer-filename ()
  (interactive)
  (message (or buffer-file-name default-directory)))

(defun string-subst-char (new old string)
  (let (index)
    (setq old (regexp-quote (char-to-string old))
	  string (substring string 0))
    (while (setq index (string-match old string))
      (aset string index new)))
  string)

(defun find-my-tags () (interactive) (find-file "~/tags"))

(defmacro definteractive (&rest body)
  (`
    (function (lambda () (interactive) (,@ body )))
  )
)

(defun my-exit-from-emacs ()8
  (interactive)
  (if (yes-or-no-p "Do you want to exit ")
      (save-buffers-kill-emacs)))

(global-set-key "\C-x\C-c" 'my-exit-from-emacs)

;; Support keypad keys
(defun scroll-down-one (n) (interactive "p") (scroll-down n))
(defun scroll-up-one (n) (interactive "p") (scroll-up n))

(defvar tag-other-frame nil)
(defun find-tag-other-frame (tagname &optional next)
  "Find the tag of the item at point and display in other frame"
  (interactive (if current-prefix-arg
		   '(nil t)
		   (find-tag-tag "Find tag other frame: ")))
  (let
      (tag-default
       (old-frame (selected-frame))
       )
    (setq tag-default (find-tag-default))
    (if (null tag-other-frame)
	(setq tag-other-frame
	      (make-frame
	       '((name . "tags")
		 (left . 645)
		 (top . 0)
		 (width . 80)
		 (height . 40)))))

    (select-frame tag-other-frame)
    (if (find-tag tag-default)
	(progn
	  (raise-frame)
	  ))
    (select-frame old-frame)
    (raise-frame)
    ))

(defun my-dired-up ()
  (interactive)
  (let*
      ( (f buffer-file-name)
	(d (if f
	       (file-name-directory f)
	       (file-name-directory (substring default-directory 0 -1))))
	(b (get-buffer (concat d))))
    (if b
	(switch-to-buffer b)
	(if d (dired d)))))

(defun my-dired-mode-hook ()
  (interactive)
  (setq truncate-lines t)
  (define-key dired-mode-map [button2] (mousefun-set-point (setq)))
  (define-key dired-mode-map [button2up] (mousefun-set-point (dired-find-file)))
  (define-key dired-filename-local-map [button2] (mousefun-set-point (setq)))
  (define-key dired-filename-local-map [button2up] (mousefun-set-point (dired-find-file)))
  )
(defun toggle-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (message "truncate-lines = %s" truncate-lines))

(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(defun minibuffer-electric-slash ()
  ;; by Stig@hackvan.com
  (interactive)
  (and (eq ?/ (preceding-char))
       (not (eq (point) (1+ (point-min)))) ; permit `//hostname/path/to/file'
       (not (eq ?: (char-after (- (point) 2))))	; permit `http://url/goes/here'
       (delete-region (point-min) (point)))
  (self-insert-internal ?/))

;(prompt-for-insert "xs1")
(defun newline-and-auto-fill ()
  "Start a new line that is filled with current prefix"
  (interactive)
  (if (null fill-prefix)
      (let (fill-prefix)
	(filladapt-adapt nil nil)
	(insert "\n")
	(insert fill-prefix)
	)
    (insert "\n")
    (insert fill-prefix)
    ))


;; Disable standard term setup.
(setq term-setup-hook nil)

(setq tags-search-nuke-uninteresting-buffers nil)
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
;;(load "verilog-mode")
(setq auto-mode-alist (cons  '("\\.vhd\\'" . vhdl-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.v\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.vpm\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.vpp\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.u\\'" . verilog-mode) auto-mode-alist))
(setq verilog-library-directories
      '("." "../lx_c3000_lib" "../lx_cache_lib" "../lx_common_lib" "../lx_control_lib" 
	"../lx_datapath_lib" "../lx_guf_lib" "../lx_iblocks_lib" "../lx_issue_br_lib"
	"../lx_lx1_lib" "../lx_p100_lib" "../lx_regfile_lib" "../lx_testchip_lib"))
;(prompt-for-insert "xs2")

(load "xemacs-mouse")
(load "insert-key")
(load "suffix")
(load "filename")
(load "template")
(load "istr")
;;(load-library "completer")
(load "myshell")
;(prompt-for-insert "xs5")
;(load "rlf-replace")
;(prompt-for-insert "xs7")
(load "lxgdbsrc.el")
(load "filladapt")
;(prompt-for-insert "xs6")
(load "dired")
(load "comint")
(load "shell")
(load "gud")
;(prompt-for-insert "xs4")
(load "gdb")
(load "gdbsrc")
(load "specifier")

;;(load "pcl-cvs")
;;(load "vc")
;;(load "rlf-vc")
(load "dired-xemacs")
(load "xemacs-bindings")
(load-my-kbd-macros)
(setq gnus-select-method '(nntp "news.bri.st.com"))
(load "mif-mode")
(setq auto-mode-alist (cons  '("\\.mif\\'" . mif-mode) auto-mode-alist))
(setq dired-no-confirm '(symlink uncompress recursive-delete kill-file-buffer
				 kill-dired-buffer patch create-top-dir revert-subdirs))
(setq delete-key-deletes-forward nil)
(load "max-line-length")

;;
;; Configure indentation
;;
;(prompt-for-insert "xs3")

(defconst sacg-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)
				   (statement-case-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
				   (comment-intro     . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    )
  "SACG C Programming Style")

;; Customizations for all of c-mode, c++-mode, and objc-mode
(defun sacg-c-mode-common-hook ()
  ;; add personal style and set it for the current buffer
  (c-add-style "sacg" sacg-c-style t)
  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '++)
  ;; other customizations
  (setq tab-width 8
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  (c-toggle-hungry-state 1)
;;  (c-toggle-auto-state 1)
  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, and idl-mode-map inherit from it.
  (define-key c-mode-base-map '(control m) 'newline-and-indent)
  (make-variable-buffer-local 'after-save-hook)
  ;; (add-hook 'after-save-hook 'check-max-length-hook)
  )

(defun rct-c-mode-common-hook ()
  ;; add personal style and set it for the current buffer
  (c-add-style "rct" rct-c-style t)
  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro '++)
  ;; other customizations
  (setq tab-width 8
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  (c-toggle-hungry-state 1)
;;  (c-toggle-auto-state 1)
  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, and idl-mode-map inherit from it.
  (define-key c-mode-base-map '(control m) 'newline-and-indent)
  (make-variable-buffer-local 'after-save-hook)
  ;; (add-hook 'after-save-hook 'check-max-length-hook)
  )

;(add-hook 'c-mode-common-hook 'sacg-c-mode-common-hook)
;(toggle-tags-always-exact)
(setq cperl-indent-level 4)
;(load "psgml")
(load "convert-comments")
(load "find-grep")

; For maxima.

(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))
;(load "emaxima")
(defmacro define-minor-mode (mode doc &optional init-value lighter keymap &rest body)
  "Define a minor mode like in Emacs."
  `(progn
     (defvar ,mode ,init-value "Non-nil if the corresponding mode is enabled.")
     (defun ,mode (&optional arg)
	    ,doc
	    (setq ,mode (if arg
			   (> (prefix-numeric-value arg) 0)
			 (not foo)))
	    ,@body
	    ,mode)
     (add-minor-mode ,mode ,lighter ,keymap)))

(setq-default comint-prompt-regexp "^$ ")
(setq-default shell-prompt-pattern "^$ ")
(setq comint-prompt-regexp "^$ ")
(setq shell-prompt-pattern "^$ ")
;;************************************************************************
;;
;; Stuff to add to your .emacs file to invoke the csharp mode.
;;
;; C#
;;
;;; 6/11/02 FIXED FOR EMACS VERSIONS 21.1.1 AND HIGHER.  These have a new
;;; version of cc-mode.
 
;; (autoload 'csharp-mode "csharp")
;; 
;;  (defun my-csharp-mode()
;;   "Setup my private C# mode"
;;   (setq indent-tabs-mode nil)
;;   (setq c-basic-offset 4)
;;   (c-set-offset 'case-label 2 t)
;;   (c-set-offset 'innamespace 2 t)
;;   (modify-syntax-entry ?_ "w" java-mode-syntax-table)
;;   )
;; 
;; (add-hook 'csharp-mode-hook 'my-csharp-mode)
;; (add-hook 'csharp-mode          'turn-on-font-lock t) 
;; 
;; (or (assoc "\\.cs$" auto-mode-alist)
;;     (setq auto-mode-alist (cons '("\\.cs$" . csharp-mode)
;; 				auto-mode-alist)))
;; 
;; ;; Teach emacs to parse csc warning/error messages
;; (add-hook
;;  'compilation-mode-hook
;;  (function
;;   (lambda ()
;;     (setq compilation-error-regexp-alist
;; 	  (cons
;; 	   ;;C# Compiler
;; 	   ;;t.cs(6,18): error SC1006: Name of constructor must match name of class
;; 	   '("^\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) CS[0-9]+:" 1 3 4)
;; 	   compilation-error-regexp-alist)))))
;;(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (setq auto-mode-alist 
;;      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(autoload 'powershell "powershell" "Run powershell as a shell within emacs." t) 
