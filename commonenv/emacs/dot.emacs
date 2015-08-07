;; -*- Mode: Emacs-Lisp -*-
;;(setq Info-default-directory-list (cons "/opt/gnu/info" Info-default-directory-list))

(setq commonenv-dir (or (getenv "COMMONENVDIR") (expand-file-name "~/commonenv/")))
(setq home-dir (expand-file-name "~/"))
(defun prompt-for-insert (prompt) (interactive)
  (let ((start (copy-marker (point)))
	end)
    (message prompt)
    (recursive-edit)
    (buffer-substring start (point))
    )
)

;;(prompt-for-insert "in dot.emacs")

;;; Define a variable to indicate whether we're running XEmacs/Lucid Emacs.
;;; (You do not have to defvar a global variable before using it --
;;; you can just call `setq' directly like we do for `emacs-major-version'
;;; below.  It's clearer this way, though.)

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;;; Older versions of emacs did not have these variables
;;; (emacs-major-version and emacs-minor-version.)
;;; Let's define them if they're not around, since they make
;;; it much easier to conditionalize on the emacs version.

(if (and (not (boundp 'emacs-major-version))
	 (string-match "^[0-9]+" emacs-version))
    (setq emacs-major-version
	  (string-to-int (substring emacs-version
				    (match-beginning 0) (match-end 0)))))
(if (and (not (boundp 'emacs-minor-version))
	 (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version))
    (setq emacs-minor-version
	  (string-to-int (substring emacs-version
				    (match-beginning 1) (match-end 1)))))

;;; Define a function to make it easier to check which version we're
;;; running.

(defun running-emacs-version-or-newer (major minor)
  (or (> emacs-major-version major)
      (and (= emacs-major-version major)
	   (>= emacs-minor-version minor))))

(put 'eval-expression 'disabled nil)

(setq minibuffer-max-depth nil)
(cond
 (running-xemacs
  (custom-set-variables
   '(user-mail-address "richard.l.ford@intel.com" t)
   '(query-user-mail-address nil))
  (custom-set-faces)
  ))


;; Options Menu Settings
;; =====================
(cond
 ((and (string-match "XEmacs" emacs-version)
       (boundp 'emacs-major-version)
       (or (and
            (= emacs-major-version 19)
            (>= emacs-minor-version 14))
           (= emacs-major-version 20))
       (fboundp 'load-options-file))
  (load-options-file (concat home-dir ".xemacs-options"))))
;; ============================
;; End of Options Menu Settings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			Basic Customization			    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable the command `narrow-to-region' ("C-x n n"), a useful
;; command, but possibly confusing to a new user, so it's disabled by
;; default.
(put 'narrow-to-region 'disabled nil)

;(prompt-for-insert "e2")

(setq load-path (append
		 (list
		  (concat commonenv-dir "/emacs/common")
		  "c:/User/Maxima-5.9.2/share/maxima/5.9.2/emacs"
		  )
		 load-path))

;(prompt-for-insert "b1")
(load "common-setup")
;(prompt-for-insert "b2")

(cond (running-xemacs
       ;;
       ;; Code for any version of XEmacs/Lucid Emacs goes here
       ;;
       ;;(setq frame-title-format (concat "%S " (getenv "REALUSER") "@" (getenv "HOST") ": %b"))
       (setq frame-title-format (if (string= buffer-file-name "") 
				    (concat (getenv "WHICHENV") " (%f) [%l]") 
				  (concat (getenv "WHICHENV") " (%b) [%l]")))
       (setq frame-icon-title-format ": %b")
       (setq load-path (append
			(list (concat commonenv-dir "/emacs/xemacs"))
			load-path))
       ;(prompt-for-insert "b3")
       (load "xemacs-setup")
       ;(prompt-for-insert "b4")
       )
      )

;(prompt-for-insert "b3")

(cond ((and running-xemacs
	    (running-emacs-version-or-newer 19 6))
       ;;
       ;; Code requiring XEmacs/Lucid Emacs version 19.6 or newer goes here
       ;;
       ))

(cond ((>= emacs-major-version 19)
       ;;
       ;; Code for any vintage-19 emacs goes here
       ;;
       ))

(cond ((and (not running-xemacs)
	    (>= emacs-major-version 19))
       ;;
       ;; Code specific to FSF Emacs 19 (not XEmacs/Lucid Emacs) goes here
       ;;
       ))

(cond ((< emacs-major-version 19)
       ;;
       ;; Code specific to emacs 18 goes here
       ;;
       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		Customization of Specific Packages		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Load gnuserv, which will allow you to connect to XEmacs sessions
;;; using `gnuclient'.

;; If you never run more than one XEmacs at a time, you might want to
;; always start gnuserv.  Otherwise it is preferable to specify
;; `-f gnuserv-start' on the command line to one of the XEmacsen.
;(load "gnuserv")
;(gnuserv-start)


;;; ********************
;;; Load a partial-completion mechanism, which makes minibuffer completion
;;; search multiple words instead of just prefixes; for example, the command
;;; `M-x byte-compile-and-load-file RET' can be abbreviated as `M-x b-c-a RET'
;;; because there are no other commands whose first three words begin with
;;; the letters `b', `c', and `a' respectively.
;;;
;(load-library "completer")


;;; ********************
;;; Load crypt, which is a package for automatically decoding and reencoding
;;; files by various methods - for example, you can visit a .Z or .gz file,
;;; edit it, and have it automatically re-compressed when you save it again.
;;; 
(setq crypt-encryption-type 'pgp   ; default encryption mechanism
      crypt-confirm-password t	   ; make sure new passwords are correct
      ;crypt-never-ever-decrypt t  ; if you don't encrypt anything, set this to
				   ; tell it not to assume that "binary" files
				   ; are encrypted and require a password.
      )
;(require 'crypt)


;;; ********************
;;; Edebug is a source-level debugger for emacs-lisp programs.
;;;
(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)


;;; ********************
;;; func-menu is a package that scans your source file for function
;;; definitions and makes a menubar entry that lets you jump to any
;;; particular function definition by selecting it from the menu.  The
;;; following code turns this on for all of the recognized languages.
;;; Scanning the buffer takes some time, but not much.
;;;
;;; Send bug reports, enhancements etc to:
;;; David Hughes <ukchugd@ukpmr.cs.philips.nl>
;;;
;(prompt-for-insert "b4")
(cond (running-xemacs
       (require 'func-menu)
       ;(define-key global-map 'f8 'function-menu)
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       ;(define-key global-map "\C-cl" 'fume-list-functions)
       ;(define-key global-map "\C-cg" 'fume-prompt-function-goto)

       ;; The Hyperbole information manager package uses (shift button2) and
       ;; (shift button3) to provide context-sensitive mouse keys.  If you
       ;; use this next binding, it will conflict with Hyperbole's setup.
       ;; Choose another mouse key if you use Hyperbole.
       (define-key global-map '(shift button3) 'mouse-function-menu)

       ;; For descriptions of the following user-customizable variables,
       ;; type C-h v <variable>
       (setq fume-max-items 25
             fume-fn-window-position 3
             fume-auto-position-popup t
             fume-display-in-modeline-p t
             fume-menubar-menu-location "File"
             fume-buffer-name "*Function List*"
             fume-no-prompt-on-valid-default nil)
       ))

;;; ********************
;;; resize-minibuffer-mode makes the minibuffer automatically
;;; resize as necessary when it's too big to hold its contents.

;(autoload 'resize-minibuffer-mode "rsz-minibuf" nil t)
;(resize-minibuffer-mode)
;(setq resize-minibuffer-window-exactly nil)

;(custom-set-variables
; '(verilog-indent-level 2)
; '(verilog-indent-level-behavorial 2)
; '(verilog-indent-level-module 0)
; '(Manual-use-rosetta-man t)
; '(verilog-indent-level-declaration 2)
; '(user-mail-address "richard.l.ford@intel.com" t)
; '(query-user-mail-address nil))
;(custom-set-faces
; '(font-lock-keyword-face ((t (:foreground "darkgoldenrod4"))))
; '(cperl-pod-face ((t (:foreground "red")))))

;; For XML
;(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
;(setq auto-mode-alist
;      (append
;       '(("\\.[sj]?html?\\'" . html-mode)
;	 ("\\.jsp\\'" . html-mode)
;	 ("\\.xml\\'" . xml-mode)
;	 ("\\.\\(?:sgml?\\|dtd\\)\\'" . sgml-mode)
;	 ("\\.t\\'" . perl-mode)
;	 )
;       auto-mode-alist))
;(prompt-for-insert "b5")

;;For OCAML
(setq load-path (append
		 (list (concat commonenv-dir "/emacs/xemacs/ocaml"))
		 load-path))
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode))
(autoload 'caml-mode "caml" "Major mode for editing OCaml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior OCaml process." t)
(autoload 'camldebug "camldebug" "Run ocamldebug on program." t)
(add-to-list 'interpreter-mode-alist '("ocamlrun" . caml-mode))
(add-to-list 'interpreter-mode-alist '("ocaml" . caml-mode))
;(prompt-for-insert "b6")
