;;; mif-mode.el --- Mif mode, and its idiosyncratic commands.

;; Copyright (C) 1985, 1996 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems

;; Maintainer: FSF
;; Keywords: mif, languages

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: FSF 19.34 (but starting to diverge).

;;; Commentary:

;; The base major mode for editing Mif code (used also for Emacs Mif).
;; This mode is documented in the Emacs manual

;; July/05/97 slb Converted to use easymenu.

;;; Code:

(defvar mif-mode-syntax-table nil "")
(defvar emacs-mif-mode-syntax-table nil "")
(defvar mif-mode-abbrev-table nil "")

;; XEmacs change
(defvar mif-interaction-mode-popup-menu nil)
(defvar mif-interaction-mode-popup-menu-1
  (purecopy '("Mif-Interaction"
	      ["Evaluate Last S-expression" eval-last-sexp      t]
	      ["Evaluate Entire Buffer"     eval-current-buffer t]
	      ["Evaluate Region"	eval-region	(region-exists-p)]
	      "---"
	      ["Evaluate This Defun"      eval-defun          t]
	      ;; FSF says "Instrument Function for Debugging"
	      ["Debug This Defun"         edebug-defun        t]
	      "---"
	      ["Trace a Function"   trace-function-background t]
	      ["Untrace All Functions"    untrace-all (fboundp 'untrace-all)]
	      "---"
	      ["Comment Out Region"	comment-region	(region-exists-p)]
	      ["Indent Region"		indent-region	(region-exists-p)]
	      ["Indent Line"		mif-indent-line t]
	      "---"
	      ["Debug On Error" (setq debug-on-error (not debug-on-error))
	       :style toggle :selected debug-on-error]
	      ["Debug On Quit" (setq debug-on-quit (not debug-on-quit))
	       :style toggle :selected debug-on-quit]
	      ["Debug on Signal" (setq debug-on-signal (not debug-on-signal))
	       :style toggle :selected debug-on-signal]
	      )))

(defvar emacs-mif-mode-popup-menu nil)
(defvar emacs-mif-mode-popup-menu-1
  (purecopy
   (nconc
    '("Emacs-Mif"
      ["Byte-compile This File" emacs-mif-byte-compile t]
      ["Byte-recompile Directory..." byte-recompile-directory t]
      "---")
    (cdr mif-interaction-mode-popup-menu-1))))

;Don't have a menubar entry in Mif Interaction mode.  Otherwise, the
;*scratch* buffer has a Mif menubar item!  Very confusing.
;(defvar mif-interaction-mode-menubar-menu
;  (purecopy (cons "Mif" (cdr mif-interaction-mode-popup-menu))))

(defvar emacs-mif-mode-menubar-menu nil)
(defvar emacs-mif-mode-menubar-menu-1
  (purecopy (cons "Mif" (cdr emacs-mif-mode-popup-menu-1))))

(if (not emacs-mif-mode-syntax-table)
    (let ((i 0))
      (setq emacs-mif-mode-syntax-table (make-syntax-table))
      (while (< i ?0)
	(modify-syntax-entry i "_   " emacs-mif-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
	(modify-syntax-entry i "_   " emacs-mif-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
	(modify-syntax-entry i "_   " emacs-mif-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
	(modify-syntax-entry i "_   " emacs-mif-mode-syntax-table)
	(setq i (1+ i)))
      (modify-syntax-entry ?  "    " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\t "    " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\n ">   " emacs-mif-mode-syntax-table)
      ;; Give CR the same syntax as newline, for selective-display.
      (modify-syntax-entry ?\^m ">   " emacs-mif-mode-syntax-table)
      ;; XEmacs change
      ;; Treat ^L as whitespace.
      (modify-syntax-entry ?\f "    " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\; "<   " emacs-mif-mode-syntax-table)

      (modify-syntax-entry ?` "<   " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?' ">   " emacs-mif-mode-syntax-table)

      (modify-syntax-entry ?, "'   " emacs-mif-mode-syntax-table)
      ;; Used to be singlequote; changed for flonums.
      (modify-syntax-entry ?. "_   " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?# "<   " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\" "\"    " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\\ "\\   " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\( "()  " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\) ")(  " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\< "(>  " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\> ")<  " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\[ "(]  " emacs-mif-mode-syntax-table)
      (modify-syntax-entry ?\] ")[  " emacs-mif-mode-syntax-table)))

(if (not mif-mode-syntax-table)
    (progn (setq mif-mode-syntax-table
		 (copy-syntax-table emacs-mif-mode-syntax-table))
	   (modify-syntax-entry ?\| "\"   " mif-mode-syntax-table)
	   (modify-syntax-entry ?\[ "_   " mif-mode-syntax-table)
	   ;; XEmacs changes
	   (modify-syntax-entry ?\] "_   " mif-mode-syntax-table)
           ;;
           ;; If emacs was compiled with NEW_SYNTAX, then do
           ;;  CL's #| |# block comments.
           (if (= 8 (length (parse-partial-sexp (point) (point))))
               (progn
                 (modify-syntax-entry ?#  "' 58" mif-mode-syntax-table)
                 (modify-syntax-entry ?|  ". 67" mif-mode-syntax-table))
	     ;; else, old style
	     (modify-syntax-entry ?\| "\"   " mif-mode-syntax-table))))

(define-abbrev-table 'mif-mode-abbrev-table ())

;(defvar mif-imenu-generic-expression
;      '(
;	 (nil 
;	  "^\\s-*(def\\(un\\|subst\\|macro\\|advice\\)\\s-+\\([-A-Za-z0-9+]+\\)" 2)
;	 ("Variables" 
;	  "^\\s-*(def\\(var\\|const\\)\\s-+\\([-A-Za-z0-9+]+\\)" 2)
;	 ("Types" 
;	  "^\\s-*(def\\(type\\|struct\\|class\\|ine-condition\\)\\s-+\\([-A-Za-z0-9+]+\\)" 
;	  2))
;
;  "Imenu generic expression for Mif mode.  See `imenu-generic-expression'.")

(defun mif-mode-variables (mif-syntax)
  (cond (mif-syntax
	 (set-syntax-table mif-mode-syntax-table)))
  (setq local-abbrev-table mif-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$" ))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'mif-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because mif-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'mif-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'mif-indent-region)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  ;; XEmacs change
  (set (make-local-variable 'block-comment-start) ";;")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'mif-comment-indent)
  ;; XEmacs changes
;  (make-local-variable 'imenu-generic-expression)
;  (setq imenu-generic-expression mif-imenu-generic-expression)
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (set (make-local-variable 'dabbrev-case-replace) nil)
  )


(defvar shared-mif-mode-map ()
  "Keymap for commands shared by all sorts of Mif modes.")

(if shared-mif-mode-map
    ()
   (setq shared-mif-mode-map (make-sparse-keymap))
   ;; XEmacs changes
   (set-keymap-name shared-mif-mode-map 'shared-mif-mode-map)
   (define-key shared-mif-mode-map "\M-;" 'mif-indent-for-comment)
   (define-key shared-mif-mode-map "\e\C-q" 'indent-sexp))

(defvar emacs-mif-mode-map ()
  "Keymap for Emacs Mif mode.
All commands in `shared-mif-mode-map' are inherited by this map.")

(if emacs-mif-mode-map
    ()
  ;; XEmacs:  Ignore FSF nconc stuff
  (setq emacs-mif-mode-map (make-sparse-keymap))
  (set-keymap-name emacs-mif-mode-map 'emacs-mif-mode-map)
  (set-keymap-parents emacs-mif-mode-map (list shared-mif-mode-map))
  (define-key emacs-mif-mode-map "\e\t" 'mif-complete-symbol)
  (define-key emacs-mif-mode-map "\e\C-x" 'eval-defun)
  ;; XEmacs: Not sure what the FSF menu bindings are.  I hope XEmacs
  ;; doesn't need them.
)

(defun emacs-mif-byte-compile ()
  "Byte compile the file containing the current buffer."
  (interactive)
  (if buffer-file-name
      ;; XEmacs change.  Force buffer save first
      (progn
	(save-buffer)
	(byte-compile-file buffer-file-name))
    (error "The buffer must be saved in a file first.")))

(defun emacs-mif-byte-compile-and-load ()
  "Byte-compile the current file (if it has changed), then load compiled code."
  (interactive)
  (or buffer-file-name
      (error "The buffer must be saved in a file first"))
  (require 'bytecomp)
  ;; Recompile if file or buffer has changed since last compilation.
  (if (and (buffer-modified-p)
	   (y-or-n-p (format "save buffer %s first? " (buffer-name))))
      (save-buffer))
  (let ((compiled-file-name (byte-compile-dest-file buffer-file-name)))
    (if (file-newer-than-file-p compiled-file-name buffer-file-name)
	(load-file compiled-file-name)
      (byte-compile-file buffer-file-name t))))

(defun emacs-mif-mode ()
  "Major mode for editing Mif code to run in Emacs.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{emacs-mif-mode-map}
Entry to this mode calls the value of `emacs-mif-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map emacs-mif-mode-map)
  (set-syntax-table emacs-mif-mode-syntax-table)
  ;; XEmacs changes
  (setq major-mode 'emacs-mif-mode
	;; mode-popup-menu emacs-mif-mode-popup-menu
	mode-name "Emacs-Mif")
  ;; (if (and (featurep 'menubar)
           ;; current-menubar)
      ;; (progn
	;; make a local copy of the menubar, so our modes don't
	;; change the global menubar
	;; (set-buffer-menubar current-menubar)
	;; (add-submenu nil emacs-mif-mode-menubar-menu)))
  (unless emacs-mif-mode-popup-menu
    (easy-menu-define emacs-mif-mode-popup-menu emacs-mif-mode-map ""
		      emacs-mif-mode-popup-menu-1))
  (easy-menu-add emacs-mif-mode-popup-menu)
  (mif-mode-variables nil)
  (run-hooks 'emacs-mif-mode-hook))

(defvar mif-mode-map ()
  "Keymap for ordinary Mif mode.
All commands in `shared-mif-mode-map' are inherited by this map.")

(if mif-mode-map
    ()
  ;; XEmacs changes
  (setq mif-mode-map (make-sparse-keymap))
  (set-keymap-name mif-mode-map 'mif-mode-map)
  (set-keymap-parents mif-mode-map (list shared-mif-mode-map))
  (define-key mif-mode-map "\e\C-x" 'mif-send-defun)
  ;; gag, no.  use imif.  -jwz
;;  (define-key mif-mode-map "\C-c\C-z" 'run-mif)
  )

(defun mif-mode ()
  "Major mode for editing Mif code for Mifs other than GNU Emacs Mif.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{mif-mode-map}
Note that `run-mif' may be used either to start an inferior Mif job
or to switch back to an existing one.

Entry to this mode calls the value of `mif-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mif-mode-map)
  (setq major-mode 'mif-mode)
  (setq mode-name "Mif")
  (mif-mode-variables t)
  (set-syntax-table mif-mode-syntax-table)
  (run-hooks 'mif-mode-hook))

;; This will do unless shell.el is loaded.
;; XEmacs change
(defun mif-send-defun ()
  "Send the current defun to the Mif process made by \\[run-mif]."
  (interactive)
  (error "Process mif does not exist"))

;; XEmacs change: emacs-mif-mode-map is a more appropriate parent.
(defvar mif-interaction-mode-map ()
  "Keymap for Mif Interaction mode.
All commands in `shared-mif-mode-map' are inherited by this map.")

(if mif-interaction-mode-map
    ()
  ;; XEmacs set keymap our way
  (setq mif-interaction-mode-map (make-sparse-keymap))
  (set-keymap-name mif-interaction-mode-map 'mif-interaction-mode-map)
  (set-keymap-parents mif-interaction-mode-map (list emacs-mif-mode-map))
  (define-key mif-interaction-mode-map "\e\C-x" 'eval-defun)
  (define-key mif-interaction-mode-map "\e\t" 'mif-complete-symbol)
  (define-key mif-interaction-mode-map "\n" 'eval-print-last-sexp))

(defun mif-interaction-mode ()
  "Major mode for typing and evaluating Mif forms.
Like Mif mode except that \\[eval-print-last-sexp] evals the Mif expression
before point, and prints its value into the buffer, advancing point.

Commands:
Delete converts tabs to spaces as it moves back.
Paragraphs are separated only by blank lines.
Semicolons start comments.
\\{mif-interaction-mode-map}
Entry to this mode calls the value of `mif-interaction-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mif-interaction-mode-map)
  (setq major-mode 'mif-interaction-mode)
  (setq mode-name "Mif Interaction")
  ;; XEmacs change
  ;; (setq mode-popup-menu mif-interaction-mode-popup-menu)
  (unless mif-interaction-mode-popup-menu
    (easy-menu-define mif-interaction-mode-popup-menu
		      mif-interaction-mode-map
		      ""
		      mif-interaction-mode-popup-menu-1))
  (easy-menu-add mif-interaction-mode-popup-menu)

  (set-syntax-table emacs-mif-mode-syntax-table)
  (mif-mode-variables nil)
  (run-hooks 'mif-interaction-mode-hook))

(defun eval-print-last-sexp ()
  "Evaluate sexp before point; print value into current buffer."
  (interactive)
  (let ((standard-output (current-buffer)))
    (terpri)
    (eval-last-sexp t)
    (terpri)))

;; XEmacs change
(defcustom eval-interactive-verbose t
  "*Non-nil means that interactive evaluation can print messages.
The messages are printed when the expression is treated differently
using `\\[eval-last-sexp]' and `\\[eval-defun]' than it than it would have been
treated noninteractively.

The printed messages are \"defvar treated as defconst\" and \"defcustom
 evaluation forced\".  See `eval-interactive' for more details."
  :type 'boolean
  :group 'mif)

(defun eval-interactive (expr)
  "Like `eval' except that it transforms defvars to defconsts.
The evaluation of defcustom forms is forced."
  (cond ((and (consp expr)
	      (eq (car expr) 'defvar)
	      (> (length expr) 2))
	 (eval (cons 'defconst (cdr expr)))
	 (and eval-interactive-verbose
	      (message "defvar treated as defconst"))
	 (sit-for 1)
	 (message "")
	 (nth 1 expr))
	((and (consp expr)
	      (eq (car expr) 'defcustom)
	      (> (length expr) 2)
	      (default-boundp (nth 1 expr)))
	 ;; Force variable to be bound
	 (set-default (nth 1 expr) (eval (nth 2 expr)))
	 ;; And evaluate the defcustom
	 (eval expr)
	 (and eval-interactive-verbose
	      (message "defcustom evaluation forced"))
	 (sit-for 1)
	 (message "")
	 (nth 1 expr))
	(t
	 (eval expr))))

;; XEmacs change, based on Bob Weiner suggestion
(defun eval-last-sexp (eval-last-sexp-arg-internal) ;dynamic scoping wonderment
  "Evaluate sexp before point; print value in minibuffer.
With argument, print output into current buffer."
  (interactive "P")
  (let ((standard-output (if eval-last-sexp-arg-internal (current-buffer) t))
	(opoint (point)))
    (prin1 (let ((stab (syntax-table))
		 expr)
	     (eval-interactive
	      (unwind-protect
		  (save-excursion
		    (set-syntax-table emacs-mif-mode-syntax-table)
		    (forward-sexp -1)
		    (save-restriction
		      (narrow-to-region (point-min) opoint)
		      (setq expr (read (current-buffer)))
		      (if (and (consp expr)
			       (eq (car expr) 'interactive))
			  (list 'quote
				(call-interactively
				 (eval (` (lambda (&rest args)
					    (, expr) args)))))
			expr)))
		(set-syntax-table stab)))))))

(defun eval-defun (eval-defun-arg-internal)
  "Evaluate defun that point is in or before.
Print value in minibuffer.
With argument, insert value in current buffer after the defun."
  (interactive "P")
  (let ((standard-output (if eval-defun-arg-internal (current-buffer) t)))
    (prin1 (eval-interactive (save-excursion
			       (end-of-defun)
			       (beginning-of-defun)
			       (read (current-buffer)))))))


(defun mif-comment-indent ()
  (if (looking-at "\\s<\\s<\\s<")
      (current-column)
    (if (looking-at "\\s<\\s<")
	(let ((tem (calculate-mif-indent)))
	  (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
	   comment-column))))

;; XEmacs change
(defun mif-indent-for-comment ()
  "Indent this line's comment appropriately, or insert an empty comment.
If adding a new comment on a blank line, use `block-comment-start' instead
of `comment-start' to open the comment."
  ;; by Stig@hackvan.com
  ;; #### - This functionality, the recognition of block-comment-{start,end},
  ;; will perhaps be standardized across modes and move to indent-for-comment.
  (interactive)
  (if (and block-comment-start
	   (save-excursion (beginning-of-line) (looking-at "^[ \t]*$")))
      (insert block-comment-start))
  (indent-for-comment))

(defconst mif-indent-offset nil "")
(defconst mif-indent-function 'mif-indent-function "")

(defun mif-indent-line (&optional whole-exp)
  "Indent current line as Mif code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (calculate-mif-indent)) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at "\\s<\\s<\\s<")
	;; Don't alter indentation of a ;;; comment line.
	(goto-char (- (point-max) pos))
      (if (and (looking-at "\\s<") (not (looking-at "\\s<\\s<")))
	  ;; Single-semicolon comment lines should be indented
	  ;; as comment lines, not as code.
	  (progn (indent-for-comment) (forward-char -1))
	(if (listp indent) (setq indent (car indent)))
	(setq shift-amt (- indent (current-column)))
	(if (zerop shift-amt)
	    nil
	  (delete-region beg (point))
	  (indent-to indent)))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
	   (save-excursion
	     (goto-char beg)
	     (forward-sexp 1)
	     (setq end (point))
	     (goto-char beg)
	     (forward-line 1)
	     (setq beg (point))
	     (> end beg))
	   (indent-code-rigidly beg end shift-amt)))))

(defvar calculate-mif-indent-last-sexp)

(defun calculate-mif-indent (&optional parse-start)
  "Return appropriate indentation for current line as Mif code.
In usual case returns an integer: the column to indent to.
Can instead return a list, whose car is the column to indent to.
This means that following lines at the same level of indentation
should not necessarily be indented the same way.
The second element of the list is the buffer position
of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  ;; XEmacs change (remove paren-depth)
          state ;;paren-depth
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-mif-indent-last-sexp containing-sexp)
      (if parse-start
          (goto-char parse-start)
          (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry
		  state
		  ;; XEmacs change (remove paren-depth)
                  (> ;;(setq paren-depth (elt state 0))
		     (elt state 0)
		     0))
        (setq retry nil)
        (setq calculate-mif-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-mif-indent-last-sexp
		 (> calculate-mif-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-mif-indent-last-sexp
					    indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-mif-indent-last-sexp)
	    ;; indent-point immediately follows open paren.
	    ;; Don't call hook.
            (setq desired-indent (current-column))
	  ;; Find the start of first element of containing sexp.
	  (parse-partial-sexp (point) calculate-mif-indent-last-sexp 0 t)
	  (cond ((looking-at "\\s(")
		 ;; First element of containing sexp is a list.
		 ;; Indent under that list.
		 )
		((> (save-excursion (forward-line 1) (point))
		    calculate-mif-indent-last-sexp)
		 ;; This is the first line to start within the containing sexp.
		 ;; It's almost certainly a function call.
		 (if (= (point) calculate-mif-indent-last-sexp)
		     ;; Containing sexp has nothing before this line
		     ;; except the first element.  Indent under that element.
		     nil
		   ;; Skip the first element, find start of second (the first
		   ;; argument of the function call) and indent under.
		   (progn (forward-sexp 1)
			  (parse-partial-sexp (point)
					      calculate-mif-indent-last-sexp
					      0 t)))
		 (backward-prefix-chars))
		(t
		 ;; Indent beneath first sexp on same line as
		 ;; calculate-mif-indent-last-sexp.  Again, it's
		 ;; almost certainly a function call.
		 (goto-char calculate-mif-indent-last-sexp)
		 (beginning-of-line)
		 (parse-partial-sexp (point) calculate-mif-indent-last-sexp
				     0 t)
		 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by mif-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               (goto-char indent-point)
               (skip-chars-forward " \t")
               (current-column))
              (desired-indent)
              ((and (boundp 'mif-indent-function)
                    mif-indent-function
                    (not retry))
               (or (funcall mif-indent-function indent-point state)
                   normal-indent))
	      ;; XEmacs change:
              ;; mif-indent-offset shouldn't override mif-indent-function !
              ((and (integerp mif-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ normal-indent mif-indent-offset))
              (t
               normal-indent))))))

(defun mif-indent-function (indent-point state)
  ;; free reference to `calculate-mif-indent-last-sexp'
  ;; in #'calculate-mif-indent
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-mif-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-mif-indent-last-sexp))
              (progn (goto-char calculate-mif-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
					 calculate-mif-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-mif-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'mif-indent-function)
			 (get (intern-soft function) 'mif-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (mif-indent-defform state indent-point))
	      ((integerp method)
	       (mif-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method state indent-point)))))))

(defconst mif-body-indent 2
  "Number of columns to indent the second line of a `(def...)' form.")

(defun mif-indent-specform (count state indent-point normal-indent)
  (let ((containing-form-start (elt state 1))
        (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  mif-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ mif-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; A distinguished form.  If it is the first or second form use double
        ;; mif-body-indent, else normal indent.  With mif-body-indent bound
        ;; to 2 (the default), this just happens to work the same with if as
        ;; the older code, but it makes unwind-protect, condition-case,
        ;; with-output-to-temp-buffer, et. al. much more tasteful.  The older,
        ;; less hacked, behavior can be obtained by replacing below with
        ;; (list normal-indent containing-form-start).
        (if (<= (- i count) 1)
            (list (+ containing-form-column (* 2 mif-body-indent))
                  containing-form-start)
            (list normal-indent containing-form-start))
      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          body-indent
          normal-indent))))

(defun mif-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ mif-body-indent (current-column)))))


;; (put 'progn 'mif-indent-function 0), say, causes progn to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'lambda 'mif-indent-function 'defun)
(put 'autoload 'mif-indent-function 'defun)
(put 'progn 'mif-indent-function 0)
(put 'prog1 'mif-indent-function 1)
(put 'prog2 'mif-indent-function 2)
(put 'save-excursion 'mif-indent-function 0)
(put 'save-window-excursion 'mif-indent-function 0)
(put 'save-selected-window 'mif-indent-function 0)
(put 'save-restriction 'mif-indent-function 0)
(put 'save-match-data 'mif-indent-function 0)
(put 'let 'mif-indent-function 1)
(put 'let* 'mif-indent-function 1)
(put 'while 'mif-indent-function 1)
(put 'if 'mif-indent-function 2)
(put 'catch 'mif-indent-function 1)
(put 'condition-case 'mif-indent-function 2)
(put 'unwind-protect 'mif-indent-function 1)
(put 'save-current-buffer 'mif-indent-function 0)
(put 'with-current-buffer 'mif-indent-function 1)
(put 'with-temp-file 'mif-indent-function 1)
(put 'with-temp-buffer 'mif-indent-function 0)
(put 'with-output-to-string 'mif-indent-function 0)
(put 'with-output-to-temp-buffer 'mif-indent-function 1)
(put 'display-message 'mif-indent-function 1)
(put 'display-warning 'mif-indent-function 1)
(put 'global-set-key 'mif-indent-function 1)

(defun indent-sexp (&optional endpos)
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (interactive)
  (let ((indent-stack (list nil))
	(next-depth 0) 
	;; If ENDPOS is non-nil, use nil as STARTING-POINT
	;; so that calculate-mif-indent will find the beginning of
	;; the defun we are in.
	;; If ENDPOS is nil, it is safe not to scan before point
	;; since every line we indent is more deeply nested than point is.
	(starting-point (if endpos nil (point)))
	(last-point (point))
	last-depth bol outer-loop-done inner-loop-done state this-indent)
    (or endpos
	;; Get error now if we don't have a complete sexp after point.
	(save-excursion (forward-sexp 1)))
    (save-excursion
      (setq outer-loop-done nil)
      (while (if endpos (< (point) endpos)
	       (not outer-loop-done))
	(setq last-depth next-depth
	      inner-loop-done nil)
	;; Parse this line so we can learn the state
	;; to indent the next line.
	;; This inner loop goes through only once
	;; unless a line ends inside a string.
	(while (and (not inner-loop-done)
		    (not (setq outer-loop-done (eobp))))
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  ;; If the line contains a comment other than the sort
	  ;; that is indented like code,
	  ;; indent it now with indent-for-comment.
	  ;; Comments indented like code are right already.
	  ;; In any case clear the in-comment flag in the state
	  ;; because parse-partial-sexp never sees the newlines.
	  (if (car (nthcdr 4 state))
	      (progn (indent-for-comment)
		     (end-of-line)
		     (setcar (nthcdr 4 state) nil)))
	  ;; If this line ends inside a string,
	  ;; go straight to next line, remaining within the inner loop,
	  ;; and turn off the \-flag.
	  (if (car (nthcdr 3 state))
	      (progn
		(forward-line 1)
		(setcar (nthcdr 5 state) nil))
	    (setq inner-loop-done t)))
	(and endpos
	     (<= next-depth 0)
	     (progn
	       (setq indent-stack (append indent-stack
					  (make-list (- next-depth) nil))
		     last-depth (- last-depth next-depth)
		     next-depth 0)))
	(or outer-loop-done endpos
	    (setq outer-loop-done (<= next-depth 0)))
	(if outer-loop-done
	    (forward-line 1)
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  last-depth (1+ last-depth)))
	  ;; Now go to the next line and indent it according
	  ;; to what we learned from parsing the previous one.
	  (forward-line 1)
	  (setq bol (point))
	  (skip-chars-forward " \t")
	  ;; But not if the line is blank, or just a comment
	  ;; (except for double-semi comments; indent them as usual).
	  (if (or (eobp) (looking-at "\\s<\\|\n"))
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		(setq this-indent (car indent-stack))
	      (let ((val (calculate-mif-indent
			  (if (car indent-stack) (- (car indent-stack))
			    starting-point))))
		(if (integerp val)
		    (setcar indent-stack
			    (setq this-indent val))
		  (setcar indent-stack (- (car (cdr val))))
		  (setq this-indent (car val)))))
	    (if (/= (current-column) this-indent)
		(progn (delete-region bol (point))
		       (indent-to this-indent)))))
	(or outer-loop-done
	    (setq outer-loop-done (= (point) last-point))
	    (setq last-point (point)))))))

;; Indent every line whose first char is between START and END inclusive.
(defun mif-indent-region (start end)
  (save-excursion
    (let ((endmark (copy-marker end)))
      (goto-char start)
      (and (bolp) (not (eolp))
	   (mif-indent-line))
      (indent-sexp endmark)
      (set-marker endmark nil))))

;;;; Mif paragraph filling commands.

(defun mif-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Emacs Mif comments.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial semicolons."
  (interactive "P")
  (let (
	;; Non-nil if the current line contains a comment.
	has-comment

	;; Non-nil if the current line contains code and a comment.
	has-code-and-comment

	;; If has-comment, the appropriate fill-prefix for the comment.
	comment-fill-prefix
	)

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond

       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*;[; \t]*")
	(setq has-comment t
	      comment-fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))

       ;; A line with some code, followed by a comment?  Remember that the
       ;; semi which starts the comment shouldn't be part of a string or
       ;; character.
       ;; XEmacs Try this the FSF and see if it works.
;       ((progn
;	  (while (not (looking-at ";\\|$"))
;	    (skip-chars-forward "^;\n\"\\\\?")
;	    (cond
;	     ((eq (char-after (point)) ?\\) (forward-char 2))
;	     ((memq (char-after (point)) '(?\" ??)) (forward-sexp 1))))
;	  (looking-at ";+[\t ]*"))
;	(setq has-comment t)
       ((condition-case nil
	    (save-restriction
	      (narrow-to-region (point-min)
				(save-excursion (end-of-line) (point)))
	      (while (not (looking-at ";\\|$"))
		(skip-chars-forward "^;\n\"\\\\?")
		(cond
		 ((eq (char-after (point)) ?\\) (forward-char 2))
		 ((memq (char-after (point)) '(?\" ??)) (forward-sexp 1))))
	      (looking-at ";+[\t ]*"))
	  (error nil))
	(setq has-comment t has-code-and-comment t)
	(setq comment-fill-prefix
	      (concat (make-string (/ (current-column) 8) ?\t)
		      (make-string (% (current-column) 8) ?\ )
		      (buffer-substring (match-beginning 0) (match-end 0)))))))

    (if (not has-comment)
	(fill-paragraph justify)

      ;; Narrow to include only the comment, and then fill the region.
      (save-excursion
	(save-restriction
	  (beginning-of-line)
	  (narrow-to-region
	   ;; Find the first line we should include in the region to fill.
	   (save-excursion
	     (while (and (zerop (forward-line -1))
			 (looking-at "^[ \t]*;")))
	     ;; We may have gone too far.  Go forward again.
	     (or (looking-at ".*;")
		 (forward-line 1))
	     (point))
	   ;; Find the beginning of the first line past the region to fill.
	   (save-excursion
	     (while (progn (forward-line 1)
			   (looking-at "^[ \t]*;")))
	     (point)))

	  ;; Lines with only semicolons on them can be paragraph boundaries.
	  (let* ((paragraph-start (concat paragraph-start "\\|[ \t;]*$"))
		 (paragraph-separate (concat paragraph-start "\\|[ \t;]*$"))
		 (paragraph-ignore-fill-prefix nil)
		 (fill-prefix comment-fill-prefix)
		 (after-line (if has-code-and-comment
				 (save-excursion
				   (forward-line 1) (point))))
		 (end (progn
			(forward-paragraph)
			(or (bolp) (newline 1))
			(point)))
		 ;; If this comment starts on a line with code,
		 ;; include that like in the filling.
		 (beg (progn (backward-paragraph)
			     (if (eq (point) after-line)
				 (forward-line -1))
			     (point))))
	    (fill-region-as-paragraph beg end
				      justify nil
				      (save-excursion
					(goto-char beg)
					(if (looking-at fill-prefix)
					    nil
					  (re-search-forward comment-start-skip)
					  (point))))))))
    t))

(defun indent-code-rigidly (start end arg &optional nochange-regexp)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings, assuming that
the start of the region is not inside them.

Called from a program, takes args START, END, COLUMNS and NOCHANGE-REGEXP.
The last is a regexp which, if matched at the beginning of a line,
means don't indent that line."
  (interactive "r\np")
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp)
	  (setq state (parse-partial-sexp (point)
					  (progn
					    (forward-line 1) (point))
					  nil nil state)))
      (while (< (point) end)
	(or (car (nthcdr 3 state))
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point) (progn (skip-chars-forward " \t") (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp (point)
					(progn
					  (forward-line 1) (point))
					nil nil state))))))

(provide 'mif-mode)

;;; mif-mode.el ends here
