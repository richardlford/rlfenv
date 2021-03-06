;;; dbx.el --- run dbx under Emacs

;; Copyright (C) 1988 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA (umerin@flab.fujitsu.junet)
;; Keywords: c, unix, tools, debugging

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

;;; Synched up with: Not in FSF

;;; Code:

(require 'comint)

(defvar dbx-trace-flag nil
  "Dbx trace switch.")

(defvar dbx-process nil
  "The process in which dbx is running.")

(defvar dbx-break-point
  "stopped in .* at line \\([0-9]*\\) in file \"\\([^\"]*\\)\""
  "Regexp of pattern that dbx writes at break point.")

(defvar inferior-dbx-mode-map nil)
(if inferior-dbx-mode-map
    nil
  (setq inferior-dbx-mode-map (make-sparse-keymap))
  (set-keymap-name inferior-dbx-mode-map 'inferior-dbx-mode-map)
  (set-keymap-parent inferior-dbx-mode-map comint-mode-map)
  (define-key inferior-dbx-mode-map "\C-c\C-w" 'dbx-where)
  (define-key inferior-dbx-mode-map "\C-c\C-t" 'dbx-trace-mode)
  (define-key ctl-x-map " " 'dbx-stop-at))

(defun inferior-dbx-mode ()
  "Major mode for interacting with an inferior dbx process.

The following commands are available:
\\{inferior-dbx-mode-map}

Entry to this mode calls the value of dbx-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of comint-mode-hook.
dbx-mode-hook is called after comint-mode-hook.

You can display the debugging program in other window and point out
where you are looking at using the command \\[dbx-where].

\\[dbx-trace-mode] toggles dbx-trace mode. In dbx-trace mode,
debugging program is automatically traced using output from dbx.

The command \\[dbx-stop-at] sets break point at current line of the
program in the buffer. Major mode name of the buffer must be in
dbx-language-mode-list.

Commands:

Return at end of buffer sends line as input.
Return not at end copies line, sans any dbx prompt, to end and sends it.
\\[shell-send-eof] sends end-of-file as input.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops, likewise. \\[comint-quit-subjob] sends quit signal, likewise.
\\[dbx-where] displays debugging program in other window and
 points out where you are looking at.
\\[dbx-trace-mode] toggles dbx-trace mode.
\\[dbx-stop-at] sets break point at current line."
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (use-local-map inferior-dbx-mode-map)
  (setq major-mode 'inferior-dbx-mode
	mode-name "Inferior dbx"
	comint-prompt-regexp "^[^)]*dbx) *")
  (make-local-variable 'dbx-trace-flag)
  (or (assq 'dbx-trace-flag minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(dbx-trace-flag " Trace") minor-mode-alist)))
  (run-hooks 'dbx-mode-hook))

(defun run-dbx (path)
  "Run inferior dbx process on PROGRAM, with I/O via buffer *dbx-PROGRAM*."
  (interactive "fProgram to debug: ")
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*dbx-" file "*"))
    (setq default-directory (file-name-directory path))
    (switch-to-buffer (make-comint (concat "dbx-" file) "dbx" nil file)))
  (setq dbx-process (get-buffer-process (current-buffer)))
  (set-process-filter dbx-process 'dbx-filter)
  (inferior-dbx-mode))

(defun dbx-trace-mode (arg)
  "Toggle dbx-trace mode.
With arg, turn dbx-trace mode on iff arg is positive.
In dbx-trace mode, user program is automatically traced."
  (interactive "P")
  (if (not (eql major-mode 'inferior-dbx-mode))
      (error "dbx-trace mode is effective in inferior-dbx mode only."))
  (setq dbx-trace-flag
	(if (null arg)
	    (not dbx-trace-flag)
	  (> (prefix-numeric-value arg) 0)))
  ;; Force mode line redisplay
  (set-buffer-modified-p (buffer-modified-p)))

(defun dbx-filter (process string)
  "Trace debugging program automatically if dbx-trace-flag is not nil."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (let ((beg (point)))
      (insert-before-markers string)
      (if dbx-trace-flag		;Trace mode is on?
	  (dbx-where beg t)))
    (if (process-mark process)
	(set-marker (process-mark process) (point-max))))
  (if (eq (process-buffer process)
	  (current-buffer))
      (goto-char (point-max)))
  )

(defun dbx-where (&optional begin quiet)
  "Display dbx'ed program in other window and point out where you are looking.
BEGIN bounds the search. If QUIET, just return nil (no error) if fail."
  (interactive)
  (let (file line)
    (save-excursion
      (if (re-search-backward dbx-break-point begin quiet)
	  (progn
	    (setq line (buffer-substring (match-beginning 1) (match-end 1)))
	    (setq file (buffer-substring (match-beginning 2) (match-end 2)))
	    )))
    (if (and file line)			;Find break point?
	(progn
	  (find-file-other-window (expand-file-name file nil))
	  (goto-line (string-to-int line)) ;Jump to the line
	  (beginning-of-line)
	  (setq overlay-arrow-string "=>")
	  (or overlay-arrow-position 
	      (setq overlay-arrow-position (make-marker)))
	  (set-marker overlay-arrow-position (point) (current-buffer))
	  (other-window 1))		;Return to dbx
      )))

(defun dbx-stop-at ()
  "Set break point at current line."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(1+ (count-lines 1 (point))))))
    (process-send-string dbx-process
			 (concat "stop at \"" file-name "\":" line "\n"))))

(provide 'dbx)

;;; dbx.el ends here
