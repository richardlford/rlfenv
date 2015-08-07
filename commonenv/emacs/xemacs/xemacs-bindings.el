;;; Bindings for xemacs.

;;; Documentation for mouse bindings;
;;; 
;;;     Modifers | #/u | Function
;;; A H S M C Sh |     | 
;;; --------------------------------------------------
;;; - - - - - -- |  1  | mouse-track
;;; - - - - - -- |  2  | mouse-yank
;;; - - - - - -- |  3  | popup-mode-menu

;;; - - - - - Sh |  1  | mouse-track-adjust
;;; - - - - - Sh |  2  | mouse-track-copy
;;; - - - - - Sh |  3  | mouse-function-menu

;;; - - - - C -- |  1  | mouse-track-insert
;;; - - - - C -- |  2  | x-set-point-and-move-selection
;;; - - - - C -- |  3  | 

;;; - - - - C Sh |  1  | mouse-track-delete-and-insert
;;; - - - - C Sh |  2  | mouse-track-delete
;;; - - - - C Sh |  3  | 

;;; - - - M - -- |  1  | mouse-track-do-rectangle
;;; - - - M - -- |  2  | mouse-isearch-thing-word ;;Overridden by next entry for now.
;;; - - _ M - -- |  2  | my-dired-up
;;; - - - M - -- |  3  | 

;;; - - - M - Sh |  1  | mouse-isearch-thing
;;; - - - M - Sh |  2  | mouse-scroll
;;; - - - M - Sh |  3  | mouse-vscroll

;;; - - - M C -- |  1  | find tag
;;; - - - M C -- |  2  | find tag other frame
;;; - - - M C -- |  3  | popup-buffer-menu

;;; - - - M C Sh |  1  | Set point, exec M-kp-1
;;; - - - M C Sh |  2  | Set point, exec M-kp-2
;;; - - - M C Sh |  3  | Set point, exec M-kp-3

;;; - - S - - -- |  1  | scroll-up
;;; - - S - - -- |  2  | my-dired-up
;;; - - S - - -- |  3  | scroll-down

;;; - - S - - Sh |  1  | Set point, exec S-kp-1
;;; - - S - - Sh |  2  | Set point, exec S-kp-2
;;; - - S - - Sh |  3  | Set point, exec S-kp-3

;;; - - S - C -- |  1  | yank from kill ring, not selection
;;; - - S - C -- |  2  | mouse-select-thing (selected and copied)
;;; - - S - C -- |  3  | mouse-select-thing
;;; - - S - C -- |  3u | mouse-thing-copy

;;; - - S - C Sh |  1  | 
;;; - - S - C Sh |  2  | 
;;; - - S - C Sh |  3  | 

;;; - - S M - -- |  1  | mouse-select-thing
;;; - - S M - -- |  1u | mouse-thing-grab
;;; - - S M - -- |  2  | mouse-select-thing
;;; - - S M - -- |  2u | mouse-thing-kill
;;; - - S M - -- |  3  | 

;;; - H - - - -- |  1  | gdbsrc-print-csexp
;;; - H - - - -- |  2  | gdbsrc-*print-csexppgopti\
;;; - H - - - -- |  3  | 

;;; - H - - - Sh |  1  | Set point, exec H-kp-1
;;; - H - - - Sh |  2  | Set point, exec H-kp-2
;;; - H - - - Sh |  3  | Set point, exec H-kp-3

;;; A - - - - -- |  1  | gdbsrc-set-break
;;; A - - - - -- |  2  | gdbsrc-set-tbreak-continue
;;; A - - - - -- |  3  | 

;;; A - - - - Sh |  1  | dtwm: f.move
;;; A - - - - Sh |  2  | dtwm: f.restore
;;; A - - - - Sh |  3  | dtwm: f.minimize
;;; 

;; Make the sequence "C-x w" execute the `what-line' command, 
;; which prints the current line number in the echo area.
(global-set-key "\C-xw" 'what-line)

;; When editing C code (and Lisp code and the like), I often
;; like to insert tabs into comments and such.  It gets to be
;; a pain to always have to use `C-q TAB', so I set up a more
;; convenient binding.  Note that this does not work in
;; TTY frames, where tab and shift-tab are indistinguishable.
(define-key global-map '(shift tab) 'self-insert-command)


(global-set-key [(super \2)] 'make-frame)
(global-set-key [(super b)] 'show-buffer-filename)
(global-set-key [(super e)] 'toggle-debug-on-error)
(global-set-key [(super f)] 'find-my-tags)
(global-set-key [(super i)] 'insert-buffer)
(global-set-key [(super j)] 'newline-and-auto-fill)
(global-set-key [(super k)] 'zap-line)
(global-set-key [(super l)] 'toggle-truncate-lines)
(global-set-key [(super r)] 'isearch-backward)
(global-set-key [(super s)] 'isearch-forward)
(global-set-key [(super t)] 'toggle-tags-always-exact)
(global-set-key [(alt tab)] 'indent-relative)

;; Mode specific
;;
(define-key isearch-mode-map [(super g)] 'isearch-repeat-backward)
(define-key isearch-mode-map [(super r)] 'isearch-repeat-backward)
(define-key isearch-mode-map [(super s)] 'isearch-repeat-forward)

(define-key dired-mode-map [button2] 'nil)
(define-key dired-mode-map [button2up] (mousefun-set-point (dired-find-file)))
(define-key dired-mode-map '(meta button2) (mousefun-set-point (my-dired-up)))
(define-key dired-mode-map '(meta button2up) 'nil)

;(define-key Buffer-menu-mode-map [(meta button2)]  (mousefun-set-point (Buffer-menu-this-window)))

; Undo bindings for gdbsrc mode. I use my own.
(cond
 ((boundp 'gdbsrc-global-map)
  (define-key gdbsrc-global-map '(meta button1) nil)
  (define-key gdbsrc-global-map '(hyper button1) 'gdbsrc-print-csexp)
  (define-key gdbsrc-global-map '(meta button2) nil)
  (define-key gdbsrc-global-map '(hyper button2) 'gdbsrc-*print-csexp)
  (define-key gdbsrc-global-map '(meta shift button1) nil)
  (define-key gdbsrc-global-map '(alt button1) 'gdbsrc-set-break)
  (define-key gdbsrc-global-map '(meta shift button2) nil)
  (define-key gdbsrc-global-map '(alt button2) 'gdbsrc-set-tbreak-continue)
  ))

;;(define-key verilog-mode-map '(meta tab) 'indent-relative)

(add-hook 'c++-mode-hook
	  (lambda ()
	    (define-key c++-mode-map [(super ?\\)] 'backslashify-region)))

(global-set-key [(meta tab)] 'indent-relative)

;
; Keypad and Function Key Bindings.
;

(global-set-key [(kp-0)] 'undo)
;(global-set-key [(shift kp-0)] 'zap-line)
;(global-set-key [(meta kp-0)] 'unzap-line)
;(global-set-key [(control kp-0)] 'pascal-narrow-proc)
;(global-set-key [(meta shift kp-0)] 'c-quote-line)
;(global-set-key [(control shift kp-0)] 'nil)

(global-set-key [(kp-1)] 're-search-forward)
(global-set-key [(shift kp-1)] 're-search-backward)
;(global-set-key [(meta kp-1)] 'nil)
;(global-set-key [(control kp-1)] 'pascal-make-ext)
;(global-set-key [(meta shift kp-1)] 'nil)
;(global-set-key [(control shift kp-1)] 'nil)

(global-set-key [(kp-2)] 'what-line)
;(global-set-key [(shift kp-2)] 'pascal-decl)
;(global-set-key [(meta kp-2)] 'nil)
;(global-set-key [(control kp-2)] 'latexoccur)
;(global-set-key [(meta shift kp-2)] 'nil)
;(global-set-key [(control shift kp-2)] 'nil)

(global-set-key [(kp-3)] 'lisp-complete-symbol)
(global-set-key [(shift kp-3)] 'bind-last-kbd-macro)
(global-set-key [(super kp-3)] 'bind-last-kbd-macro)
;(global-set-key [(meta kp-3)] 'nil)
;(global-set-key [(control kp-3)] 'comm-pipes)
;(global-set-key [(meta shift kp-3)] 'nil)
;(global-set-key [(control shift kp-3)] 'nil)

;(global-set-key [(kp-4)] 'RLK-search-backward-again)
;(global-set-key [(shift kp-4)] 'RLK-search-backward)
;(global-set-key [(meta kp-4)] 'nil)
(global-set-key [(control kp-4)] 'nil)
;(global-set-key [(meta shift kp-4)] 'nil)
;(global-set-key [(control shift kp-4)] 'nil)

;(defun fixsyntax () (interactive) (modify-syntax-entry ?_ "_"))
(global-set-key [(kp-5)] 'recenter)
;(global-set-key [(shift kp-5)] 'fixsyntax)
;(global-set-key [(meta kp-5)] 'nil)
(global-set-key [(control kp-5)] 'nil)
;(global-set-key [(meta shift kp-5)] 'nil)
;(global-set-key [(control shift kp-5)] 'nil)

;(global-set-key [(kp-6)] 'RLK-search-forward-again)
;(global-set-key [(shift kp-6)] 'RLK-search-forward)
;(global-set-key [(meta kp-6)] 'nil)
(global-set-key [(control kp-6)] 'nil)
;(global-set-key [(meta shift kp-6)] 'nil)
;(global-set-key [(control shift kp-6)] 'nil)

;(global-set-key [(kp-7)] 'RLK-word-search-backward-again)
;(global-set-key [(shift kp-7)] 'RLK-word-search-backward)
;(global-set-key [(meta kp-7)] 'nil)
;(global-set-key [(control kp-7)] '\\-region)
;(global-set-key [(meta shift kp-7)] 'nil)
;(global-set-key [(control shift kp-7)] 'nil)

(global-set-key [(kp-8)] 'auto-fill-mode)
(global-set-key [(shift kp-8)] 'nil)
;(global-set-key [(meta kp-8)] 'nil)
;(global-set-key [(control kp-8)] 'un\\-region)
;(global-set-key [(meta shift kp-8)] 'nil)
;(global-set-key [(control shift kp-8)] 'nil)

;(global-set-key [(kp-9)] 'RLK-word-search-forward-again)
;(global-set-key [(shift kp-9)] 'RLK-word-search-forward)
;(global-set-key [(meta kp-9)] 'nil)
(global-set-key [(control kp-9)] 'nil)
;(global-set-key [(meta shift kp-9)] 'nil)
;(global-set-key [(control shift kp-9)] 'nil)

(global-set-key [(kp-decimal)] 'execute-extended-command)
(global-set-key [(shift kp-decimal)] 'eval-current-buffer)
(global-set-key [(super kp-decimal)] 'eval-current-buffer)
(global-set-key [(super delete)] 'eval-current-buffer)
(global-set-key [(meta kp-decimal)] 'find-tag-other-window)
(global-set-key [(control kp-decimal)] 'find-tag-other-frame)
;(global-set-key [(meta shift kp-decimal)] 'nil)
;(global-set-key [(control shift kp-decimal)] 'nil)

(global-set-key [(kp-divide)] 'overwrite-mode)
;(global-set-key [(shift kp-divide)] 'edit-picture)
;(global-set-key [(meta kp-divide)] 'nil)
(global-set-key [(control kp-divide)] 'nil)
;(global-set-key [(meta shift kp-divide)] 'nil)
;(global-set-key [(control shift kp-divide)] 'nil)

(global-set-key [(kp-multiply)] 'buffer-menu)
;(global-set-key [(shift kp-multiply)] 'switch-frame)
;(global-set-key [(meta kp-multiply)] 'nil)
(global-set-key [(control kp-multiply)] 'nil)
;(global-set-key [(meta shift kp-multiply)] 'nil)
;(global-set-key [(control shift kp-multiply)] 'nil)

(global-set-key [(kp-subtract)] 'save-buffer)
;(global-set-key [(shift kp-subtract)] 'find-buffer-other-frame)
;(global-set-key [(meta kp-subtract)] 'pop-tags-history-stack)
(global-set-key [(control kp-subtract)] 'exit-recursive-edit)
;(global-set-key [(meta shift kp-subtract)] 'nil)
;(global-set-key [(control shift kp-subtract)] 'nil)

(global-set-key [(kp-add)] 'find-file)
(global-set-key [(shift kp-add)] 'find-file-other-frame)
(global-set-key [(meta kp-add)] 'bind-last-kbd-macro)
;(global-set-key [(control kp-add)] ' tags-toggle)
;(global-set-key [(meta shift kp-add)] 'nil)
;(global-set-key [(control shift kp-add)] 'nil)

; On PC, keypad enter does not have a separate keysym!
(global-set-key [(kp-enter)] 'server-edit)
;(global-set-key [(shift kp-enter)] 'nil)
;(global-set-key [(meta kp-enter)] 'nil)
;(global-set-key [(control kp-enter)] 'nil)
;(global-set-key [(meta shift kp-enter)] 'nil)
;(global-set-key [(control shift kp-enter)] 'nil)

;(global-set-key [(backspace)] 'backward-delete-char-untabify) ;We left as \177, see above.
;(global-set-key [(shift backspace)] 'nil)      		      ;We left as \377, see above.
;(global-set-key [(meta backspace)] 'backward-kill-word)
;(global-set-key [(control backspace)] 'undo)
;(global-set-key [(meta shift backspace)] 'nil)
;(global-set-key [(control shift backspace)] 'nil)

; F16
(global-set-key [(insert)] 'istr)
;(global-set-key [(shift f16)] 'RLK-toggle-display-wrap)
(global-set-key [(meta f16)] 'toggle-read-only)
;(global-set-key [(control f16)] 'bibinsert)
;(global-set-key [(meta shift insert)] 'nil)
;(global-set-key [(control shift insert)] 'nil)

;(global-set-key [(home)] 'beginning-of-line)
(global-set-key [(shift home)] 'nil)
;(global-set-key [(meta home)] 'nil)
(global-set-key [(control home)] 'beginning-of-buffer)
;(global-set-key [(meta shift home)] 'nil)
;(global-set-key [(control shift home)] 'nil)

;(global-set-key [(end)] 'end-of-line)
(global-set-key [(shift end)] 'nil)
;(global-set-key [(meta end)] 'nil)
(global-set-key [(control end)] 'end-of-buffer)
;(global-set-key [(meta shift end)] 'nil)
;(global-set-key [(control shift end)] 'nil)

;(global-set-key [(prior)] 'page-down)
;(global-set-key [(shift prior)] 'to-next-frame)
;(global-set-key [(meta prior)] 'scroll-other-window-back)
(global-set-key [(control prior)] (definteractive (raise-frame)))
(global-set-key [(meta shift prior)] 'beginning-of-buffer-in-other-window)
;(global-set-key [(control shift prior)] 'nil)

;(global-set-key [(next)] 'page-up)
;(global-set-key [(shift next)] 'to-prev-frame)
(global-set-key [(meta next)] 'scroll-other-window)
(global-set-key [(control next)] (definteractive (lower-frame)))
(global-set-key [(meta shift next)] 'end-of-buffer-in-other-window)
;(global-set-key [(control shift next)] 'nil)

;(global-set-key [(up)] 'previous-line)
(global-set-key [(shift up)] 'scroll-down-one)
;(global-set-key [(meta up)] 'nil)
(global-set-key [(control up)] 'nil)
;(global-set-key [(meta shift up)] 'nil)
;(global-set-key [(control shift up)] 'nil)

(global-set-key [(left)] 'backward-char)
(global-set-key [(shift left)] 'backward-word)
(global-set-key [(meta left)] 'backward-template)
;(global-set-key [(control left)] 'lbackward-word)
(global-set-key [(control left)] 'backward-sexp)
;(global-set-key [(meta shift left)] 'nil)
;(global-set-key [(control shift left)] 'nil)

(global-set-key [(right)] 'forward-char)
(global-set-key [(shift right)] 'forward-word)
(global-set-key [(meta right)] 'forward-template)
;(global-set-key [(control right)] 'lforward-word)
(global-set-key [(control right)] 'forward-sexp)
(global-set-key [(meta shift right)] 'nil)
(global-set-key [(control shift right)] 'nil)

;(global-set-key [(down)] 'next-line)
(global-set-key [(shift down)] 'scroll-up-one)
(global-set-key [(meta down)] 'other-window)
; When running vnc, the names of the insert, home, 
; prior, next, delete, end, up, down, left, right
; keys seem all to have kp- in front of them,
; making them indistiguishable from the keypad
; keys when numeric lock is off.
(global-set-key [(meta kp-down)] 'other-window)
(global-set-key [(control down)] 'other-window)
;(global-set-key [(meta shift down)] 'nil)
;(global-set-key [(control shift down)] 'nil)

; Now reserve f1 for help-related.  Use f13 instead.
; But not on my pc keyboard--I don't have an f13.
(global-set-key [(f1)] 'call-last-kbd-macro)
(global-set-key [(shift f1)] 'global-set-key)
(global-set-key [(meta f1)] 'start-kbd-macro)
(global-set-key [(control f1)] 'end-kbd-macro)

(global-set-key [(f2)] 'kill-buffer)
(global-set-key [(shift f2)] 'nil)
(global-set-key [(meta f2)] 'revert-buffer)
(global-set-key [(control f2)] 'delete-frame)
;(global-set-key [(meta shift f2)] 'nil)
;(global-set-key [(control shift f2)] 'nil)

(global-set-key [(f3)] 'query-interchange-region)
(global-set-key [(shift f3)] 'interchange-string-region)
(global-set-key [(meta f3)] 'query-interchange)
(global-set-key [(control f3)] 'interchange-string)
;(global-set-key [(meta shift f3)] 'nil)
;(global-set-key [(control shift f3)] 'nil)

(global-set-key [(f4)] 'query-replace)
(global-set-key [(shift f4)] 'query-replace-regexp)
(global-set-key [(meta f4)] 'query-replace-region)
(global-set-key [(control f4)] 'query-replace-regexp-region)
;(global-set-key [(meta shift f4)] 'nil)
;(global-set-key [(control shift f4)] 'nil)

(global-set-key [(f5)] 'replace-string)
(global-set-key [(shift f5)] 'replace-regexp)
(global-set-key [(meta f5)] 'replace-string-region)
(global-set-key [(control f5)] 'replace-regexp-region)
;(global-set-key [(meta shift f5)] 'nil)
;(global-set-key [(control shift f5)] 'nil)

(global-set-key [(f6)] 'myshell-f6-powershell)
(global-set-key [(shift f6)] 'myshell-f6-shift-powershell)
(global-set-key [(meta f6)] 'myshell-f6-meta-powershell)
(global-set-key [(control f6)] 'myshell-f6-ctrl-powershell)
;(global-set-key [(shift f6)] 'tilda-region)
;(global-set-key [(meta f6)] 'tilda-programexample)
;(global-set-key [(control f6)] 'mh-smail)
;(global-set-key [(meta shift f6)] 'nil)
;(global-set-key [(control shift f6)] 'nil)

(global-set-key [(f7)] 'describe-key-briefly)
(global-set-key [(shift f7)] 'describe-key)
;(global-set-key [(meta f7)] 'describe-mouse)
;(global-set-key [(control f7)] 'describe-mouse-briefly)
;(global-set-key [(meta shift f7)] 'nil)
;(global-set-key [(control shift f7)] 'nil)

(global-set-key [(f8)] 'myshell)
(global-set-key [(shift f8)] 'myshell-shift)
(global-set-key [(meta f8)] 'myshell-meta)
(global-set-key [(control f8)] 'myshell-ctrl)
(global-set-key [(super f8)]
		(definteractive (switch-to-buffer "*scratch*")))
;(global-set-key [(meta shift f8)] 'nil)
;(global-set-key [(control shift f8)] 'nil)

(global-set-key [(f9)] 'myshell-powershell)
(global-set-key [(shift f9)] 'myshell-shift-powershell)
(global-set-key [(meta f9)] 'myshell-meta-powershell)
(global-set-key [(control f9)] 'myshell-ctrl-powershell)
;(global-set-key [(f9)] 'insert-Maribila)
;(global-set-key [(shift f9)] 'insert-andf-tech)
;(global-set-key [(meta f9)] 'insert-andf_)
;(global-set-key [(control f9)] 'insert-andf-tech-request)
;(global-set-key [(super f9)] 'insert-ANDF)
;(global-set-key [(meta shift f9)] 'nil)
;(global-set-key [(control shift f9)] 'nil)

(global-set-key [(f10)] 'my-dired-up)
(global-set-key [(shift f10)] 'grep)
;(global-set-key [(meta f10)] 'gnus-Subject-catch-up)
(global-set-key [(control f10)] 'nil)
;(global-set-key [(meta shift f10)] 'nil)
;(global-set-key [(control shift f10)] 'nil)

(global-set-key [(f11)] 'insert-key-function)
(global-set-key [(shift f11)] 'byte-compile-file)
(global-set-key [(meta f11)] 'byte-recompile-directory)
(global-set-key [(control f11)] 'nil)
;(global-set-key [(meta shift f11)] 'nil)
;(global-set-key [(control shift f11)] 'nil)

(global-set-key [(f12)] 'tags-search)
(global-set-key [(shift f12)] 'next-file)
(global-set-key [(meta f12)] 'tags-query-replace)
(global-set-key [(control f12)] 'tags-replace)
;(global-set-key [(meta shift f12)] 'nil)
;(global-set-key [(control shift f12)] 'nil)

(global-set-key [(f13)] 'call-last-kbd-macro)
(global-set-key [(shift f13)] 'global-set-key)
(global-set-key [(meta f13)] 'start-kbd-macro)
(global-set-key [(control f13)] 'end-kbd-macro)
;(global-set-key [(meta shift f13)] 'nil)
;(global-set-key [(control shift f13)] 'nil)

(defun my-compile ()
  (interactive)
  ;;(compile "make")
  (compile "xmfe all.m")
  )


;(global-set-key [(f19)] 'my-compile)
;(global-set-key [(f20)] 'eval-expression)

;;; ********************
;;; Edebug is a source-level debugger for emacs-lisp programs.
;;;
(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)


;;;; Now mouse bindings.


;(global-set-key [(button1)] 'mouse-track)
;(global-set-key [(button2)] 'mouse-yank)
;(global-set-key [(button3)] 'popup-mode-menu)

;(global-set-key [(shift button1)] 'mouse-track-adjust)
(global-set-key [(shift button2)] 'mouse-track-copy)
;(global-set-key [(shift button3)] 'mouse-function-menu

;(global-set-key [(control button1)] 'mouse-track-adjust)
(global-set-key [(control button2)] 'x-set-point-and-move-selection)
(global-set-key [(control button3)] 'mouse-select-thing)
(global-set-key [(control button3up)] 'mouse-thing-grab)
;(global-set-key [(control button3)] 'mouse-function-menu

;(global-set-key [(control shift button1)] 'mouse-track-delete-and-insert)
(global-set-key [(control shift button2)] 'mouse-track-delete)
;(global-set-key [(control shift button3)] 'nil)

;(global-set-key [(meta button1)] 'mouse-track-do-rectangle)
;(global-set-key [(meta button2)] 'mouse-isearch-thing-word)
(global-set-key [(meta button2)] (mousefun-set-point (my-dired-up)))
(global-set-key [(meta button2up)] 'nil)
;(global-set-key [(meta button3)] 'nil)

(global-set-key [(meta shift button1)] 'mouse-isearch-thing)
(global-set-key [(meta shift button2)] 'mouse-scroll)
(global-set-key [(meta shift button3)] 'mouse-vscroll)

(global-set-key [(meta control button1)]
		(mousefun-set-point
		 (find-tag (find-tag-default))))
(global-set-key [(meta control button2)]
		(mousefun-set-point
		 (find-tag-other-frame (find-tag-default))))
;(global-set-key [(meta control button3)] 'popup-buffer-menu)

(global-set-key [(meta control shift button1)]
		(mousefun-set-point
		 (undo-boundary) (setq last-command nil)
		 (execute-kbd-macro [(meta kp-1)])))
(global-set-key [(meta control shift button2)]
		(mousefun-set-point
		 (undo-boundary) (setq last-command nil)
		 (execute-kbd-macro [(meta kp-2)])))
(global-set-key [(meta control shift button3)]
		(mousefun-set-point
		 (undo-boundary) (setq last-command nil)
		 (execute-kbd-macro [(meta kp-3)])))

(global-set-key [(super button1)] (mousefun-set-point (scroll-up)))
(global-set-key [(super button2)] (mousefun-set-point (my-dired-up)))
(global-set-key [(super button3)] (mousefun-set-point (scroll-down)))

(global-set-key [(super shift button1)]
		(mousefun-set-point
		 (undo-boundary) (setq last-command nil)
		 (execute-kbd-macro [(super kp-1)])))
(global-set-key [(super shift button2)]
		(mousefun-set-point
		 (undo-boundary) (setq last-command nil)
		 (execute-kbd-macro [(super kp-2)])))
(global-set-key [(super shift button3)]
		(mousefun-set-point
		 (undo-boundary) (setq last-command nil)
		 (execute-kbd-macro [(super kp-3)])))

(global-set-key [(super control button1)]
		(mousefun-set-point
		 (undo-boundary)
		 (yank)
		 (setq last-command 'yank)))
(global-set-key [(super control button2)] 'mouse-select-thing)
(global-set-key [(super control button2up)] 'nil)
(global-set-key [(super control button3)] 'mouse-select-thing)
(global-set-key [(super control button3up)] 'mouse-thing-copy)

(global-set-key [(super meta button1)] 'mouse-select-thing)
(global-set-key [(super meta button1up)] 'mouse-thing-grab)
(global-set-key [(super meta button2)] 'mouse-select-thing)
(global-set-key [(super meta button2up)] 'mouse-thing-kill)

(global-set-key [(hyper shift button1)]
		(mousefun-set-point
		 (undo-boundary) (setq last-command nil)
		 (execute-kbd-macro [(hyper kp-1)])))
(global-set-key [(hyper shift button2)]
		(mousefun-set-point
		 (undo-boundary) (setq last-command nil)
		 (execute-kbd-macro [(hyper kp-2)])))
(global-set-key [(hyper shift button3)]
		(mousefun-set-point
		 (undo-boundary) (setq last-command nil)
		 (execute-kbd-macro [(hyper kp-3)])))

