(require 'thing)
(defmacro mousefun-set-point (&rest body)
  (`
   (function
    (lambda (event)
      (interactive "@e")
      (mouse-set-point event)
      (,@ body )))))

(defmacro mousefun-save-set-point (&rest body)
  (`
   (function
    (lambda (event)
      (interactive "@e")
      (save-excursion
	(save-window-excursion
	  (mouse-set-point event)
	  (,@ body )))))))

(defun mouse-isearch-thing (event)
  "Start isearch with thing as default.  Type ^S to actually do the search."
  (interactive "@e")
  (mouse-set-point event)
  (let* ((bds (thing-boundaries (point)))
	 (str (buffer-substring (car bds) (cdr bds)))
	 (n (length str))
	 (i 0)
	 ch)
    (goto-char (car bds))
    (execute-kbd-macro [(control ?s)])
    (execute-kbd-macro (downcase str))
    ))

(defun mouse-isearch-thing-word (event)
  "Start isearch with thing as default.  Type ^S to actually do the search."
  (interactive "@e")
  (mouse-set-point event)
  (let* ((bds (thing-boundaries (point)))
	 (str (concat "[^_a-zA-Z0-9]" (buffer-substring (car bds) (cdr bds)) "[^_a-zA-Z0-9]"))
	 (n (length str))
	 (i 0)
	 ch)
    (goto-char (1- (car bds)))
    (execute-kbd-macro [(meta control ?s)])
    (execute-kbd-macro (downcase str))
    ))

;;;mouse-grab-drag not currently used.
(defun mouse-grab-drag (event &optional delete noinsert nosave setpoint)
  "Copy drag region to point before drag was started.  Should be used as an
up button with start-mouse-grab-drag on the down button.  First ARG is
mouse event.  With optional DELETE, drag region is deleted.  With NOINSERT
region is not inserted at point.  With NOSAVE, region is not saved in kill
ring.  With SETPOINT, point is left at the end of the drag region rather
than its location before the grab."
  (interactive "*e")
  (setq mouse-track-insert-selected-region nil)
  (let ((mouse-track-drag-up-hook 'mouse-track-insert-drag-up-hook)
	s end win pair)
    (save-excursion
      (save-window-excursion
	(mouse-track event)
	(setq win (selected-window))
	(if (consp mouse-track-insert-selected-region)
	    (progn
	      (setq pair mouse-track-insert-selected-region)
	      (setq s (buffer-substring (car pair) (cdr pair)))
	      (if (not nosave)
		  (progn (setq last-command nil)
			 (copy-region-as-kill (car pair) (cdr pair))
			 (setq this-command nil)))
	      (if delete
		  (progn (undo-boundary)
			 (delete-region (car pair) (cdr pair))))
	      ))))
    (if setpoint
	(progn
	  (select-window win)
	  (goto-char (cdr pair))))
    (or noinsert (null s) (equal s "") (insert s))))

(defvar mouse-track-copy-selected-region nil)

(defun mouse-track-copy-drag-up-hook (event click-count)
  (let ((result (default-mouse-track-return-dragged-selection event)))
    (setq mouse-track-copy-selected-region result)
    ;(default-mouse-track-maybe-own-selection result 'PRIMARY)
    )
  t)

(defun mouse-track-copy (event)
  "Like mouse track, but also copy dragged region as kill."
  (interactive "_e")
  (setq mouse-track-copy-selected-region nil)
  (let ((mouse-track-drag-up-hook 'mouse-track-copy-drag-up-hook))
    (save-excursion
      (save-window-excursion
	(mouse-track event)
	(if (consp mouse-track-copy-selected-region)
	    (let ((pair mouse-track-copy-selected-region))
	      (copy-region-as-kill (car pair) (cdr pair))))))))

(defun mouse-track-delete (event)
  "Make a selection with the mouse and delete it.
This is exactly the same as the `mouse-track' command on \\[mouse-track],
except that point is not moved; the selected text is killed
after being selected\; and the selection is immediately disowned afterwards."
  (interactive "*e")
  (setq mouse-track-insert-selected-region nil)
  (let ((mouse-track-drag-up-hook 'mouse-track-insert-drag-up-hook)
	s)
    (save-excursion
      (save-window-excursion
	(mouse-track event)
	(if (consp mouse-track-insert-selected-region)
	    (let ((pair mouse-track-insert-selected-region))
	      (setq s (prog1
			  (buffer-substring (car pair) (cdr pair))
			(kill-region (car pair) (cdr pair))))))))))


(defun mouse-vscroll (event)
  "Vertical scroll mouse spot to top of window if it mouse in the lower half
of the window or to the bottom of the window if mouse is in the upper half of
the window.  With a drag, scroll the line at the down press to the mouse
location at the up press.  This should be bound to an up button."
  (interactive "e")
  (setq mouse-track-insert-selected-region nil)
  (let ((mouse-track-drag-up-hook 'mouse-track-insert-drag-up-hook)
	s i n lines pair begin ending count)
    (mouse-track event)
    (cond ((consp mouse-track-insert-selected-region)
	   (setq pair mouse-track-insert-selected-region)
	   (setq begin (car pair))
	   (setq ending (cdr pair))
	   (setq s (buffer-substring begin ending))
	   ;; Count lines in s
	   (setq lines (- (count-lines begin ending) 1))
	   (if (= ending (point))
	       (if (< begin ending)
		   (setq count lines)
		 (setq count (- lines)))
	     (if (< begin ending)
		 (setq count (- lines))
	       (setq count lines)))
	   (scroll-down count))
	  ((< (count-lines (window-start) (point))
	      (/ (window-height) 2))
	   (recenter 0))
	  (t (recenter -1)))))

(defvar mouse-thing-window nil)
(defvar mouse-thing-start nil)
(defvar mouse-thing-end nil)
(defvar mouse-thing-save-window nil)
(defvar mouse-thing-save-marker nil)

(defun mouse-select-thing (event)
  "Highlight thing at mouse EVENT."
  (interactive "e")
  ;(message "window=%s" (selected-window))
  (setq mouse-thing-save-window (selected-window))
  (setq mouse-thing-save-marker (copy-marker (point)))
  (mouse-set-point event)
  (let* ((place (thing-boundaries (event-point event)))
	 (start (car place))
	 (end (cdr place))
	 (type 'PRIMARY))
    
    (setq mouse-thing-start (copy-marker start))
    (setq mouse-thing-end (copy-marker end))
    (goto-char end)
    (push-mark (copy-marker start))
    ;(copy-region-as-kill start end)
    (kill-new (buffer-substring start end))
    (cond (zmacs-regions
	   (zmacs-activate-region))
	  ((eq 'x (device-type (selected-device)))
	   (if (= start end)
	       (x-disown-selection type)
	     (x-own-selection (cons (set-marker (make-marker) start)
				    (set-marker (make-marker) end))
			      type))))
    ))

(defun mouse-thing-copy (event)
  "For use as up button with mouse-select-thing. Net effect
is to copy the thing into the kill ring so it can be yanked.
This function itself just restores the prior window and position."
  (interactive "e")
  (and mouse-thing-save-window
       (select-window mouse-thing-save-window)
       (goto-char mouse-thing-save-marker)))

(defun mouse-thing-grab (event)
  "For use as up button with mouse-select-thing. Net effect
is to copy the thing into the kill ring so it can be yanked.
This function itself just restores the prior window and position."
  (interactive "e")
  (and mouse-thing-save-window
       (select-window mouse-thing-save-window)
       (goto-char mouse-thing-save-marker)
       (yank)))

(defun mouse-thing-kill (event)
  "For use as up button with mouse-select-thing. Net effect
is to copy the thing into the kill ring so it can be yanked.
This function itself just restores the prior window and position."
  (interactive "e")
  (or
   (and mouse-thing-start
       (markerp mouse-thing-start)
       (marker-buffer mouse-thing-start)
       mouse-thing-end
       (markerp mouse-thing-end)
       (marker-buffer mouse-thing-end)
       (eq (marker-buffer mouse-thing-start)
	   (marker-buffer mouse-thing-end))
       )
   (error "Invalid markers"))
  (delete-region mouse-thing-start mouse-thing-end)
  ; Now back to starting point.
  (and mouse-thing-save-window
       (select-window mouse-thing-save-window)
       (goto-char mouse-thing-save-marker)))
