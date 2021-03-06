;;; man.el --- browse UNIX manual pages
;; Keywords: help

;; Copyright (C) 1985, 1993, 1994, 1996, 1997 Free Software Foundation, Inc.
;;
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
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; This file defines "manual-entry", and the remaining definitions all
;; begin with "Manual-".  This makes the autocompletion on "M-x man" work.
;; 
;; Eviscerated 26-Jun-96 by Jamie Zawinski <jwz@netscape.com>.
;; All that stuff about looking at $MANPATH and building up lists of 
;; directories was bullshit.  Now we just invoke "man" and format the
;; output, end of story.
;;
;; [ older changelog entries removed, since they're all about code that
;;   I've deleted. ]

(defgroup man nil
  "Browse Unix manual pages"
  :group 'help)

(defcustom Manual-program "man" "\
*Name of the program to invoke in order to format the source man pages."
  :type 'string
  :group 'man)

(defcustom Manual-buffer-view-mode t "\
*Whether manual buffers should be placed in view-mode.
nil means leave the buffer in fundamental-mode in another window.
t means use `view-buffer' to display the man page in the current window.
Any other value means use `view-buffer-other-window'."
  :type '(radio (const :tag "Fundamental-mode other window" nil)
		(const :tag "View-mode current window" t)
		(sexp :format "%t\n" :tag "View-mode other window" other))
  :group 'man)

(defcustom Manual-mode-hook nil
  "Function or functions run on entry to Manual-mode."
  :type 'hook
  :group 'man)

(defvar Manual-page-history nil "\
A list of names of previously visited man page buffers.")

(defvar Manual-page-minibuffer-history nil "\
Minibuffer completion history for `manual-entry'.")

;; New variables.

(defcustom Manual-snip-subchapter
  (not (string-match "solaris" system-configuration))
  "*Should man look in chapter 3 for ctime(3c)?
This is relavent for Solaris and, perhaps, other systems which have 
`man -s 3' not find things in chapter 3c, or other such sub-chapters"
  :type 'boolean
  :group 'man)

;;Here is information on RosettaMan, from Neal.Becker@comsat.com (Neal Becker):

;;RosettaMan is a filter for UNIX manual pages.  It takes as input man
;;pages formatted for a variety of UNIX flavors (not [tn]roff source)
;;and produces as output a variety of file formats.  Currently
;;RosettaMan accepts man pages as formatted by the following flavors of
;;UNIX: Hewlett-Packard HP-UX, AT&T System V, SunOS, Sun Solaris, OSF/1,
;;DEC Ultrix, SGI IRIX, Linux, SCO; and produces output for the following
;;formats: printable ASCII only (stripping page headers and footers),
;;section and subsection headers only, TkMan, [tn]roff, Ensemble, RTF,
;;SGML (soon--I finally found a DTD), HTML, MIME, LaTeX, LaTeX 2e, Perl 5's pod.

;;RosettaMan improves on other man page filters in several ways: (1) its
;;analysis recognizes the structural pieces of man pages, enabling high
;;quality output, (2) its modular structure permits easy augmentation of
;;output formats, (3) it accepts man pages formatted with the varient
;;macros of many different flavors of UNIX, and (4) it doesn't require
;;modification or cooperation with any other program.

;;RosettaMan is a rewrite of TkMan's man page filter, called bs2tk.  (If
;;you haven't heard about TkMan, a hypertext man page browser, you
;;should grab it via anonymous ftp from ftp.cs.berkeley.edu:
;;/ucb/people/phelps/tkman.tar.Z.)  Whereas bs2tk generated output only for
;;TkMan, RosettaMan generalizes the process so that the analysis can be
;;leveraged to new output formats.  A single analysis engine recognizes
;;section heads, subsection heads, body text, lists, references to other
;;man pages, boldface, italics, bold italics, special characters (like
;;bullets), tables (to a degree) and strips out page headers and
;;footers.  The engine sends signals to the selected output functions so
;;that an enhancement in the engine improves the quality of output of
;;all of them.  Output format functions are easy to add, and thus far
;;average about about 75 lines of C code each.



;;*** NOTES ON CURRENT VERSION ***

;;Help!  I'm looking for people to help with the following projects.
;;\(1) Better RTF output format.  The current one works, but could be
;;made better.  (2) Roff macros that produce text that is easily
;;parsable.  RosettaMan handles a great variety, but some things, like
;;H-P's tables, are intractable.  If you write an output format or
;;otherwise improve RosettaMan, please send in your code so that I may
;;share the wealth in future releases.

;;This version can try to identify tables (turn this on with the -T
;;switch) by looking for lines with a large amount of interword spacing,
;;reasoning that this is space between columns of a table.  This
;;heuristic doesn't always work and sometimes misidentifies ordinary
;;text as tables.  In general I think it is impossible to perfectly
;;identify tables from nroff formatted text.  However, I do think the
;;heuristics can be tuned, so if you have a collection of manual pages
;;with unrecognized tables, send me the lot, in formatted form (i.e.,
;;after formatting with nroff -man), and uuencode them to preserve the
;;control characters.  Better, if you can think of heuristics that
;;distinguish tables from ordinary text, I'd like to hear them.

;;Notes for HTML consumers: This filter does real (heuristic)
;;parsing--no <PRE>!  Man page references are turned into hypertext links.

(defcustom Manual-use-rosetta-man (not (null (locate-file "rman" exec-path))) "\
If non-nil, use RosettaMan (rman) to filter man pages.
This makes man-page cleanup virtually instantaneous, instead of
potentially taking a long time."
  :type 'boolean
  :group 'man)

(defface man-italic '((t (:italic t)))
  "Manual italics face"
  :group 'man)

(defface man-bold '((t (:bold t)))
  "Manual bold face"
  :group 'man)

(defface man-heading '((t (:bold t)))
  "Manual headings face"
  :group 'man)

(defface man-xref '((t (:underline t)))
  "Manual xrefs face"
  :group 'man)


(defvar Manual-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'Manual-mode-map)
    (define-key m "l" 'Manual-last-page)
    (define-key m 'button2 'Manual-follow-xref)
    (define-key m 'button3 'Manual-popup-menu)
    m))

;;;###autoload
(defun manual-entry (topic &optional arg silent)
  "Display the Unix manual entry (or entries) for TOPIC."
  (interactive
   (list (let* ((fmh "-A-Za-z0-9_.:")
		(default (save-excursion
			   (buffer-substring
			    (progn
			      (re-search-backward "\\sw" nil t)
			      (skip-chars-backward fmh) (point))
			    (progn (skip-chars-forward fmh) (point)))))
		(thing (read-string
			(if (equal default "") "Manual entry: "
			  (concat "Manual entry: (default " default ") "))
			nil 'Manual-page-minibuffer-history)))
	   (if (equal thing "") default thing))
	 (prefix-numeric-value current-prefix-arg)))
  (or arg (setq arg 1))
  (let (section apropos-mode)
    (let ((case-fold-search nil))
      (if (and (null section)
	       (string-match "\\`[ \t]*\\([^( \t]+\\)[ \t]*(\\(.+\\))[ \t]*\\'"
			     topic))
	  (setq section (match-string 2 topic)
		topic (match-string 1 topic))
	(if (string-match "\\`[ \t]*-k[ \t]+\\([^ \t]+\\)\\'" topic)
	    (setq section "-k"
		  topic (substring topic (match-beginning 1))))))

    (when Manual-snip-subchapter
      ;; jwz: turn section "3x11" and "3n" into "3".
      (if (and section (string-match "\\`\\([0-9]+\\)[^0-9]" section))
	  (setq section (match-string 1 section))))

    (if (equal section "-k")
	(setq apropos-mode t))

    (let ((bufname (concat "Man"
			   (when apropos-mode " apropos")
			   ": " topic
			   (when section (concat "(" section ")"))))
	  (temp-buffer-show-function 
	   (cond ((eq 't Manual-buffer-view-mode)
		  'view-buffer)
		 ((eq 'nil Manual-buffer-view-mode)
		  temp-buffer-show-function)
		 (t
		  'view-buffer-other-window))))

      (cond ((get-buffer bufname)
	     ;; reselect an old man page buffer if it exists already.
	     (save-excursion
	       (set-buffer (get-buffer bufname))
	       (Manual-mode))
	     (if temp-buffer-show-function
		 (funcall temp-buffer-show-function (get-buffer bufname))
	       (display-buffer bufname)))
	    (t
	     (with-output-to-temp-buffer bufname
	       (buffer-disable-undo standard-output)
	       (save-excursion
		 (set-buffer standard-output)
		 (setq buffer-read-only nil)
		 (erase-buffer)

		 (let ((args (list topic))
		       args-string)
		   (if section
		       (setq args
			     (if (and (eq system-type 'usg-unix-v)
				      (null apropos-mode))
				 (cons "-s" (cons section args))
			       (cons section args))))
		   (setq args-string
			 (mapconcat 'identity (cons Manual-program args) " "))
		   (if (string-match "\\`\\([^ \t/]*/\\)+" args-string)
		       (setq args-string
			     (substring args-string (match-end 0))))

		   ;(prompt-for-insert "b1")
		   (message "%s (running...)" args-string)
		   (apply 'call-process Manual-program nil '(t nil) nil args)
		   ;(prompt-for-insert "b2")
		   (if (< (buffer-size) 200)
		       (progn
			 (kill-buffer (current-buffer))
			 (error "%s not found" args-string)))

		   (message "%s (cleaning...)" args-string)
		   (Manual-nuke-nroff-bs apropos-mode)
		   (message "%s (done.)" args-string))
		 (set-buffer-modified-p nil)
		 (Manual-mode)))))
      (let ((page (if section
		      (concat topic "(" section ")")
		    topic)))
	(setq Manual-page-history
	      (cons (buffer-name)
		    (delete (buffer-name) Manual-page-history))
	      Manual-page-minibuffer-history
	      (cons page (delete page Manual-page-minibuffer-history))))))

  (message nil)
  t)

(defun Manual-mode ()
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (use-local-map Manual-mode-map)
  (setq major-mode 'Manual-mode
	mode-name "Manual")
  ;; man pages with long lines are buggy!
  ;; This looks slightly better if they only
  ;; overran by a couple of chars.
  (setq truncate-lines t)
  ;; turn off horizontal scrollbars in this buffer
  (when (featurep 'scrollbar)
    (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook #'(lambda ()
				  (setq Manual-page-history
					(delete (buffer-name)
						Manual-page-history)))
	    nil t)
  (run-hooks 'Manual-mode-hook))

(defun Manual-last-page ()
  (interactive)
  (if Manual-page-history
      (let ((page (pop Manual-page-history)))
	(if page
	    (progn
	      (get-buffer page)
	      (cons Manual-page-history page)
	      (switch-to-buffer page))))
    (error "No manual page buffers found. Use `M-x manual-entry'")))


(defmacro Manual-delete-char (n)
  ;; in v19, delete-char is compiled as a function call, but delete-region
  ;; is byte-coded, so it's much faster.  (We were spending 40% of our time
  ;; in delete-char alone.)
  (list 'delete-region '(point) (list '+ '(point) n)))

;; Hint: BS stands for more things than "back space"
(defun Manual-nuke-nroff-bs (&optional apropos-mode)
  (interactive "*")
  (if Manual-use-rosetta-man
      (call-process-region (point-min) (point-max) "rman" t t nil)
    ;;
    ;; turn underlining into italics
    ;;
    (goto-char (point-min))
    (while (search-forward "_\b" nil t)
      ;; searching for underscore-backspace and then comparing the
      ;; following chars until the sequence ends turns out to be much
      ;; faster than searching for a regexp which matches the whole
      ;; sequence.
      (let ((s (match-beginning 0)))
	(goto-char s)
	(while (and (= (following-char) ?_)
		    (= (char-after (1+ (point))) ?\b))
	  (Manual-delete-char 2)
	  (forward-char 1))
	(set-extent-face (make-extent s (point)) 'man-italic)))
    ;;
    ;; turn overstriking into bold
    ;;
    (goto-char (point-min))
    (while (re-search-forward "\\([^\n]\\)\\(\b\\1\\)" nil t)
      ;; Surprisingly, searching for the above regexp is faster than searching
      ;; for a backspace and then comparing the preceding and following chars,
      ;; I presume because there are many false matches, meaning more funcalls
      ;; to re-search-forward.
      (let ((s (match-beginning 0)))
	(goto-char s)
	;; Some systems (SGI) overstrike multiple times, eg, "M\bM\bM\bM".
	(while (looking-at "\\([^\n]\\)\\(\b\\1\\)+")
	  (delete-region (+ (point) 1) (match-end 0))
	  (forward-char 1))
	(set-extent-face (make-extent s (point)) 'man-bold)))
    ;;
    ;; hack bullets: o^H+ --> +
    (goto-char (point-min))
    (while (search-forward "\b" nil t)
      (Manual-delete-char -2))

    (if (> (buffer-size) 100) ; minor kludge
	(Manual-nuke-nroff-bs-footers))
    ) ;; not Manual-use-rosetta-man
  ;;
  ;; turn subsection header lines into bold
  ;;
  (goto-char (point-min))
  (if apropos-mode
      (while (re-search-forward "[a-zA-Z0-9] ([0-9]" nil t)
	(forward-char -2)
	(delete-backward-char 1))

    ;;    (while (re-search-forward "^[^ \t\n]" nil t)
    ;;      (set-extent-face (make-extent (match-beginning 0)
    ;;                                   (progn (end-of-line) (point)))
    ;;                      'man-heading))

    ;; boldface the first line
    (if (looking-at "[^ \t\n].*$")
	(set-extent-face (make-extent (match-beginning 0) (match-end 0))
			 'man-bold))

    ;; boldface subsequent title lines
    ;; Regexp to match section headers changed to match a non-indented
    ;; line preceded by a blank line and followed by an indented line. 
    ;; This seems to work ok for manual pages but gives better results
    ;; with other nroff'd files
    (while (re-search-forward "\n\n\\([^ \t\n].*\\)\n[ \t]+[^ \t\n]" nil t)
      (goto-char (match-end 1))
      (set-extent-face (make-extent (match-beginning 1) (match-end 1))
		       'man-heading)
      (forward-line 1))
    )

  (if Manual-use-rosetta-man
      nil
    ;; Zap ESC7,  ESC8, and ESC9
    ;; This is for Sun man pages like "man 1 csh"
    (goto-char (point-min))
    (while (re-search-forward "\e[789]" nil t)
      (replace-match "")))
  
  ;; Nuke blanks lines at start.
  ;;  (goto-char (point-min))
  ;;  (skip-chars-forward "\n")
  ;;  (delete-region (point-min) (point))

  (Manual-mouseify-xrefs)
  )

(fset 'nuke-nroff-bs 'Manual-nuke-nroff-bs) ; use old name


(defun Manual-nuke-nroff-bs-footers ()
  "For info see comments in packages/man.el"
  ;; Nuke headers and footers.
  ;;
  ;; nroff assumes pages are 66 lines high.  We assume that, and that the
  ;; first and last line on each page is expendible.  There is no way to
  ;; tell the difference between a page break in the middle of a paragraph
  ;; and a page break between paragraphs (the amount of extra whitespace
  ;; that nroff inserts is the same in both cases) so this might strip out
  ;; a blank line were one should remain.  I think that's better than
  ;; leaving in a blank line where there shouldn't be one.  (Need I say
  ;; it: FMH.)
  ;;
  ;; Note that if nroff spits out error messages, pages will be more than
  ;; 66 lines high, and we'll lose badly.  That's ok because standard
  ;; nroff doesn't do any diagnostics, and the "gnroff" wrapper for groff
  ;; turns off error messages for compatibility.  (At least, it's supposed
  ;; to.)
  ;; 
  (goto-char (point-min))
  ;; first lose the status output
  (let ((case-fold-search t))
    (if (and (not (looking-at "[^\n]*warning"))
	     (looking-at "Reformatting.*\n"))
	(delete-region (match-beginning 0) (match-end 0))))

  ;; kludge around a groff bug where it won't keep quiet about some
  ;; warnings even with -Wall or -Ww.
  (cond ((looking-at "grotty:")
	 (while (looking-at "grotty:")
	   (delete-region (point) (progn (forward-line 1) (point))))
	 (if (looking-at " *done\n")
	     (delete-region (point) (match-end 0)))))

  (let ((pages '())
	p)
    ;; collect the page boundary markers before we start deleting, to make
    ;; it easier to strip things out without changing the page sizes.
    (while (not (eobp))
      (forward-line 66)
      (setq pages (cons (point-marker) pages)))
    (setq pages (nreverse pages))
    (while pages
      (goto-char (car pages))
      (set-marker (car pages) nil)
      ;;
      ;; The lines are: 3 blank; footer; 6 blank; header; 3 blank.
      ;; We're in between the previous footer and the following header,
      ;;
      ;; First lose 3 blank lines, the header, and then 3 more.
      ;;
      (setq p (point))
      (skip-chars-forward "\n")
      (delete-region p (point))
      (and (looking-at "[^\n]+\n\n?\n?\n?")
	   (delete-region (match-beginning 0) (match-end 0)))
      ;;
      ;; Next lose the footer, and the 3 blank lines after, and before it.
      ;; But don't lose the last footer of the manual entry; that contains
      ;; the "last change" date, so it's not completely uninteresting.
      ;; (Actually lose all blank lines before it; sh(1) needs this.)
      ;;
      (skip-chars-backward "\n")
      (beginning-of-line)
      (if (null (cdr pages))
	  nil
	(and (looking-at "[^\n]+\n\n?\n?\n?")
	     (delete-region (match-beginning 0) (match-end 0))))
      (setq p (point))
      (skip-chars-backward "\n")
      (if (> (- p (point)) 4)
	  (delete-region (+ 2 (point)) p)
	(delete-region (1+ (point)) p))
;      (and (looking-at "\n\n?\n?")
;	   (delete-region (match-beginning 0) (match-end 0)))

      (setq pages (cdr pages)))
    ;;
    ;; Now nuke the extra blank lines at the beginning and end.
    (goto-char (point-min))
    (if (looking-at "\n+")
	(delete-region (match-beginning 0) (match-end 0)))
    (forward-line 1)
    (if (looking-at "\n\n+")
	(delete-region (1+ (match-beginning 0)) (match-end 0)))
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (delete-region (point) (point-max))
    (beginning-of-line)
    (forward-char -1)
    (setq p (point))
    (skip-chars-backward "\n")
    (if (= ?\n (following-char)) (forward-char 1))
    (if (> (point) (1+ p))
	(delete-region (point) p))
    ))

(defun Manual-mouseify-xrefs ()
  (goto-char (point-min))
  ;; skip the top line of manual pages, but not apropos listings.
  (unless apropos-mode (forward-line 1))
  (let ((case-fold-search nil)
	s e name splitp extent)
    ;; possibly it would be faster to rewrite this expression to search for
    ;; a less common sequence first (like "([0-9]") and then back up to see
    ;; if it's really a match.  This function is 15% of the total time, 13%
    ;; of which is this call to re-search-forward.
    (while (re-search-forward "[a-zA-Z_][-a-zA-Z0-9_.:]*([0-9][a-zA-Z0-9]*)"
			      nil t)
      (setq s (match-beginning 0)
	    e (match-end 0)
	    name (buffer-substring s e)
	    splitp nil)

      (goto-char s)
      ;; if this is a hyphenated xref, we're on the second line, 1st char now.
      (when (progn
	      (beginning-of-line)
	      (and (looking-at (concat "^[ \t]+" (regexp-quote name)))
		   (progn
		     (backward-char 1)
		     (or (equal (char-before) ?-)
			 (equal (char-before) ?\255)))
		   (setq s (progn
			     (skip-chars-backward "-\255_a-zA-Z0-9")
			     (point))
			 name (buffer-substring s e))))
	(setq splitp t)
	;; delete the spaces and dash from `name'
	(let (i)
	  (while (setq i (string-match "[-\255 \n\t]+" name i))
	    (setq name (concat (substring name 0 i)
			       (substring name (match-end 0)))
		  i (1+ i)))))

      ;; if there are upper case letters in the section, downcase them.
      (if (string-match "(.*[A-Z]+.*)$" name)
	  (setq name (concat (substring name 0 (match-beginning 0))
			     (downcase (substring name (match-beginning 0))))))

      ;; if the xref was hyphenated, don't highlight the indention spaces.
      (if splitp
	  (progn
	    (setq extent (make-extent s (progn (goto-char s) (end-of-line) (point))))
	    (set-extent-property extent 'man (list 'Manual-follow-xref name))
	    (set-extent-property extent 'highlight t)
	    (set-extent-face extent 'man-xref)
	    (goto-char e)
	    (skip-chars-backward "-_a-zA-Z0-9()")
	    (setq extent (make-extent (point) e)))
	(setq extent (make-extent s e)))
      (set-extent-property extent 'man (list 'Manual-follow-xref name))
      (set-extent-property extent 'highlight t)
      (set-extent-face extent 'man-xref)
      (goto-char e))))

(defun Manual-follow-xref (&optional name-or-event)
  "Invoke `manual-entry' on the cross-reference under the mouse.
When invoked noninteractively, the arg may be an xref string to parse instead."
  (interactive "e")
  (if (eventp name-or-event)
      (let* ((p (event-point name-or-event))
	     (extent (and p (extent-at p
			     (event-buffer name-or-event)
			     'highlight)))
	     (data (and extent (extent-property extent 'man))))
	(if (eq (car-safe data) 'Manual-follow-xref)
	    (eval data)
	  (error "no manual cross-reference there.")))
    (or (manual-entry name-or-event)
	;; If that didn't work, maybe it's in a different section than the
	;; man page writer expected.  For example, man pages tend assume
	;; that all user programs are in section 1, but X tends to generate
	;; makefiles that put things in section "n" instead...
	(and (string-match "[ \t]*([^)]+)\\'" name-or-event)
	     (progn
	       (message "No entries found for %s; checking other sections..."
			name-or-event)
	       (manual-entry
		(substring name-or-event 0 (match-beginning 0))
		nil t))))))

(defun Manual-popup-menu (&optional event)
  "Pops up a menu of cross-references in this manual page.
If there is a cross-reference under the mouse button which invoked this
command, it will be the first item on the menu.  Otherwise, they are
on the menu in the order in which they appear in the buffer."
  (interactive "e")
  (let ((buffer (current-buffer))
	(sep "---")
	xref items)
    (cond (event
	   (setq buffer (event-buffer event))
	   (let* ((p (event-point event))
		  (extent (and p (extent-at p buffer 'highlight)))
		  (data (and extent (extent-property extent 'man))))
	     (if (eq (car-safe data) 'Manual-follow-xref)
		 (setq xref (nth 1 data))))))
    (if xref (setq items (list sep xref)))
    (map-extents #'(lambda (extent ignore)
		     (let ((data (extent-property extent 'man)))
		       (if (and (eq (car-safe data) 'Manual-follow-xref)
				(not (member (nth 1 data) items)))
			   (setq items (cons (nth 1 data) items)))
		    nil))
		 buffer)
    (if (eq sep (car items)) (setq items (cdr items)))
    (let ((popup-menu-titles t))
      (and (null items) (setq popup-menu-titles nil))
      (popup-menu
       (cons "Manual Entry"
	     (mapcar #'(lambda (item)
			 (if (eq item sep)
			     item
                           (vector item
                                   (list 'Manual-follow-xref item) t)))
		     (nreverse items)))))))

(defun pager-cleanup-hook ()
  "cleanup man page if called via $PAGER"
  (let ((buf-name (or buffer-file-name (buffer-name))))
	(if (or (string-match "^/tmp/man[0-9]+" buf-name)
		(string-match ".*/man/\\(man\\|cat\\)[1-9a-z]/" buf-name))
	    (let (buffer manpage)
	      (require 'man)
	      (goto-char (point-min))
	      (setq buffer-read-only nil)
	      (Manual-nuke-nroff-bs)
	      (goto-char (point-min))
	      (if (re-search-forward "[^ \t]")
		  (goto-char (- (point) 1)))
	      (if (looking-at "\\([a-zA-Z0-9]+\\)[ \t]*(")
		  (setq manpage (buffer-substring (match-beginning 1)
						  (match-end 1)))
		(setq manpage "???"))
	      (setq buffer
		    (rename-buffer (generate-new-buffer-name
				    (concat "Man: " manpage)))
		    buffer-file-name nil)
	      (goto-char (point-min))
	      (insert (format "%s\n" buf-name))
	      (goto-char (point-min))
	      (buffer-disable-undo buffer)
	      (set-buffer-modified-p nil)
	      (Manual-mode)
	      ))))

(add-hook 'server-visit-hook 'pager-cleanup-hook)
(provide 'man)
