;; Emacs macro to edit the output from a -dM -E dump from gcc
;; to produce a program that will dump the numeric values
;; of the C macros.

(defun remove-trailing-blanks ()
  "Remove macros definition part.."

  (beginning-of-buffer)
  (while (not (eobp))
    (end-of-line)
    (delete-horizontal-space)
    (beginning-of-line)
    (next-line 1)
    ))



(defun remove-function-macros ()
  "Remove macros that have parameters."

  (beginning-of-buffer)
  (while (not (eobp))
    (lforward-word 1)
    (cond
     ((looking-at "(")
      (beginning-of-line)
      (let ((beg (point)))
	(forward-line 1)
	(delete-region beg (point)))
      )
     (t
      (beginning-of-line)
      (next-line 1)
      ))
    ))

(defun remove-empty-macros ()
  "Remove macros that have empty definition."

  (beginning-of-buffer)
  (while (not (eobp))
    (lforward-word 1)
    (let ((beg (point)))
      (end-of-line)
      (cond
       ((eq beg (point))
	(beginning-of-line)
	(setq beg (point))
	(forward-line 1)
	(delete-region beg (point)))
       (t
	(beginning-of-line)
	(next-line 1)
	)))
    ))

(defun remove-macros-definitions ()
  "Remove macros definition part.."

  (beginning-of-buffer)
  (while (not (eobp))
    (lforward-word 1)
    (let ((beg (point)))
      (end-of-line)
      (delete-region beg (point)))
    ))

(defun sort-macro-file ()
  "sort the macros"
  (beginning-of-buffer)
  (let ((beg (point)))
    (end-of-buffer)
    (sort-lines nil beg (point)))
  )

(defun add-dump-macro ()
  "Put into dump() invocation"

  (beginning-of-buffer)
  (while (not (eobp))
    (insert "  dump(")
    (end-of-line)
    (insert ");")
    (beginning-of-line)
    (next-line 1)
    ))

(defun add-dump-boilerplate ()
  "make into a program"
  (beginning-of-buffer)
  (insert (concat "#include <stdio.h>\n"
		  "#include <machine/ctl_regs.h>\n"
		  "#define dump(arg) printf(\"%8x (hex) %12d (dec) %s\\n\", (int)(arg), (int)(arg), #arg);\n"
		  "main()\n"
		  "{\n"))
  (end-of-buffer)
  (insert "}\n")
  )


(defun edit-dM-file ()
  "edit the output from a -dM -E dump from gcc"
  
  (sort-macro-file)

  (beginning-of-buffer)
  (replace-string "#define " "")

  ;; Remove trailing blanks to facilitate deleting definitions
  ;; that have empty expansions.
  (remove-trailing-blanks)

  (remove-empty-macros)
  (remove-function-macros)
  (remove-macros-definitions)

  (add-dump-macro)

  (add-dump-boilerplate)
)
