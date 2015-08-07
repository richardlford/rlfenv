;; lxgdbsrc.el - run lx gdb in gdbsrc mode.
(require 'gdbsrc "gdbsrc")

(defun lxgdbsrc (path &optional core-or-pid)
  "Activates a gdb session with gdbsrc-mode turned on.  A numeric prefix
argument can be used to specify a running process to attach, and a non-numeric
prefix argument will cause you to be prompted for a core file to debug."
  (interactive (let ((file (read-file-name "Program to debug: " nil nil t)))
		 (cond ((numberp current-prefix-arg)
			(list file (int-to-string current-prefix-arg)))
		       (current-prefix-arg
			(list file (read-file-name "Core file: " nil nil t)))
		       (t (list file)))
		 ))
  (let ((gdb-command-name "C:/rlford1/work/dev/xscale/nt/gnupro/bin/xscale-elf-gdb.EXE"))
    (gdbsrc path core-or-pid)
    ))

(defun mxgdbsrc (path &optional core-or-pid)
  "Activates a gdb session with gdbsrc-mode turned on.  A numeric prefix
argument can be used to specify a running process to attach, and a non-numeric
prefix argument will cause you to be prompted for a core file to debug."
  (interactive)
  (let ((gdb-command-name "~/bin/mygdb"))
    (gdbsrc "")
    ))

(defun mygdbsrc (path &optional core-or-pid)
  "Activates a gdb session with gdbsrc-mode turned on.  A numeric prefix
argument can be used to specify a running process to attach, and a non-numeric
prefix argument will cause you to be prompted for a core file to debug."
  (interactive (let ((file (read-file-name "Program to debug: " nil nil t)))
		 (cond ((numberp current-prefix-arg)
			(list file (int-to-string current-prefix-arg)))
		       (current-prefix-arg
			(list file (read-file-name "Core file: " nil nil t)))
		       (t (list file)))
		 ))
  (let ((gdb-command-name "/cygnus/PentiumIII-000906/H-i686-pc-cygwin/bin/gdb.exe"))
    (gdbsrc path core-or-pid)
    ))

(def-gdb-from-src "nexti"  "j" "Step one insn line (skip functions)"
  (gdb-delete-arrow-extent))
