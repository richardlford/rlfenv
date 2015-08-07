;;; DO NOT MODIFY THIS FILE
(if (featurep 'debug-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "debug/_pkg.el")

(package-provide 'debug :version 1.11 :type 'regular)

;;;***

;;;### (autoloads (gdb-with-core gdb) "gdb" "debug/gdb.el")

(defvar gdb-command-name "gdb" "\
Pathname for executing gdb.")

(autoload 'gdb "gdb" "\
Run gdb on program FILE in buffer *gdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for GDB.  If you wish to change this, use
the GDB commands `cd DIR' and `directory'." t nil)

(autoload 'gdb-with-core "gdb" "\
Debug a program using a corefile." t nil)

;;;***

;;;### (autoloads (gdbsrc) "gdbsrc" "debug/gdbsrc.el")

(autoload 'gdbsrc "gdbsrc" "\
Activates a gdb session with gdbsrc-mode turned on.  A numeric prefix
argument can be used to specify a running process to attach, and a non-numeric
prefix argument will cause you to be prompted for a core file to debug." t nil)

;;;***

;;;### (autoloads (perldb xdb dbx sdb) "gud" "debug/gud.el")

(autoload 'sdb "gud" "\
Run sdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload 'dbx "gud" "\
Run dbx on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload 'xdb "gud" "\
Run xdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

You can set the variable 'gud-xdb-directories' to a list of program source
directories if your program contains sources from more than one directory." t nil)

(autoload 'perldb "gud" "\
Run perldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

;;;***

;;;### (autoloads (pdb) "pdb" "debug/pdb.el")

(autoload 'pdb "pdb" "\
Run pdb on program FILE in buffer *gud-FILE*.
      The directory containing FILE becomes the initial working directory
      and source-file directory for your debugger." t nil)

;;;***

(provide 'debug-autoloads)
