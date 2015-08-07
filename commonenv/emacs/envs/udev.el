(setenv "WORK" (concat (getenv "HOME") "/work"))
(setenv "WORKROOT" (getenv "WORK"))
(setenv "GNUROOT" (concat (getenv "WORK") "/udev/xscale/nt/gnupro"))
(setenv "EEMBCROOT" (concat (getenv "WORK") "/CT-SATests/perfC/eembc/benchmark/source/eembc"))
(setq mk-root-dir (getenv "NUTCROOT"))
(setenv "PATH" (string-subst-char ?/ ?\\
				  (concat
				   (getenv "WORK") "/udev/build_objs/proton_linux_sa_p2gd2_objs/bin;"
				   (getenv "WORK") "/udev/xscale/nt/nordheim/bin;"
				   (getenv "WORK") "/udev/xscale/nt/bin;"
				   (getenv "WORK") "/dev/xscale/nt/nordheim/xdbsimxs;"
				   (concat mk-root-dir "/bin;")
				   (concat mk-root-dir "/bin/x11;")
				   (concat mk-root-dir "/mksnt;")
				   (getenv "HOME") "/bin;"
				   (getenv "PATH") ";"
				   (getenv "GNUROOT") "/bin;"
				   (getenv "WORK") "/udev/ia32/nt/picl;"
				   "C:/Perl/bin;c:/activeperl/bin;"
				   "C:/ndt/INTEL/NDT0.2/xdbsimxs;"
				   "c:/cmplr/bin;"
				   "c:/J2SDK_Forte/jdk1.4.0/bin;"
				   "c:/J2SDK_Forte/forte4j/bin/fastjavac;"
				   "c:/gs/gs7.04/bin;"
				   "c:/gs/gs7.04/lib;"
				   "c:/local/bin;"
				   "."
				   )))
(setenv "SHELL" (concat mk-root-dir "/mksnt/sh.exe"))
(setenv "BISON_HAIRY" "c:/cmplr/bin/bison.hairy")
(setenv "BISON_SIMPLE" "c:/cmplr/bin/bison.simple")
(setenv "CVSREAD" "1")

;; Stuff for building nordheim library
(setenv "SRC" (concat "c:/rlford1/ulm" "/libc/src"))
(setenv "LOCALHOST" "NT_2000")
(setenv "MAKEINCLUDE" (concat (getenv "SRC") "/makeinclude"))

;; REM TC needs this
(setenv "TESTROOT" "c:/testroot")
(setenv "WHICHENV" "UDEV")
(setenv "ENV" (concat (getenv "HOME") "/environ.ksh"))
(setq frame-title-format (if (string= buffer-file-name "") 
			     (concat (getenv "WHICHENV") " (%f) [%l]") 
			   (concat (getenv "WHICHENV") " (%b) [%l]")))
;;(prompt-for-insert "mks.el")
