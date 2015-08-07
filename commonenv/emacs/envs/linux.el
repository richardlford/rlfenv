;
(setenv "WORK" (concat (getenv "HOME") "/work"))
(setenv "WORKROOT" (getenv "WORK"))
(setenv "GNUROOT" (concat (getenv "WORK") "/dev/xscale/nt/gnupro"))
;(setenv "EEMBCROOT" (concat (getenv "WORK") "/CT-SATests/perfC/eembc/benchmark/source/eembc"))
;(setq mk-root-dir (getenv "NUTCROOT"))

;; Stuff for netbatch
(setenv "NBCONF" "//scnbspool.sc.ids1.intel.com/config")
(setenv "NBBIN" "//scnbspool.sc.ids1.intel.com/win32bin")
(setenv "NBPROXY" "scnbspool.sc.ids1.intel.com:3000")
(setenv "NBNOPROXY" "scnb0:orml2:testorml:drwho:scml:scml1:scml2:testscml")
(setenv "NBWD" "//scnbscrwn.sc.ids1.intel.com/scratch4/rlford1")
(setenv "NBNTDOMAINNAME" "AMR")
(setenv "NBNTNOTRANSLATE" "COLON;PATH")

(setenv "SAVEPATH" (getenv "PATH"))
;(prompt-for-insert (concat "Path before: " (getenv "PATH")))
(setenv "PATH" 
	(concat
	 (getenv "HOME") "/bin:"
;				   (getenv "HOME") "/commonenv/bin:"
;				   (getenv "GNUROOT") "/bin:"
;				   (getenv "WORK") "/dev/ia32/nt/picl:"
;				   "C:/Perl/bin:"
;				   "C:/ndt/INTEL/NDT0.2/xdbsimxs:"
;				   "c:/cmplr/bin:"
;				   "c:/J2SDK_Forte/jdk1.4.0/bin:"
;				   "c:/J2SDK_Forte/forte4j/bin/fastjavac:"
;				   "c:/gs/gs7.04/bin:"
;				   "c:/gs/gs7.04/lib:"
;				   "c:/local/bin:"
	 (getenv "NBBIN") ":"
	 "/usr/kerberos/bin:/usr/local/bin:"
	 (getenv "PATH") ":"
	 "/usr/X11R6/bin:"
	 "."
	 ))
;(prompt-for-insert (concat "Path after: " (getenv "PATH")))
;(setenv "SHELL" (concat mk-root-dir "/mksnt/sh.exe"))
(setenv "BISON_HAIRY" "c:/cmplr/bin/bison.hairy")
(setenv "BISON_SIMPLE" "c:/cmplr/bin/bison.simple")
(setenv "CVSREAD" "1")

;; Stuff for building nordheim library
;(setenv "SRC" (concat "c:/rlford1/ulm" "/libc/src"))
;(setenv "LOCALHOST" "NT_2000")
;(setenv "MAKEINCLUDE" (concat (getenv "SRC") "/makeinclude"))

;; REM TC needs this
;(setenv "TESTROOT" "c:/testroot")
(setenv "WHICHENV" "LNX")
;(setenv "ENV" (concat (getenv "HOME") "/environ.ksh"))
(setq frame-title-format (if (string= buffer-file-name "") 
			     (concat (getenv "WHICHENV") " (%f) [%l]") 
			   (concat (getenv "WHICHENV") " (%b) [%l]")))

;;(prompt-for-insert "mks.el")

;(prompt-for-insert "linux.el")