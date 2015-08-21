(put 'narrow-to-region 'disabled nil)
(setq-default abbrev-mode 't)
(setq abbrev-file-name (concat home-dir ".abbrev_defs"))
(if (file-readable-p abbrev-file-name)
    (read-abbrev-file (concat home-dir ".abbrev_defs"))
  (if (file-exists-p abbrev-file-name)
      (message (concat "Cannot read file " abbrev-file-name))
    (progn
      (find-file abbrev-file-name)
      (insert "; Abbreviation file\n")
      (save-buffer))
    ))
      
;;; Short functions
(defun reg () "current region" (buffer-substring (region-beginning) (region-end)))
(defun ireg () "current region as an integer" (string-to-int (reg)))
(defun nreg () "current region as an number" (string-to-number (reg)))
(defun iins (num) "insert an integer" (insert (int-to-string num)))
(defun fins (num &optional width digits)
  "Insert fixed point number with DIGITS digits after the decimal"
  (let ((wid (if width width 0))
	(dig (if digits digits 0)))
    (insert (format (format "%%%d.%df" wid dig) num))))
(defun cw () "current word"
  (save-excursion
    (let (beg end)
      (if (looking-at "\\w") (forward-word 1))
      (backward-word 1)
      (setq beg (point)) (forward-word 1) (setq end (point))
      (buffer-substring beg end))))
(defun icw () "current word as integer" (string-to-int (cw)))
(defun ct ()
  "Current thing"
  (interactive)
  (let* ((bds (thing-boundaries (point)))
	 (str (buffer-substring (car bds) (cdr bds))))
    str))
(defun c-quote-line ()
  (interactive)
  (end-of-line)
  (indent-to 79 1)
  (insert "\\")
  (forward-char)
  )

(defun idt ()
  "Insert date-time"
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%H%M"))
  )

(defun im3 ()
  "Insert nmake BUILD=TEST TARGET=X86"
  (interactive)
  (insert "nmake BUILD=TEST TARGET=X86")
  )

(defun imrh ()
  "Insert nmake BUILD=TEST TARGET=X86"
  (interactive)
  (insert "nmake BUILD=TEST TARGET=X86 REDHAWK=1")
  )

(defun im6 ()
  "Insert nmake BUILD=TEST TARGET=AMD64"
  (interactive)
  (insert "nmake BUILD=TEST TARGET=AMD64")
  )

(defun im66 ()
  "Insert nmake BUILD=TEST HOST=AMD64 TARGET=AMD64"
  (interactive)
  (insert "nmake BUILD=TEST HOST=AMD64 TARGET=AMD64")
  )

(defun imr3 ()
  "Insert nmake TARGET=X86 BUILD=RELEASE SkipCrossBuilds=1"
  (interactive)
  (insert "nmake TARGET=X86 BUILD=RELEASE SkipCrossBuilds=1")
  )

(defun imr6 ()
  "Insert nmake TARGET=AMD64 BUILD=RELEASE SkipCrossBuilds=1"
  (interactive)
  (insert "nmake TARGET=AMD64 BUILD=RELEASE SkipCrossBuilds=1")
  )

(defun imr66 ()
  "Insert nmake HOST=AMD64 TARGET=AMD64 BUILD=RELEASE SkipCrossBuilds=1"
  (interactive)
  (insert "nmake HOST=AMD64 TARGET=AMD64 BUILD=RELEASE SkipCrossBuilds=1")
  )

(defun isk ()
  "Insert SkipCrossBuilds=1 sepcomp=none"
  (interactive)
  (insert " SkipCrossBuilds=1 sepcomp=none")
  )

(defun is3 ()
  "Insert nmake BUILD=TEST SkipCrossBuilds=1 sepcomp=none"
  (interactive)
  (im3)
  (isk)
  )

(defun isrh ()
  "Insert nmake SkipCrossBuilds=1 sepcomp=none"
  (interactive)
  (imrh)
  (isk)
  )

(defun is6 ()
  "Insert nmake amd64 skip"
  (interactive)
  (im6)
  (isk)
  )

(defun is66 ()
  "Insert nmake amd64 amd64 skip"
  (interactive)
  (im66)
  (isk)
  )

(defun is3 ()
  "Insert nmake SkipCrossBuilds=1 sepcomp=none"
  (interactive)
  (im3)
  (isk)
  )

(defun is6 ()
  "Insert nmake amd64 skip"
  (interactive)
  (im6)
  (isk)
  )

(defun is66 ()
  "Insert nmake amd64 amd64 skip"
  (interactive)
  (im66)
  (isk)
  )

(defun isr3 ()
  "Insert nmake SkipCrossBuilds=1 sepcomp=none"
  (interactive)
  (imr3)
  (isk)
  )

(defun isr6 ()
  "Insert nmake amd64 skip"
  (interactive)
  (imr6)
  (isk)
  )

(defun isr66 ()
  "Insert nmake amd64 amd64 skip"
  (interactive)
  (imr66)
  (isk)
  )

(defun imsh6 ()
  "Insert nmake "
  (interactive)
  (insert "nmake SELFHOST=RELEASE-X86\\CLR\\RELEASE-AMD64 sepcomp=none")
  )

(defun imsh6d ()
  "Insert nmake "
  (interactive)
  (insert "nmake SELFHOST=RELEASE-X86\\DEBUG-AMD64-CLR EXTRA_BCFLAGS=/Zi")
  )

(defun imsh3 ()
  "Insert nmake "
  (interactive)
  (insert "nmake SELFHOST=RELEASE-X86\\RELEASE-X86")
  )

(defun imsh3d ()
  "Insert nmake "
  (interactive)
  (insert "nmake SELFHOST=RELEASE-X86\\DEBUG-X86 EXTRA_BCFLAGS=/Zi")
  )

(defun imt ()
  "Insert nmake TARGET=THUMB2"
  (interactive)
  (insert "nmake TARGET=THUMB2")
  )

(defun ist ()
  "Insert nmake SkipCrossBuilds=1 TARGET=THUMB2"
  (interactive)
  (imt)
  (isk)
  )

(defun imrt ()
  "Insert nmake TARGET=THUMB2 BUILD=RELEASE SkipCrossBuilds=1"
  (interactive)
  (insert "nmake TARGET=THUMB2 BUILD=RELEASE SkipCrossBuilds=1")
  )

(defun isrt ()
  "Insert nmake SkipCrossBuilds=1 TARGET=THUMB2"
  (interactive)
  (imrt)
  (isk)
  )

(defun imsht ()
  "Insert nmake "
  (interactive)
  (insert "nmake SELFHOST=RELEASE-X86\\RELEASE-THUMB2")
  )

(defun imdcor ()
  "Insert nmake BUILD=TEST HOST=X86 TARGET=X86 GC=CLR  EXTRA_BCFLAGS=/Ox VIAMDIL=1 SEPCOMP=mscorlib"
  (interactive)
  (insert "nmake BUILD=TEST HOST=X86 TARGET=X86 GC=CLR  EXTRA_BCFLAGS=/Ox VIAMDIL=1 SEPCOMP=mscorlib")
  )

(defun impcor6 ()
  "Insert nmake BUILD=TEST HOST=X86 TARGET=AMD64 GC=CLR EXTRA_BCFLAGS=\"/asynchronousdelegates=true /irimprovetypes=false /genericinterfacesonarrays=true /Ox\" SEPCOMP=mscorlib"
  (interactive)
  (insert "nmake BUILD=TEST HOST=X86 TARGET=AMD64 GC=CLR EXTRA_BCFLAGS=\"/asynchronousdelegates=true /irimprovetypes=false /genericinterfacesonarrays=true /Ox\" SEPCOMP=mscorlib")
  )

(defun impwf6 ()
  "Insert nmake BUILD=TEST HOST=X86 TARGET=AMD64 GC=CLR EXTRA_BCFLAGS=\"/asynchronousdelegates=true /irimprovetypes=false /genericinterfacesonarrays=true /Ox\" SEPCOMP=winforms"
  (interactive)
  (insert "nmake BUILD=TEST HOST=X86 TARGET=AMD64 GC=CLR EXTRA_BCFLAGS=\"/asynchronousdelegates=true /irimprovetypes=false /genericinterfacesonarrays=true /Ox\" SEPCOMP=winforms")
  )

(defun impcor ()
  "Insert nmake BUILD=TEST HOST=X86 TARGET=X86 GC=CLR EXTRA_BCFLAGS=\"/asynchronousdelegates=true /irimprovetypes=false /genericinterfacesonarrays=true /Ox\" SEPCOMP=mscorlib"
  (interactive)
  (insert "nmake BUILD=TEST HOST=X86 TARGET=X86 GC=CLR EXTRA_BCFLAGS=\"/asynchronousdelegates=true /irimprovetypes=false /genericinterfacesonarrays=true /Ox\" SEPCOMP=mscorlib")
  )

(defun impwf ()
  "Insert nmake BUILD=TEST HOST=X86 TARGET=X86 GC=CLR EXTRA_BCFLAGS=\"/asynchronousdelegates=true /irimprovetypes=false /genericinterfacesonarrays=true /Ox\" SEPCOMP=winforms"
  (interactive)
  (insert "nmake BUILD=TEST HOST=X86 TARGET=X86 GC=CLR EXTRA_BCFLAGS=\"/asynchronousdelegates=true /irimprovetypes=false /genericinterfacesonarrays=true /Ox\" SEPCOMP=winforms")
  )

(defun ijj ()
  "Insert jjpack pack command  date-time"
  (interactive)
  (insert (format-time-string "jjpack pack n:/%Y-%m-%d-%H%M-zapp.jjp"))
  )

(defun ijjg ()
  "Insert jjpack pack command with date-time for generic sharing main directory"
  (interactive)
  (insert (format-time-string "jjpack pack c:/PublicD/%Y-%m-%d-%H%M-generic-sharing.jjp"))
  )

(defun ibb ()
  "Insert bbpack pack command with date-time"
  (interactive)
  (insert (format-time-string "bbpack.cmd -o c:/PublicD/%Y-%m-%d-%H%M-zapp.cmd -c zapp"))
  )

(defun ibbg ()
  "Insert bbpack pack command with date-time for generic sharing main directory"
  (interactive)
  (insert (format-time-string "bbpack.cmd -o c:/PublicD/%Y-%m-%d-%H%M-generic-sharing.cmd"))
  )

(defun ipp ()
  "Insert pack command  date-time"
  (interactive)
  (insert (format-time-string "pack.cmd n:/%Y-%m-%d-%H%M-zapp -c zapp"))
  )

(defun ippg ()
  "Insert pack command with date-time for generic sharing main directory"
  (interactive)
  (insert (format-time-string "pack.cmd n:/%Y-%m-%d-%H%M-generic-sharing-tool"))
  )

(defun pwz ()
  "Insert jjpack pack w:/date-time"
  (interactive)
  (insert (format-time-string "jjpack pack \\\\midweb\\scratch\\richford\\%Y-%m-%d-%H%M-.jjp -p zip"))
  )

(defun dpp ()
  "Insert dpack command  date-time"
  (interactive)
  (insert (format-time-string "&{$p = $pwd;try {cd ..; dpack.ps1 %Y-%m-%d-%H%M-.jjp} finally {cd $p}}"))
  )

(defun dpm ()
  "Insert dpack command  date-time"
  (interactive)
  (insert (format-time-string "(cd ..; dpack.cmd %Y-%m-%d-%H%M-main)"))
  )

(defun dppg ()
  "Insert dpack command with date-time for generic sharing main directory"
  (interactive)
  (insert (format-time-string "(cd ..; dpack.cmd %Y-%m-%d-%H%M-generic-sharing-tool)"))
  )

(defun idiff ()
  "Insert diff command with date-time"
  (interactive)
  (insert (format-time-string "sd diff -du > %Y-%m-%d-%H%M.diffs"))
  )

(defun ireopen ()
  "Insert sd reopen -c default ... command"
  (interactive)
  (insert "sd reopen -c default ../...")
  )

(defun irbobj ()
  "Insert rmdir of obj dirs"
  (interactive)
  (insert (format-time-string "rm -rf obj gen src/vsbrowse/bin src/vsbrowse/obj ../Bartok.obj"))
  )

(defun irobjs ()
  "Insert rmdir of obj dirs"
  (interactive)
  (insert (format-time-string "rm -rf obj gen src/vsbrowse/bin src/vsbrowse/obj ../Bartok.obj ../Midori.obj ../Phoenix.obj ../Phoenix/binaries ../Phoenix/gen"))
  )

(defun icrpc ()
  "Insert BARTOK_COMPILER CreateReproPackage for cygwin"
  (interactive)
  (insert "export BARTOK_COMPILER=\"/CreateReproPackage=true /timestamps=true\"")
  )

(defun icrpp ()
  "Insert BARTOK_COMPILER CreateReproPackage for Powershell"
  (interactive)
  (insert "$env:BARTOK_COMPILER=\"/CreateReproPackage=true /timestamps=true\"")
  )

(defun ikrs ()
  "Insert BARTOK_COMPILER KeepReprobuilderScript for Powershell"
  (interactive)
  (insert "$env:BARTOK_COMPILER=\"/KeepReprobuilderScript=true\"")
  )

(defun imsr ()
  "Insert \\\\midweb\\scratch\\richford\\"
  (interactive)
  (insert "\\\\midweb\\scratch\\richford\\")
  )

;; Functions to set title and maybe more.

(defun cd1 ()
  "Setup cygwin session in c:/doc1/main/Midori/Internal/Docs/Notes"
  (interactive)
  (setenv "WHICHENV" "CD1")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/doc1/main/Midori/Internal/Docs/Notes")
  )
 
(defun cl0 ()
  "Setup cygwin session in c:/long0/tools/bartok"
  (interactive)
  (setenv "WHICHENV" "CL0")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/long0/LongBartok/tools/bartok")
  )
 
(defun cl1 ()
  "Setup cygwin session in c:/long1/tools/bartok"
  (interactive)
  (setenv "WHICHENV" "CL1")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/long1/LongBartok/tools/bartok")
  )
 
(defun cl2 ()
  "Setup cygwin session in c:/long2/tools/bartok"
  (interactive)
  (setenv "WHICHENV" "CL2")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/long2/LongBartok/tools/bartok")
  )
 
(defun cl3 ()
  "Setup cygwin session in c:/long3/tools/bartok"
  (interactive)
  (setenv "WHICHENV" "CL3")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/long3/LongBartok/tools/bartok")
  )
 
(defun cl4 ()
  "Setup cygwin session in c:/long4/tools/bartok"
  (interactive)
  (setenv "WHICHENV" "CL4")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/long4/LongBartok/tools/bartok")
  )
 
(defun cl5 ()
  "Setup cygwin session in c:/long5/tools/bartok"
  (interactive)
  (setenv "WHICHENV" "CL5")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/long5/LongBartok/tools/bartok")
  )
 
(defun cd0 ()
  "Setup cygwin session in c:/e/d0/bartok"
  (interactive)
  (setenv "WHICHENV" "CD0")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/d0/bartok")
  )
 
(defun cm0 ()
  "Setup cygwin session in c:/e/m0/bartok"
  (interactive)
  (setenv "WHICHENV" "CM0")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/m0/bartok")
  )
 
(defun cm1 ()
  "Setup cygwin session in c:/e/m1/bartok"
  (interactive)
  (setenv "WHICHENV" "CM1")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/m1/bartok")
  )
 
(defun cm2 ()
  "Setup cygwin session in c:/e/m2/bartok"
  (interactive)
  (setenv "WHICHENV" "CM2")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/m2/bartok")
  )
 
(defun cm3 ()
  "Setup cygwin session in c:/e/m3/bartok"
  (interactive)
  (setenv "WHICHENV" "CM3")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/m3/bartok")
  )

(defun ce0 ()
  "Setup cygwin session in c:/e/e0/bartok"
  (interactive)
  (setenv "WHICHENV" "CE0")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/e0/bartok")
  )

 
(defun cf0 ()
  "Setup cygwin session in c:/e/f0/bartok"
  (interactive)
  (setenv "WHICHENV" "CF0")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/f0/bartok")
  )

(defun cf1 ()
  "Setup cygwin session in c:/e/f1/bartok"
  (interactive)
  (setenv "WHICHENV" "CF1")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/f1/bartok")
  )

(defun ck1 ()
  "Setup cygwin session in c:/e/k1/bartok"
  (interactive)
  (setenv "WHICHENV" "CK1")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/k1/bartok")
  )

(defun ct0 ()
  "Setup cygwin session in c:/e/t0/bartok"
  (interactive)
  (setenv "WHICHENV" "CT0")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/t0/bartok")
  )
 
(defun ct1 ()
  "Setup cygwin session in c:/e/t1/bartok"
  (interactive)
  (setenv "WHICHENV" "CT1")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/t1/bartok")
  )
 
(defun ctr ()
  "Setup cygwin session in c:/e/t1/bartok"
  (interactive)
  (setenv "WHICHENV" "CTR")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/tr/bartok")
  )
 
(defun ct2 ()
  "Setup cygwin session in c:/e/t2/bartok"
  (interactive)
  (setenv "WHICHENV" "CT2")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/t2/bartok")
  )
 
(defun ct3 ()
  "Setup cygwin session in c:/e/t3/bartok"
  (interactive)
  (setenv "WHICHENV" "CT3")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/t3/bartok")
  )
 
(defun ct4 ()
  "Setup cygwin session in c:/e/t4/bartok"
  (interactive)
  (setenv "WHICHENV" "CT4")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/t4/bartok")
  )
 
(defun ct5 ()
  "Setup cygwin session in c:/e/t5/bartok"
  (interactive)
  (setenv "WHICHENV" "CT5")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/t5/bartok")
  )
 
(defun ct6 ()
  "Setup cygwin session in c:/e/t6/bartok"
  (interactive)
  (setenv "WHICHENV" "CT6")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/t6/bartok")
  )
 
(defun ct7 ()
  "Setup cygwin session in c:/e/t7/bartok"
  (interactive)
  (setenv "WHICHENV" "CT7")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/t7/bartok")
  )
 
(defun ct8 ()
  "Setup cygwin session in c:/e/t8/bartok"
  (interactive)
  (setenv "WHICHENV" "CT8")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/t8/bartok")
  )
 
(defun ct9 ()
  "Setup cygwin session in c:/e/t9/bartok"
  (interactive)
  (setenv "WHICHENV" "CT9")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/t9/bartok")
  )
 
(defun cta ()
  "Setup cygwin session in c:/e/ta/bartok"
  (interactive)
  (setenv "WHICHENV" "CTa")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/ta/bartok")
  )
 
(defun ctb ()
  "Setup cygwin session in c:/e/tb/bartok"
  (interactive)
  (setenv "WHICHENV" "CTb")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/e/tb/bartok")
  )
 
(defun pm0 ()
  "Setup powershell session in c:/mid1/Midori"
  (interactive)
  (setenv "WHICHENV" "PM0")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell-powershell)
  (shell-cd "c:/mid0/Midori")
  (global-set-key [(f8)] 'myshell-powershell)
  )
 
(defun pm1 ()
  "Setup powershell session in c:/mid1/Midori"
  (interactive)
  (setenv "WHICHENV" "PM1")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell-powershell)
  (shell-cd "c:/mid1/Midori")
  (global-set-key [(f8)] 'myshell-powershell)
  )
 
(defun pm2 ()
  "Setup powershell session in c:/mid1/Midori"
  (interactive)
  (setenv "WHICHENV" "PM2")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell-powershell)
  (shell-cd "c:/mid2/Midori")
  (global-set-key [(f8)] 'myshell-powershell)
  )
 
(defun pm3 ()
  "Setup powershell session in c:/mid1/Midori"
  (interactive)
  (setenv "WHICHENV" "PM3")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell-powershell)
  (shell-cd "c:/mid3/Midori")
  (global-set-key [(f8)] 'myshell-powershell)
  )
 
(defun pm4 ()
  "Setup powershell session in c:/mid1/Midori"
  (interactive)
  (setenv "WHICHENV" "PM4")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell-powershell)
  (shell-cd "c:/mid4/Midori")
  )
 
(defun pa1 ()
  "Setup powershell session in c:/async1/Midori"
  (interactive)
  (setenv "WHICHENV" "PA1")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell-powershell)
  (shell-cd "c:/async1/Midori")
  )
 
(defun cp1 ()
  "Setup cygwin session in c:/phx1/phoenix"
  (interactive)
  (setenv "WHICHENV" "CP1")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd "c:/phx1/phoenix")
  )

(defun cx1 ()
  "Setup xplat development in $datadrive/bm"
  (interactive)
  (setenv "WHICHENV" "CX1")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd (concat (getenv "datadrive") "/bm"))
  )

(defun cx2 ()
  "Setup xplat development in $datadrive/bm"
  (interactive)
  (setenv "WHICHENV" "CX2")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd (concat (getenv "datadrive") "/bm"))
  )

(defun cx3 ()
  "Setup xplat development in $datadrive/bm"
  (interactive)
  (setenv "WHICHENV" "CX3")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd (concat (getenv "datadrive") "/bm"))
  )

(defun cx4 ()
  "Setup xplat development in $datadrive/bm"
  (interactive)
  (setenv "WHICHENV" "CX4")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd (concat (getenv "datadrive") "/bm"))
  )

(defun cx5 ()
  "Setup xplat development in $datadrive/bm"
  (interactive)
  (setenv "WHICHENV" "CX5")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd (concat (getenv "datadrive") "/bm"))
  )

(defun cx6 ()
  "Setup xplat development in $datadrive/bm"
  (interactive)
  (setenv "WHICHENV" "CX6")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]")))
  (myshell)
  (shell-cd (concat (getenv "datadrive") "/bm"))
  )

;;;(setq manual-program "/afs/ri/user/richford/bin/man")
(defun insert-gets () "Insert := "(interactive) (insert " := "))
(defun insert-Maribila () "Insert ANDF "(interactive) (insert "Maribila"))

(defun toggle-tags-always-exact ()
  "Toggle the value of tags-always-exact"
  (interactive)
  (setq tags-always-exact (not tags-always-exact)))

;;; The following are from Larry Smith
;;; The following functions work in emacs 18.56.
;;; 
;;; Place point and mark within the first and last lines of a macro
;;; definition (either in a C++ file or a shell script), then type
;;; 
;;; M-x \-region
;;; 
;;; and it puts a line of backslashes one past the longest line in the
;;; region (replacing those that are there if any).
;;; 
;;; M-x un\-region
;;; 
;;; removes them.

(defvar backslash-char 92)

(defun unbackslashify-region (pos beg end)
  "Removes a line of backslashes from the end of the line"
  (interactive "P\nr")
  (let ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (progn (end-of-line)
		    (< (point)end-marker))
	(if (= (char-after (- (point) 1))
	       backslash-char)
	    (backward-delete-char 1))
	(delete-horizontal-space)
	(forward-line 1)))))

(defun backslashify-region (pos beg end)
  "Puts a line of backslashes one past the longest line in the region"
  (interactive "P\nr")
  (let ((widest 0)
	(end-marker (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (progn (end-of-line)
		    (< (point)end-marker))
	(if (= (char-after (- (point) 1))
	       backslash-char)
	    (backward-delete-char 1))
	(delete-horizontal-space)
	(if (< widest (current-column))
	    (setq widest (current-column)))
	(forward-line 1))
      (end-of-line)
      (if (< widest (current-column))
	    (setq widest (current-column))))
    (if (null pos)
	(setq pos (+ 2 widest)))
    (save-excursion
      (goto-char beg)
      (while (progn (end-of-line)
		    (< (point) end-marker))
	(indent-to-column pos)
	(insert backslash-char)
	(forward-line 1)))))
  
(load "zap")

(setq auto-mode-alist (append auto-mode-alist '(("\\.h\\'" . c++-mode)
						("\\.ii\\'" . c++-mode)
						("\\.il\\'" . c++-mode)
						("\\.pizza\\'" . java-mode)
						)))

(defun igt ()
  (interactive)
  (let* ((dir0 (default-directory))
	 (dir1 (split-string-by-char dir0 ?\\))
	 (dir2 (mapconcat (function (lambda (x) x)) dir1 "/"))
	 )
    (insert (format "gtail -f %sbcdump.log" dir2))
    )
)