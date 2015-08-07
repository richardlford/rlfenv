(progn
  (setenv "WHICHENV" "Cg5")
  (setq frame-title-format (if (string= buffer-file-name "") 
			       (concat (getenv "WHICHENV") " (%f) [%l]") 
			     (concat (getenv "WHICHENV") " (%b) [%l]"))))
