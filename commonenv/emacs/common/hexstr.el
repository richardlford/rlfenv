(defun hexstr (hex)
  "hex is a character string in hex.  Convert to ascii and return"
  (let ((n (length hex))
	c1 c2 ca
	)
    (cond
     ((>= n 2)
      (setq c1 (hexch (aref hex 0)))
      (setq c2 (hexch (aref hex 1)))
      (setq ca (+ c2 (* 16 c1)))
      (concat (char-to-string ca) (hexstr (substring hex 2))))
     ((eql n 1)
      (setq ca (hexch (aref hex 0)))
      (char-to-string ca))
     ((eql n 0)
      ""))))

(defun hexch (ch)
  "Return hex number from hex digit"
  (cond
   ((and (<= ?0 ch) (<= ch ?9))
    (- ch ?0))
   ((and (<= ?a ch) (<= ch ?f))
    (+ (- ch ?a) 10))
   ((and (<= ?A ch) (<= ch ?F))
    (+ (- ch ?A) 10))
   (t
    0)))