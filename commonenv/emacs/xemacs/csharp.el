;;; Hey Emacs!  This is -*- Emacs-Lisp -*-
;;;
;;; csharp.el -- an Emacs mode for editing C# code
;;; based on cc-mode.el and its progeny
;;;
;;; 6/11/02 FIXED FOR EMACS VERSIONS 21.1.1 AND HIGHER.  These have a new
;;; version of cc-mode.
;;;
;;; Author: BAL
;;; Version: 0.2
;;;

(require 'cc-mode)
(c-initialize-cc-mode);

(defun csharp-mode ()
  "Major mode for editing C# code, based on CC model.
To see what version of CC mode you are running, enter `\\[c-version]'.

The hook variable `csharp-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the common hook
`c-mode-common-hook' is run first.  Note that this mode automatically
sets the \"csharp\" style before calling any hooks so be careful if you
set styles in `c-mode-common-hook'.

Key bindings:
\\{csharp-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table java-mode-syntax-table)
  (setq major-mode 'csharp-mode
        mode-name "C#"
        local-abbrev-table java-mode-abbrev-table)
  (use-local-map java-mode-map)
  (c-common-init)
  (setq comment-start "// "
        comment-end   ""
        c-conditional-key c-Java-conditional-key
        c-comment-start-regexp c-Java-comment-start-regexp
        c-class-key c-csharp-class-key
        c-method-key nil
        c-baseclass-key nil
        c-recognize-knr-p nil
        c-access-key nil
        c-inexpr-class-key c-Java-inexpr-class-key
        ;defun-prompt-regexp c-Java-defun-prompt-regexp
        imenu-generic-expression cc-imenu-java-generic-expression
        imenu-case-fold-search nil
	c-extra-toplevel-key c-C++-extra-toplevel-key
        )
  (make-local-variable 'font-lock-defaults)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'csharp-mode-hook)
  (c-update-modeline))

(defvar csharp-mode-abbrev-table nil
  "Abbreviation table used in C# mode buffers.")
(define-abbrev-table 'csharp-mode-abbrev-table ())

(defvar csharp-mode-map ()
  "Keymap used in C# mode buffers.")

(if csharp-mode-map
    nil
  (setq csharp-mode-map (c-make-inherited-keymap))
  )

(defvar csharp-mode-syntax-table nil
  "Syntax table used in C# mode buffers.")
(if csharp-mode-syntax-table
    ()
  (setq csharp-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table csharp-mode-syntax-table))

(easy-menu-define c-csharp-menu csharp-mode-map "C# mode Commands"
                  (c-mode-menu "C#"))

(defconst c-csharp-class-key
  (concat
   "\\(" c-protection-key "\\s +\\)?"
   "\\(interface\\|class\\|delegate\\|enum\\)\\s +"
   c-symbol-key                               ;name of the class
   ;;"\\(\\s *extends\\s *" c-symbol-key "\\)?" ;maybe followed by superclass
   "\\(\\s *implements *[^{]+{\\)?"         ;maybe the adopted protocols list
   ))

(defconst c-csharp-access-key nil)

(defconst c-csharp-conditional-key nil)

(defconst c-csharp-inexpr-class-key "\\<new\\>")

(defconst c-csharp-comment-start-regexp "/\\(/\\|[*][*]?\\)")

(defvar cc-imenu-csharp-generic-expression
  (`
   ((nil
     (,
      (concat
       "^\\([ \t]\\)*"
       "\\([A-Za-z0-9_-]+[ \t]+\\)?"          ; type specs; there can be
        "\\([A-Za-z0-9_-]+[ \t]+\\)?"         ; more than 3 tokens, right?
       "\\([A-Za-z0-9_-]+[ \t]*[[]?[]]?\\)"
       "\\([ \t]\\)"
       "\\([A-Za-z0-9_-]+\\)"                 ; the string we want to get
       "\\([ \t]*\\)+("
       "[][a-zA-Z,_1-9\n \t]*"   ; arguments
       ")[ \t]*"
;       "[^;(]"
       "[,a-zA-Z_1-9\n \t]*{"               
       )) 6)))
  "Imenu generic expression for C# mode.  See `imenu-generic-expression'.")


;;; Stuff for font-lock mode

(defconst csharp-font-lock-keywords-1 nil
  "Subdued level highlighting for C# mode.")

(defconst csharp-font-lock-keywords-2 nil
  "Medium level highlighting for C# mode.")

(defconst csharp-font-lock-keywords-3 nil
  "Gaudy level highlighting for C# mode.")

;; Regexps written with help from Fred White <fwhite@bbn.com>,
;; Anders Lindgren <andersl@andersl.com> and Carl Manning <caroma@ai.mit.edu>.
(let* ((csharp-keywords
        (regexp-opt
         '("catch" "checked" 
           "default" "do" "else" "event" "explicit" "extern"
           "super" "this" "finally" "for" "foreach" "if" "implicit"
           "internal" "is" "lock" "object" "operator" "out" 
           "override" "throws" "instanceof" "new" "ref"
           "return" "sizeof" "string" "struct" "switch" "this" "throw" "try" 
           "typeof" "unchecked" "unsafe" "while") t))
       ;;
       ;; Classes immediately followed by an object name.
       (csharp-type-names
        `(mapconcat 'identity
          (cons 
           (,@ (regexp-opt '("bool" "byte" "char" "decimal" "double" 
                             "exdouble" "exfloat" "float" "int" "long"
                             "sbyte" "short" "uint" "ulong" "ushort" "variant" 
                             "void")))
           csharp-font-lock-extra-types)
          "\\|"))
       (csharp-type-names-depth `(regexp-opt-depth (,@ csharp-type-names)))
       ;;
       ;; These are eventually followed by an object name.
       (csharp-type-specs
        (regexp-opt
         '("abstract" "base" "const" "fixed" "sealed" "static" 
           "public" "private" "protected" "readonly" "virtual"
           )))
       )
  (setq csharp-font-lock-keywords-1
        (list
         ;;
         ;; Fontify class names.
         '("\\<\\(class\\|delegate\\|enum\\|interface\\)\\>[ \t]*\\(\\sw+\\)?"
           (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
         ;;
         ;; Fontify package names in import directives.
         '("\\<\\(using\\|package\\|namespace\\)\\>[ \t]*\\(\\sw+\\)?"
           (1 font-lock-keyword-face)
           (2 font-lock-constant-face nil t)
           ("\\=\\.\\(\\*\\|\\sw+\\)" nil nil
            (1 font-lock-constant-face nil t)))
         ))

 (setq csharp-font-lock-keywords-2
  (append csharp-font-lock-keywords-1
   (list
    ;;
    ;; Fontify class names.
    `(eval .
      (cons (concat "\\<\\(" (,@ csharp-type-names) "\\)\\>")
            '(1 font-lock-type-face)))
    ;;
    ;; Fontify all builtin keywords (except below).
    (list (concat "\\<\\(" csharp-keywords "\\|" csharp-type-specs "\\)\\>")
	  '(1 font-lock-keyword-face))
    ;;
    ;; Fontify keywords and targets, and case default/goto tags.
    (list "\\<\\(break\\|case\\|continue\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
          '(1 font-lock-keyword-face) '(2 font-lock-constant-face nil t))
    ;; This must come after the one for keywords and targets.
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*$"
           (beginning-of-line) (end-of-line)
           (1 font-lock-constant-face)))
    ;;
    ;; Fontify all constants.
    '("\\<\\(false\\|null\\|true\\)\\>" . font-lock-constant-face)
    ;;
    )))

 (setq csharp-font-lock-keywords-3
  (append csharp-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   (list
    ;;
    ;; Fontify random types immediately followed by an item or items.
    `(eval .
      (list (concat "\\<\\(" (,@ csharp-type-names) "\\)\\>"
                    "\\([ \t]*\\[[ \t]*\\]\\)*"
                    "\\([ \t]*\\sw\\)")
            ;; Fontify each declaration item.
            (list 'font-lock-match-c-style-declaration-item-and-skip-to-next
                  ;; Start and finish with point after the type specifier.
                  (list 'goto-char (list 'match-beginning
                                         (+ (,@ csharp-type-names-depth) 3)))
                  (list 'goto-char (list 'match-beginning
                                         (+ (,@ csharp-type-names-depth) 3)))
                  ;; Fontify as a variable or function name.
                  '(1 (if (match-beginning 2)
                          font-lock-function-name-face
                        font-lock-variable-name-face)))))
    ;;
    ;; Fontify those that are eventually followed by an item or items.
    (list (concat "\\<\\(" csharp-type-specs "\\)\\>"
                  "\\([ \t]+\\sw+\\>"
                  "\\([ \t]*\\[[ \t]*\\]\\)*"
                  "\\)*")
          ;; Fontify each declaration item.
          '(font-lock-match-c-style-declaration-item-and-skip-to-next
            ;; Start with point after all type specifiers.
            (goto-char (or (match-beginning 5) (match-end 1)))
            ;; Finish with point after first type specifier.
            (goto-char (match-end 1))
            ;; Fontify as a variable or function name.
            (1 (if (match-beginning 2)
                   font-lock-function-name-face
                 font-lock-variable-name-face))))
    )))
 )

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.")

(defcustom csharp-font-lock-extra-types
  '("[A-Z\300-\326\330-\337]\\sw*[a-z]\\sw)*")
  "*List of extra types to fontify in C# mode.
Each list item should be a regexp not containing word-delimiters.
For example, a value of (\"[A-Z\300-\326\330-\337]\\\\sw*[a-z]\\\\sw*\") means capitalised words (and words conforming to the C# spec) are treated as type names.

The value of this variable is used when Font Lock mode is turned on."
  :type 'font-lock-extra-types-widget
  :group 'font-lock-extra-types)
