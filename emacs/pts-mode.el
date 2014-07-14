(defvar pts-keywords
  '("module" "import" "export" "assert"
    "Lambda" "lambda" "Pi" "->"
    "if0" "then" "else")
  "Keywords of PTS, that is, reserved words with special
  syntax.")

(defvar pts-primitives
  '("Int" "add" "mul" "sub" "div")
  "Primitives of PTS, that is, reserved words with the same
  syntax as function calls. ")

(defvar pts-interpunctuation
  '(?\. ?\: ?\; ?\=)
  "Interpunctuation characters of PTS, that is, non-space
  non-matching characters that cannot be part of a name.")

(defvar pts-font-lock-keywords
  `((,(regexp-opt pts-keywords 'words) . 'font-lock-keyword-face)
    (,(regexp-opt pts-primitives 'words) . 'font-lock-constant-face)
    (,(concat "[" pts-interpunctuation "]") . 'font-lock-builtin-face)
    ("\\<\\*+\\>" . 'font-lock-constant-face))
  "Font lock configuration for PTS.")

(define-derived-mode pts-mode fundamental-mode "pts"
  "Major mode for editing (literate) pts files."

  ; install font-lock configuration
  (setq font-lock-defaults
    `(pts-font-lock-keywords))

  ; by default, all printable characters are word constituents
  (modify-syntax-entry '(?\x20 . ?\x7F) "w")

  ; whitespace
  (modify-syntax-entry ?\  " ")

  ; interpunctuation
  (dolist (char pts-interpunctuation)
    (modify-syntax-entry char "."))

  ; matching characters
  (modify-syntax-entry ?\( "()")
  (modify-syntax-entry ?\) ")(")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")[")

  ; comments
  (modify-syntax-entry ?\/ "w 14")
  (modify-syntax-entry ?\* "w 23"))

(add-to-list 'auto-mode-alist '("\\.l?pts\\'" . pts-mode))

(provide 'pts-mode)
