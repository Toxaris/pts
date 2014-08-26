(defvar pts-keywords
  '("language" "module" "import" "export" "assert"
    "Lambda" "lambda" "Pi" "->"
    "if0" "then" "else")
  "Keywords of PTS, that is, reserved words with special
  syntax.")

(defvar pts-primitives
  '("Int" "add" "mul" "sub" "div")
  "Primitives of PTS, that is, reserved words with the same
  syntax as function calls. ")

(defvar pts-reserved-words
  (append pts-keywords pts-primitives)
  "Reserved words of PTS.")

(defvar pts-reserved-words-regexp
  (concat "^\\(?:\\*+\\)\\|" (regexp-opt pts-reserved-words) "$")
  "Regular expression to match PTS reserved words.")

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

(defvar pts-command
  "pts"
  "Command to use to call the PTS interpreter.")

(defvar pts-project-directory
  nil
  "The PTS project directory.")

(defvar pts-instance
  nil
  "The PTS instance.")

(defvar pts-instance-list
  (split-string
    (shell-command-to-string
       (concat pts-command " --enumerate-instances=machine-readable")))
  "Available PTS instances.")

(defun pts-setup-fontlock ()
  (setq font-lock-defaults `(pts-font-lock-keywords))
  "Install `font-lock-defaults' for PTS mode.")

(defun pts-setup-syntax-table ()
  "Setup the syntax table for PTS mode."
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

(defun pts-setup-tags ()
  (put 'pts-mode 'find-tag-default-function 'pts-find-tag-default))

(defun pts-module-name ()
  "Find the name of the PTS module in the current buffer.
Returns nil if no module name is found."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "module\\s-+\\(\\w+\\(\\.\\w+\\)*\\)\\s-*;" nil t)
        (match-string 1))))

(defun pts-project-directory ()
  "Return the PTS project directory for the current buffer.
Asks the user if project directory is not known and cannot be guessed."
  (unless pts-project-directory
    (let* ((module-name (or (pts-module-name) ""))
           (parts (reverse (split-string module-name "\\.")))
           (directory (file-name-sans-extension buffer-file-name)))
      (while (and parts (equal (car parts) (file-name-base directory)))
        (setq parts (cdr parts))
        (setq directory (directory-file-name (file-name-directory directory))))
      (unless parts
        (setq-local pts-project-directory directory))))
  (unless pts-project-directory
    (setq-local pts-project-directory
      (read-directory-name "Project directory: " nil nil t)))
  pts-project-directory)

(defun pts-instance ()
  "Returns the PTS instance for the current buffer.
Asks the user if the PTS instance is not known."
  (or (save-excursion
        (goto-char (point-min))
        (when (search-forward-regexp "language\\s-+\\(\\w+\\(\\.\\w+\\)*\\)\\s-*;" nil t)
          (match-string 1)))
      pts-instance
      (setq-local pts-instance
        (completing-read "PTS instance: " pts-instance-list))))

(defun pts-process-file ()
  "Processes the current PTS file."
  (interactive)
  (let* ((project-directory (pts-project-directory))
         (instance (pts-instance))
         (relative-down (file-relative-name buffer-file-name project-directory))
         (relative-up (file-relative-name project-directory default-directory))
         (command (combine-and-quote-strings
                    (list "cd"
                          relative-up
                          "&&"
                          pts-command
                          "--instance" instance
                          "--quiet"
                          relative-down))))
    (compile command)))

(defun pts-variable-name-at-point ()
  "Check whether point is inside a PTS variable name.
If point is inside a PTS variable name, return the
name as a string. Otherwise, return nil.
  This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (let ((candidate (thing-at-point 'symbol)))
    (and (stringp candidate)
         (not (string-match pts-reserved-words-regexp candidate))
         candidate)))

(defun pts-module-import-at-point ()
  "Check whether point is inside a PTS module import.
If point is inside a PTS module import, return the name of the
imported module module. Otherwise, return nil.
  This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (let ((old-point (point)))
    (save-excursion
      (beginning-of-line)
      (and (search-forward-regexp "\\=\\(?:> \\)?import\\s-+\\(\\w+\\(?:\\.\\w+\\)*\\)\\s-*\\(;\\|$\\)" nil t)
           (<= old-point (point))
           (match-string 1)))))

(defun pts-find-tag-default ()
  "Find default for `find-tag' etc."
    (or (pts-module-import-at-point)
        (pts-variable-name-at-point)))

(define-derived-mode pts-mode fundamental-mode "pts"
  "Major mode for editing (literate) pts files."
  (pts-setup-fontlock)
  (pts-setup-syntax-table)
  (pts-setup-tags))

(define-key pts-mode-map "\C-c\C-l" 'pts-process-file)

(add-to-list 'auto-mode-alist '("\\.l?pts\\'" . pts-mode))

(provide 'pts-mode)
