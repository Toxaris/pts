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
  (unless pts-instance
    (setq-local pts-instance
      (completing-read "PTS instance: " pts-instance-list)))
  pts-instance)

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

(define-derived-mode pts-mode fundamental-mode "pts"
  "Major mode for editing (literate) pts files."
  (pts-setup-fontlock)
  (pts-setup-syntax-table))

(define-key pts-mode-map "\C-c\C-l" 'pts-process-file)

(add-to-list 'auto-mode-alist '("\\.l?pts\\'" . pts-mode))

(provide 'pts-mode)
