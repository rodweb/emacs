(use-package run-command
  :bind
  ("C-c c" . run-command)
  :config
  (defun with-run-command (targets working-dir command-prefix)
    (mapcar (lambda (target)
              (list :command-name target
                    :command-line (concat command-prefix target)
                    :display target
                    :working-dir working-dir))
            targets))

  (defun run-command-recipe-npm--get-targets (package-json-file)
    "Extract npm targets from PACKAGE-JSON."
    (with-temp-buffer
      (insert-file-contents package-json-file)
      (when-let ((script-hash (gethash "scripts" (json-parse-buffer))))
        (let (scripts (list))
          (maphash (lambda (key _value) (push key scripts)) script-hash)
          scripts))))

  (defun run-command-recipe-npm ()
    "Generate commands from package.json."
    (ignore-errors
      (when-let* ((working-dir (locate-dominating-file default-directory ".git"))
                  (package-json (concat working-dir "package.json"))
                  (targets (run-command-recipe-npm--get-targets package-json)))
        (with-run-command targets working-dir "npm run "))))
  (add-to-list 'run-command-recipes #'run-command-recipe-npm)

  (defun run-command-recipe-makefile--get-targets (makefile)
    "Extract make targets from MAKEFILE."
    (with-temp-buffer
      (insert-file-contents makefile)
      (let ((targets (list)))
        (while (re-search-forward "^\\([a-zA-Z0-9-]+\\):" nil t)
          (add-to-list 'targets (match-string-no-properties 1)))
        targets)))

  (defun run-command-recipe-make ()
    "Generate commands from Makefile."
    (when-let* ((working-dir (locate-dominating-file default-directory "Makefile"))
                (makefile (concat working-dir "Makefile"))
                (targets (run-command-recipe-makefile--get-targets makefile)))
      (with-run-command targets working-dir "make ")))
  (add-to-list 'run-command-recipes #'run-command-recipe-make)

  (defun run-command-recipe-terraform ()
    "Generate Terraform commands."
    (if-let ((working-dir (locate-dominating-file default-directory "main.tf"))
             (targets '("init" "fmt" "validate" "plan" "apply")))
        (with-run-command targets working-dir "terraform ")))
  (add-to-list 'run-command-recipes #'run-command-recipe-terraform))

(use-package compile
  :straight nil
  :config
  (require 'ansi-color)
  (defun rod/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'rod/colorize-compilation-buffer))

(provide 'rod-command)
