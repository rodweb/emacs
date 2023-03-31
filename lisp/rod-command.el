(use-package run-command
  :bind
  ("C-c c" . run-command)
  :custom
  (run-command-default-runner 'run-command-runner-compile)
  :config
  (defun with-run-command (targets working-dir command-prefix &optional file)
    (mapcar (lambda (target)
              (let* ((command-name (if (consp target) (cdr target) target))
                     (target (if (consp target) (car target) target))
                     (command-line (concat
                                    command-prefix
                                    (if file (concat target " " (buffer-file-name)) target))))
                (list :command-name command-name
                      :command-line command-line
                      :display command-name
                      :working-dir working-dir)))
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
    "Generates commands for npm."
    (when-let* ((working-dir (locate-dominating-file default-directory "package.json"))
                (targets '("clean-install" "install" "install --force" "audit" "audit fix" "audit fix --force" "version patch" "publish")))
      (with-run-command targets working-dir "npm ")))
  (add-to-list 'run-command-recipes #'run-command-recipe-npm)

  (defun run-command-recipe-npm-scripts ()
    "Generate commands from package.json."
    (ignore-errors
      (when-let* ((working-dir (locate-dominating-file default-directory "package.json"))
                  (package-json (concat working-dir "package.json"))
                  (targets (run-command-recipe-npm--get-targets package-json)))
        (with-run-command targets working-dir "npm run "))))
  (add-to-list 'run-command-recipes #'run-command-recipe-npm-scripts)

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

  (defun run-command--get-terraform-script (main-tf-dir)
    (let ((custom-tf-script-dir (locate-dominating-file default-directory "deploy")))
      (if (string-equal main-tf-dir custom-tf-script-dir)
          "./deploy "
        "terraform ")))

  (defun run-command-recipe-terraform ()
    "Generate Terraform commands."
    (if-let ((working-dir (locate-dominating-file default-directory "main.tf"))
             (targets '("init" "fmt" "validate" "plan" "apply")))
        (with-run-command targets working-dir (run-command--get-terraform-script working-dir))))
  (add-to-list 'run-command-recipes #'run-command-recipe-terraform)

  (defun run-command-recipe-golang ()
    "Generate Go commands."
    (if-let ((working-dir (and (s-ends-with? ".go" buffer-file-name) (file-name-directory (buffer-file-name))))
             (targets '("build" "test" "run" "fmt")))
        (with-run-command targets working-dir "go " t)))
  (add-to-list 'run-command-recipes #'run-command-recipe-golang)

  (defun run-command-recipe-podman ()
    "Generate Pod commands."
    (if-let ((working-dir (locate-dominating-file default-directory "Dockerfile"))
             (targets '("machine start" "machine stop")))
        (with-run-command targets working-dir "podman ")))
  (add-to-list 'run-command-recipes #'run-command-recipe-podman)

  (defun run-command-recipe-lerna ()
    "Generate Lerna commands"
    (if-let ((working-dir (locate-dominating-file default-directory "lerna.json"))
             (targets '("bootstrap")))
        (with-run-command targets working-dir "lerna ")))
  (add-to-list 'run-command-recipes #'run-command-recipe-lerna)

  (defun run-command-recipe-json ()
    "Generate JSON commands."
    (when (eq major-mode 'json-mode)
      nil))
  (add-to-list 'run-command-recipes #'run-command-recipe-json)

  (defun run-command-recipe-docker-compose ()
    "Generate docker-compose commands."
    (when-let* ((working-dir (locate-dominating-file default-directory "docker-compose.yml"))
                (prefix "docker-compose "))
      (list
       (list :command-name "up detached"
             :command-line (concat prefix "up -d")
             :working-dir working-dir))))
  (add-to-list 'run-command-recipes #'run-command-recipe-docker-compose))

(use-package alert :defer t)

(use-package compile
  :straight nil
  :config
  (require 'ansi-color)
  (defun rod/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (defun rod/notify-compilation-finished (buffer exit-string)
    "Notifies when the compilation has finished."
    ;; TODO: not working on Mac
    (alert "compilation finished"
           :severity (if (string-prefix-p "exited abnormally" exit-string)
                         'high
                       'normal)))
  (add-hook 'compilation-filter-hook 'rod/colorize-compilation-buffer)
  (add-hook 'compilation-finish-functions 'rod/notify-compilation-finished))

(provide 'rod-command)
