(setq native-comp-async-report-warnings-errors 'silent)

(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(add-hook 'after-init-hook #'(lambda () (message "Emacs took %s to load." (emacs-init-time))))

(defun rod/make-module (name)
  "Create a new module at `user-emacs-directory' lisp directory.
   NAME is the module suffix."
  (interactive "sName: ")
  (let* ((filename (concat user-emacs-directory "lisp/" "rod-" name ".el"))
         (buffer (get-buffer-create filename)))
    (with-current-buffer (or (get-buffer "init.el") (get-buffer "init.el<emacs>"))
      (goto-char (point-max))
      (insert (concat "(require 'rod-" name ")"))
      (save-buffer))
    (with-current-buffer buffer
      (emacs-lisp-mode)
      (insert "(provide 'rod-" name ")")
      (goto-char (point-min))
      (open-line 2)
      (write-region nil nil filename)
      (set-buffer-modified-p nil)
      (set-visited-file-name (expand-file-name filename))
      (switch-to-buffer buffer))))

(use-package files
  :straight nil
  :custom
  (confirm-kill-emacs #'y-or-n-p)
  :config
  (defalias 'yes-or-no-p #'y-or-n-p))

(use-package restart-emacs
  :commands restart-emacs)

(defun rod/edit-emacs ()
  "Open init.el."
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

(defun rod/reload-dir-locals ()
  "Reload dir-locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun rod/reload-all-dir-locals-same-directory ()
  "For every buffer with the same `default-directory' as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (rod/reload-dir-locals))))))

(provide 'rod-qol)
