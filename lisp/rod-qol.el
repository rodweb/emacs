(defun rod/make-module (name)
  "Create a new module at `user-emacs-directory' lisp directory.
   NAME is the module suffix."
  (interactive "sName: ")
  (let* ((filename (concat user-emacs-directory "lisp/" "rod-" name ".el"))
	 (buffer (get-buffer-create filename)))
    (with-current-buffer buffer
      (emacs-lisp-mode)
      (insert "(provide 'rod-" name ")")
      (goto-char (point-min))
      (open-line 2)
      (switch-to-buffer buffer))))

(use-package files
  :straight nil
  :custom
  (confirm-kill-emacs #'y-or-n-p)
  :config
  (defalias 'yes-or-no-p #'y-or-n-p))

(provide 'rod-qol)