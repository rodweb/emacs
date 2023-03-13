(use-package general
  :config
  (general-evil-setup)
  (general-override-mode))

(use-package which-key
  :after general
  :config
  (which-key-mode))

(use-package hydra :defer t)

(use-package keyfreq
  :defer 1
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands '(self-insert-command
                                    org-self-insert-command
                                    vterm--self-insert
                                    mwheel-scroll
                                    lsp-ui-doc--handle-mouse-movement
                                    ignore)))

(general-create-definer leader-def
  :prefix "SPC"
  :states '(normal visual emacs)
  :keymaps 'override)

(defun rod/alternate-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun rod/indent-buffer ()
  "Indent whole buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun rod/format-buffer ()
  "Format whole buffer."
  (interactive)
  (cond ((string-equal major-mode "json-mode") (json-mode-beautify (point-min) (point-max)))
        ((string-equal major-mode "sql-mode") (sqlformat-buffer))
        ((bound-and-true-p eglot--managed-mode) (eglot-format-buffer))
        ((bound-and-true-p lsp-mode) (lsp-format-buffer))
        (t (rod/indent-buffer))))

(defun rod/copy-filename ()
  "Copy `buffer-file-name'."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (message (kill-new filename))
      (error "Buffer not visiting file."))
    filename))

(defun rod/copy-directory ()
  "Copy `buffer-file-name's directory."
  (interactive)
  (if-let ((filename (let ((inhibit-message t))
                       (rod/copy-filename))))
      (message (kill-new (url-file-directory filename)))))

(defun rod/rename ()
  "Rename symbol."
  (interactive)
  (cond ((bound-and-true-p tide-mode) (tide-rename-symbol))
        ((bound-and-true-p eglot--managed-mode) (eglot-rename))
        ((bound-and-true-p lsp-mode) (call-interactively #'lsp-rename))
        (t (call-interactively #'rod/rename-symbol))))

(defun rod/rename-symbol (new)
  "Rename symbol at point."
  (interactive (list
                (read-string (format "Rename %s to: " (thing-at-point 'symbol)))))
  (let ((old (thing-at-point 'symbol)))
    (mark-defun)
    (replace-string old new)))

(defun rod/setup-bindings ()
  "Setup custom bindings."
  (leader-def
    "SPC" 'execute-extended-command
    "TAB" 'rod/alternate-buffer
    "=" 'rod/format-buffer
    ";" 'eval-expression
    "/" 'rg-menu
    "a" 'projectile-toggle-between-implementation-and-test
    "b" 'consult-buffer
    "B" 'magit-blame
    "c" (general-simulate-key "C-c")
    "d" 'kill-buffer-and-window
    "e" nil
    "eb" 'eval-buffer
    "ee" 'rod/edit-emacs
    "ef" 'eval-defun
    "er" 'eval-region
    "es" 'eshell
    "et" 'vterm
    "f" 'projectile-find-file
    "g" 'magit-status
    "h" 'consult-recent-file
    "i" 'consult-imenu
    "j" 'avy-goto-char-timer
    "k" 'kill-this-buffer
    "l" 'consult-line
    "L" 'magit-log-buffer-file
    "m" nil
    "ma" 'bookmark-set
    "mm" 'consult-bookmark
    "mj" 'bookmark-jump
    "md" 'bookmark-delete
    "n" nil
    "na" 'org-agenda
    "nc" 'org-capture
    "nf" 'org-roam-node-find
    "ni" #'(lambda () (interactive) (org-capture t "i"))
    "nj" 'org-journal-new-entry
    "nl" 'org-roam-buffer-toggle
    "nn" 'rod/find-org-file
    "o" 'delete-other-windows
    "p" 'projectile-command-map
    "q" 'consult-ripgrep
    "r" #'rod/rename
    "s" 'save-buffer
    "t" 'rgr-command-map
    "u" (general-simulate-key "C-x t")
    "v" (general-simulate-key "C-x 4")
    "w" 'ace-window
    "x" nil
    "y" nil
    "yg" 'git-link
    "yf" 'rod/copy-filename
    "yd" 'rod/copy-directory
    "z" nil
    "zp" 'prodigy
    "zd" 'docker
    "zk" 'kubernetes-overview)

  (general-nmap
    ;; goto
    "ga" 'lsp-execute-code-action
    "gb" 'xref-pop-marker-stack
    "ghs" 'git-gutter:stage-hunk
    "ghr" 'git-gutter:revert-hunk
    "ghp" 'git-gutter:popup-hunk
    "gl" 'ace-link
    "go" 'lsp-organize-imports
    "gr" 'xref-find-references
    "gI" 'lsp-find-implementation
    "]e" 'flymake-goto-next-error
    "[e" 'flymake-goto-prev-error
    "]l" 'next-error
    "[l" 'previous-error
    "]g" 'git-gutter:next-hunk
    "[g" 'git-gutter:previous-hunk))

(add-hook 'after-init-hook #'rod/setup-bindings)

(provide 'rod-bindings)
