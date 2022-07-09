(use-package general
  :config
  (general-evil-setup)
  (general-override-mode))

(use-package which-key
  :after general
  :config
  (which-key-mode))

(use-package keyfreq
  :defer 1
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(general-create-definer leader-def
  :prefix "SPC"
  :states '(normal visual emacs)
  :keymaps 'override)

(defun rod/alternate-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun rod/format-elisp-buffer ()
  "Format an emacs-lisp-mode buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun rod/format-buffer ()
  "Format whole buffer."
  (interactive)
  (cond ((string-equal major-mode "json-mode") (json-mode-beautify (point-min) (point-max)))
        ((string-equal major-mode "sql-mode") (sqlformat-buffer))
        ((string-equal major-mode "emacs-lisp-mode") (rod/format-elisp-buffer))
        (t (lsp-format-buffer))))

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
    "r" 'lsp-rename
    "s" 'save-buffer
    "t" 'rgr-command-map
    "u" (general-simulate-key "C-x t")
    "v" (general-simulate-key "C-x 4")
    "w" 'other-window
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
    "gl" 'ace-link
    "go" 'lsp-organize-imports
    "gr" 'xref-find-references
    "gI" 'lsp-find-implementation
    "]e" 'flymake-goto-next-error
    "[e" 'flymake-goto-prev-error
    "]l" 'next-error
    "[l" 'previous-error
    "]h" 'diff-hl-show-hunk-next
    "[h" 'diff-hl-show-hunk-previous))

(add-hook 'after-init-hook #'rod/setup-bindings)

(provide 'rod-bindings)
