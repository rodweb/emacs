(use-package general
  :config
  (general-evil-setup)
  (general-override-mode))

(general-create-definer leader-def
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :states '(normal visual emacs))

(defun rod/alternate-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun rod/format-buffer ()
  "Format whole buffer."
  (interactive)
  (cond ((string-equal major-mode "json-mode") (json-mode-beautify (point-min) (point-max)))
        ((string-equal major-mode "sql-mode") (sqlformat-buffer))
        (t (eglot-format-buffer))))

(defun rod/setup-bindings ()
  "Setup custom bindings."
  (leader-def
    "SPC" 'execute-extended-command
    "TAB" 'rod/alternate-buffer
    "=" 'rod/format-buffer
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
    "h" nil
    "i" 'consult-imenu
    "j" 'avy-goto-char-timer
    "k" 'kill-this-buffer
    "l" 'consult-line
    "m" 'consult-bookmark
    "n" (general-simulate-key "C-c n")
    "o" 'delete-other-windows
    "p" 'projectile-command-map
    "q" nil
    "r" 'eglot-rename
    "s" 'save-buffer
    "t" 'rgr-command-map
    "u" nil
    "v" (general-simulate-key "C-x 4")
    "w" 'other-window
    "x" nil
    "y" nil
    "z" nil
    ";" 'eval-expression)

  (general-nmap
    ;; goto
    "g." 'eglot-code-action-quickfix
    "ga" 'eglot-code-actions
    "gb" 'xref-pop-marker-stack
    "go" 'eglot-code-action-organize-imports
    "gr" 'xref-find-references
    "gI" 'eglot-find-implementation
    "g]" 'flymake-goto-next-error
    "g[" 'flymake-goto-prev-error
    "]e" 'next-error
    "[e" 'previous-error
    "]h" 'diff-hl-show-hunk-next
    "[h" 'diff-hl-show-hunk-previous))
(add-hook 'after-init-hook #'rod/setup-bindings)

(provide 'rod-bindings)
