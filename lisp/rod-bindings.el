(use-package general
  :config
  (general-evil-setup)
  (general-override-mode))

(general-create-definer leader-def
  :prefix "SPC"
  :states '(normal visual emacs))

(defun rod/alternate-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun rod/setup-bindings ()
  "Setup custom bindings."
  (leader-def
    "SPC" 'execute-extended-command
    "TAB" 'rod/alternate-buffer
    "a" 'projectile-toggle-between-implementation-and-test
    "b" 'consult-buffer
    "c" (general-simulate-key "C-c")
    "d" 'kill-buffer-and-window
    "e" nil
    "eb" 'eval-buffer
    "ef" 'eval-defun
    "f" 'projectile-find-file
    "g" 'magit-status
    "h" nil
    "i" 'consult-imenu
    "j" nil
    "k" 'kill-this-buffer
    "l" 'consult-line
    "m" 'consult-bookmark
    "n" nil
    "o" 'delete-other-windows
    "p" 'projectile-command-map
    "q" nil
    "r" 'eglot-rename
    "s" 'save-buffer
    "t" (general-simulate-key "C-x t")
    "u" nil
    "v" nil
    "w" 'other-window
    "x" nil
    "y" nil
    "z" nil
    ";" 'eval-expression)

  (general-nmap
    ;; goto
    "gb" 'xref-pop-marker-stack
    "gr" 'xref-find-references
    "]e" 'next-error
    "[e" 'previous-error
    "]h" 'diff-hl-show-hunk-next
    "[h" 'diff-hl-show-hunk-previous))
(add-hook 'after-init-hook #'rod/setup-bindings)

(provide 'rod-bindings)
