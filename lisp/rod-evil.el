(setq evil-want-keybinding nil)

(use-package evil
  :defer 1
  :custom
  (evil-want-C-i-jump t)
  (evil-want-Y-yank-to-eol t)
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-redo)
  (evil-symbol-word-search t)
  (evil-default-state 'emacs)
  :config
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'text-mode 'normal)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-mode))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(provide 'rod-evil)
