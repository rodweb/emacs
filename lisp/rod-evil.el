(setq evil-want-keybinding nil)

(use-package evil
  :defer 1
  :custom
  (evil-want-C-i-jump t)
  (evil-want-Y-yank-to-eol t)
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-redo)
  (evil-symbol-word-search t)
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (add-hook 'dired-mode-hook 'evil-emacs-state)
  (add-hook 'help-mode-hook 'evil-emacs-state)
  (evil-mode))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(provide 'rod-evil)
