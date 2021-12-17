(setq evil-want-keybinding nil)

(use-package evil
  :defer 1
  :general
  (general-nmap
    :keymaps 'org-mode-map
    "TAB" #'org-cycle)
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
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'ert-results-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-mode))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package goto-chg
  :after evil)

(provide 'rod-evil)
