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
  (evil-set-initial-state 'vterm-mode 'insert)
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'text-mode 'normal)
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'ert-results-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-mode))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-textobj-tree-sitter
  :after (evil tree-sitter)
  :config
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer")))

(use-package goto-chg
  :after evil)

(provide 'rod-evil)
