(use-package ws-butler
  :defer t)

(use-package editorconfig
  :hook
  (prog-mode . editorconfig-mode)
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode))

(use-package tree-sitter
  :defer 1
  :config (setq tree-sitter-debug-jump-buttons t
                tree-sitter-debug-highlight-jump-region t)
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package dumb-jump
  :defer 1
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (dumb-jump-mode 1))

(use-package quickrun
  :custom
  (quickrun-focus-p nil)
  :bind
  ("C-c ," . quickrun))

(use-package avy
  :commands
  (avy-goto-char-timer)
  :custom
  (avy-background t)
  (avy-timeout-seconds 0.3))

(use-package ace-link :defer t)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(defun rod/show-trailing-whitespace-in-prog-mode ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'rod/show-trailing-whitespace-in-prog-mode)

(defun rod/setup-programming-defaults ()
  (setq-default indent-tabs-mode nil)

  (save-place-mode 1)
  (show-paren-mode 1)
  (column-number-mode 1)
  (electric-pair-mode 1)
  (global-hl-line-mode 1)
  (delete-selection-mode 1)

  ;; better default functions
  (global-set-key [remap isearch-forward] #'isearch-forward-regexp)
  (global-set-key [remap isearch-backward] #'isearch-backward-regexp)
  (global-set-key [remap list-buffers] #'ibuffer)
  (global-set-key [remap dabbrev-expand] #'hippie-expand))
(add-hook 'after-init-hook #'rod/setup-programming-defaults)

(provide 'rod-prog)
