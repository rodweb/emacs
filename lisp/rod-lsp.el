(use-package eglot
  :hook
  (typescript-mode . eglot-ensure)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c l h" . eldoc)
        ("C-c l o" . eglot-code-action-organize-imports)
        ("C-c l f" . eglot-format-buffer)
        ("C-c l q" . eglot-code-action-quickfix)
        ("C-c l a" . eglot-code-actions)
        ("C-c l s" . consult-eglot-symbols)))

(use-package consult-eglot :after eglot)

(provide 'rod-lsp)
