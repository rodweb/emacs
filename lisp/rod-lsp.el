(use-package eglot
  :hook
  (typescript-mode . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c l h" . eldoc)
        ("C-c l o" . eglot-code-action-organize-imports)
        ("C-c l f" . eglot-format)
        ("C-c l a" . eglot-code-actions)))

(provide 'rod-lsp)
