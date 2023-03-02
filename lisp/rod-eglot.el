(use-package eglot
  :hook
  ((typescript-mode tsx-ts-mode) . eglot-ensure)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (add-to-list 'eglot-server-programs `(tsx-ts-mode . ("typescript-language-server" "--stdio"))))

(general-nmap
  "g." 'eglot-code-action-quickfix
  "ga" 'eglot-code-actions
  "go" 'eglot-code-action-organize-imports
  "gs" 'consult-eglot-symbols)

(use-package consult-eglot :after eglot)

(provide 'rod-eglot)
