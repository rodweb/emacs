(use-package eglot
  :hook
  ((typescript-mode tsx-ts-mode go-mode go-ts-mode) . eglot-ensure)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  :config
  (add-to-list 'eglot-server-programs `(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs `(go-ts-mode . ("gopls"))))

(defun rod/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-super-capf
                     #'eglot-completion-at-point
                     #'cape-file))))

(add-hook 'eglot-managed-mode-hook #'rod/eglot-capf)

(defun rod/eglot-bindings ()
  (general-nmap
    "g." 'eglot-code-action-quickfix
    "ga" 'eglot-code-actions
    "go" 'eglot-code-action-organize-imports
    "gs" 'consult-eglot-symbols))

(add-hook 'eglot-managed-mode-hook #'rod/eglot-bindings)

(use-package consult-eglot :after eglot)

(provide 'rod-eglot)
