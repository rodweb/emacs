;; (use-package eglot
;;   :hook
;;   (typescript-mode . eglot-ensure)
;;   :custom
;;   (eglot-confirm-server-initiated-edits nil)
;;   :bind
;;   (:map eglot-mode-map
;;         ("C-c l r" . eglot-rename)
;;         ("C-c l h" . eldoc)
;;         ("C-c l o" . eglot-code-action-organize-imports)
;;         ("C-c l f" . eglot-format-buffer)
;;         ("C-c l q" . eglot-code-action-quickfix)
;;         ("C-c l a" . eglot-code-actions)
;;         ("C-c l s" . consult-eglot-symbols)))

;; (use-package consult-eglot :after eglot)

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred lsp-org)
  :hook
  (typescript-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-keymap-prefix "C-c l")
  :config (setq read-process-output-max (* 1024 1024)
                lsp-headerline-breadcrumb-enable nil
                lsp-enable-symbol-highlighting nil
                lsp-restart 'auto-restart))

(use-package lsp-ui
  :after lsp-mode)

(use-package consult-lsp
  :defer t
  :config (consult-lsp-marginalia-mode t))

(provide 'rod-lsp)
