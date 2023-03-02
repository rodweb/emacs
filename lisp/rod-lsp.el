(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred lsp-org)
  :hook
  (typescript-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (js-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-eslint-enable nil)
  :config (setq read-process-output-max (* 1024 1024)
                lsp-headerline-breadcrumb-enable nil
                lsp-enable-symbol-highlighting nil
                lsp-restart 'ignore))

(use-package lsp-ui
  :after lsp-mode)

(use-package consult-lsp
  :defer t)

(use-package dap-mode
  :after (lsp-mode)
  :config
  (dap-mode 1)
  (require 'dap-node)
  (dap-node-setup))

(provide 'rod-lsp)
