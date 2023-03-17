(use-package tempel
  :custom
  (tempel-trigger-prefix ",")
  :bind
  (:map tempel-map
        ("C-<tab>" . tempel-next))
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-abbrev-mode)
  (add-hook 'org-mode-hook 'tempel-setup-capf)
  :config
  (global-tempel-abbrev-mode))

(provide 'rod-tempel)
