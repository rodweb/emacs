(use-package yasnippet
  :hook ((org-mode prog-mode) . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'rod-snippet)
