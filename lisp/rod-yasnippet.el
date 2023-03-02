(use-package yasnippet
  :hook ((org-mode prog-mode) . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("C-<tab>" . yas-next-field))
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'rod-yasnippet)
