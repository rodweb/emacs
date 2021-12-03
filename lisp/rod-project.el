(use-package projectile
  :defer 1
  :custom
  (projectile-generic-command "rg --files --hidden")
  (projectile-project-search-path '("~/dev" "~/dev/work"))
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :config
  (projectile-mode))

(provide 'rod-project)
