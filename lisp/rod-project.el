(use-package projectile
  :defer 1
  :custom
  (projectile-generic-command "rg --files --hidden")
  (projectile-project-search-path '("~/dev" "~/dev/gupy"))
  :config
  (projectile-mode))

(provide 'rod-project)
