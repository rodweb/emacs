(use-package projectile
  :defer 1
  :custom
  (projectile-generic-command "rg --files --hidden")
  (projectile-project-search-path '("~/dev" "~/Projects"))
  (projectile-create-missing-test-files t)
  :config
  (projectile-mode))

(provide 'rod-project)
