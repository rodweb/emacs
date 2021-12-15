(use-package terraform-mode :defer t)

(use-package company-terraform
  :defer 1
  :init (company-terraform-init))

(provide 'rod-terraform)
