(use-package terraform-mode :defer t)

(use-package company-terraform
  :defer 1
  :init (company-terraform-init))

(add-hook 'terraform-mode-hook (lambda ()
                                 (setq-local completion-at-point-functions
                                             (mapcar #'cape-company-to-capf
                                                     (list #'company-terraform)))))

(provide 'rod-terraform)
