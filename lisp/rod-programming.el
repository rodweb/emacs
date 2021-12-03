(use-package dumb-jump
  :defer 1
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'rod-programming)
