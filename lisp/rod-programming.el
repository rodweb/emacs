(use-package dumb-jump
  :defer 1
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(save-place-mode 1)

(provide 'rod-programming)
