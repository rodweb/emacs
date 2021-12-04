(use-package dumb-jump
  :defer 1
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(save-place-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)

(provide 'rod-programming)
