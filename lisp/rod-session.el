(use-package desktop
  :custom
  (desktop-dirname (expand-file-name user-emacs-directory))
  (desktop-restore-eager 2)
  :config
  (desktop-save-mode 1))

(use-package winner
  :defer 1
  :config (winner-mode 1))

(provide 'rod-session)
