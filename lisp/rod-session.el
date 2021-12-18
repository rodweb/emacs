(use-package desktop
  :custom
  (desktop-dirname (expand-file-name user-emacs-directory))
  (desktop-restore-eager 2)
  :config
  (desktop-save-mode 1))

(provide 'rod-session)
