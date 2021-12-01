(use-package desktop
  :custom
  (desktop-dirname (expand-file-name user-emacs-directory))
  :config
  (desktop-save-mode 1))

(provide 'rod-session)
