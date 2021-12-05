(use-package magit
  :custom
  (vc-follow-symlinks t)
  :commands (magit-status))

(use-package forge
  :after magit)

(use-package diff-hl
  :defer 1
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1))

(provide 'rod-git)
