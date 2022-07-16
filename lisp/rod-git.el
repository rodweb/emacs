(use-package magit
  :custom
  (vc-follow-symlinks t)
  :commands (magit-status))

(use-package forge
  :after magit)

(use-package code-review
  :bind
  (:map forge-topic-mode-map
        ("C-c r" . code-review-forge-pr-at-point)))

(use-package diff-hl
  :defer 1
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1))

(use-package git-gutter
  :defer 1
  :custom
  (git-gutter:ask-p nil)
  (git-gutter:display-p nil)
  :config
  (global-git-gutter-mode 1)
  (add-to-list 'git-gutter:update-commands #'other-window))

(use-package git-link
  :defer t
  :custom
  (git-link-use-commit t))

(provide 'rod-git)
