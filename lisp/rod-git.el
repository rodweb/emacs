(use-package magit
  :custom
  (vc-follow-symlinks t)
  :commands (magit-status)
  :hook (magit-blame-mode . evil-emacs-state)
  :config
  (setq magit-process-finish-apply-ansi-colors t)
  (setq magit-clone-default-directory "~/dev") ;; TODO: use variable
  (setq magit-clone-set-remote.pushDefault t))

(defun rod/magit-blame-quit ()
  (when (not (bound-and-true-p magit-blame-mode))
    (evil-normal-state)))

(advice-add 'magit-blame-quit :after #'rod/magit-blame-quit)

(use-package forge
  :after magit)

(use-package code-review
  :bind
  (:map forge-topic-mode-map
        ("C-c r" . code-review-forge-pr-at-point))
  :custom (code-review-auth-login-marker 'forge))

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

(use-package clone
  :straight (clone :host github :repo "rodweb/clone.el" :branch "main")
  :commands (clone-repo)
  :custom
  (clone-protocol 'ssh)
  (clone-directory "/Users/rod/dev"))

(provide 'rod-git)
