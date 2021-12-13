(use-package org
  :defer t
  :custom
  (org-imenu-depth 4)
  (org-directory "~/org")
  (org-agenda-files '("~/org/inbox.org"))
  (org-crypt-key "rod.apd@gmail.com"))

(use-package org-roam
  :defer t
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory "~/org/roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

(provide 'rod-org)
