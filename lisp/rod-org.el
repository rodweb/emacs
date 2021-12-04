(use-package org
  :defer t
  :custom
  (org-imenu-depth 4)
  (org-directory "~/org")
  (org-agenda-files '("~/org/inbox.org"))
  (org-crypt-key "rod.apd@gmail.com"))

(provide 'rod-org)
