(use-package org
  :defer t
  :hook (org-capture-mode . evil-insert-state)
  :custom
  (org-imenu-depth 4)
  (org-directory "~/org")
  (org-agenda-files '("~/org/inbox.org"))
  (org-crypt-key "rod.apd@gmail.com")
  (org-capture-templates '(("i" "Inbox" entry (file+headline "inbox.org" "Inbox") "** TODO %?")))
  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)
                              (sql . t)
                              (shell . t)
                              (verb . t)))
  :bind (("C-c n a" . org-agenda)
         ("C-c n c" . org-capture)))

(use-package org-roam
  :defer t
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory "~/org/roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

(provide 'rod-org)
