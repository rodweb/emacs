(use-package org
  :defer t
  :hook (org-capture-mode . evil-insert-state)
  :init (setq org-directory "~/org")
  :custom
  (org-imenu-depth 4)
  (org-agenda-files '("~/org/inbox.org"))
  (org-crypt-key "rod.apd@gmail.com")
  (org-capture-templates '(("i" "Inbox" entry (file+headline "inbox.org" "Inbox") "** TODO %?")))
  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)
                              (sql . t)
                              (js . t)
                              (calc .t)
                              (shell . t)))
  :bind (("C-c n n" . rod/find-org-file)
         ("C-c n a" . org-agenda)
         ("C-c n c" . org-capture)))

(use-package org-roam
  :defer t
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory "~/org/roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

(defun rod/find-org-file ()
  "Find .org or .gpg files recursively from `org-directory'."
  (interactive)
  (let ((files (directory-files-recursively org-directory "\\.\\(org\\|gpg\\)$" nil)))
    (find-file (completing-read "Find file: " files))))

(provide 'rod-org)
