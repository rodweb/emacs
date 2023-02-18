(use-package org
  :defer t
  :hook (org-capture-mode . evil-insert-state)
  :init
  (setq org-directory "~/org")
  :bind (("C-M-<return>" . org-meta-return))
  :custom
  (org-imenu-depth 4)
  (org-agenda-files '("~/org/inbox.org" "~/org/work.org" "~/org/personal.gpg" "~/org/habits.gpg"))
  (org-archive-location "~/org/archive.org::datetree/* Finished Tasks")
  (org-crypt-key "rod.apd@gmail.com")
  (org-capture-templates '(("i" "Inbox" entry (file+headline "inbox.org" "Inbox") "** TODO %?")))
  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)
                              (sql . t)
                              (js . t)
                              (calc .t)
                              (shell . t))))

(use-package org-journal
  :defer t
  :custom
  (org-journal-dir "~/org/journal")
  (org-journal-encrypt-journal t)
  (org-journal-file-type 'monthly)
  (org-journal-date-format "%F"))

(use-package org-roam
  :defer t
  :init (setq org-roam-v2-ack t)
  :hook ((org-mode . auto-fill-mode)
         (org-capture-mode . auto-fill-mode))
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-capture-templates
   '(("d" "default" plain "\n\n* ${title}\n\n%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

(use-package org-fc :after org)

(use-package ob-async :after org)

(defun rod/find-org-file ()
  "Find .org or .gpg files recursively from `org-directory'."
  (interactive)
  (let ((files (directory-files-recursively org-directory "\\.\\(org\\|gpg\\)$" nil)))
    (find-file (completing-read "Find file: " files))))

(provide 'rod-org)
