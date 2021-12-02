(use-package files
  :straight nil
  :init
  (make-directory "~/tmp/emacs/backups" t)
  :custom
  (backup-by-copying t)
  (backup-directory-alist
   '(("." . "~/tmp/emacs/backups"))))

(provide 'rod-backup)
