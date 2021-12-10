(use-package abbrev
  :straight nil
  :custom (abbrev-file-name (expand-file-name (concat user-emacs-directory "abbrev_defs")))
  :hook (prog-mode . abbrev-mode))

(provide 'rod-abbrev)
