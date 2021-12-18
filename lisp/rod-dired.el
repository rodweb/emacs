(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :init (put 'dired-find-alternate-file 'disabled nil)
  :bind
  (:map dired-mode-map
        ("-" . #'dired-up-directory)
        ("C-c C-q" . #'wdired-change-to-wdired-mode)
        ("<return>" . #'dired-find-alternate-file))
  :general
  (general-nvmap
    "-" #'dired-jump)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode)
         (dired-mode . diff-hl-dired-mode)))

(use-package dired-hide-dotfiles
  :after dired
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :config
  (dired-hide-dotfiles-mode))

(provide 'rod-dired)
