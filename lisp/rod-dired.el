(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind
  (:map dired-mode-map
	("C-c C-q" . #'wdired-change-to-wdired-mode))
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
