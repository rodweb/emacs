(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :hook ((dired-mode . dired-hide-details-mode)
	 (dired-mode . dired-omit-mode)))

(provide 'rod-dired)
