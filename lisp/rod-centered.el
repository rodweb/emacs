(advice-add 'evil-next-line :after #'evil-scroll-line-to-center)
(advice-add 'evil-previous-line :after #'evil-scroll-line-to-center)

(use-package centered-cursor-mode :defer t)

(provide 'rod-centered)
