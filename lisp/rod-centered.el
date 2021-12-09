(advice-add 'evil-next-line :after #'evil-scroll-line-to-center)
(advice-add 'evil-previous-line :after #'evil-scroll-line-to-center)

(provide 'rod-centered)
