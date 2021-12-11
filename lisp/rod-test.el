(use-package rgr
  :straight (:host github :repo "rodweb/rgr.el")
  :hook ((typescript-mode javascript-mode) . rgr-mode))

(provide 'rod-test)
