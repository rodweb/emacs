(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("C-<return>" . copilot-accept-completion)))

(provide 'rod-copilot)
