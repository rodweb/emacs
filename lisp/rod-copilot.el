(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :custom (copilot-node-executable "~/.nvm/versions/node/v16.19.0/bin/node")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("C-<return>" . copilot-accept-completion)))

(provide 'rod-copilot)
