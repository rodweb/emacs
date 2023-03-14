(defvar rod/nvm-dir "~/.nvm")

(use-package eshell
  :straight nil
  :hook (eshell-mode . rod/eshell-setup-aliases))

(defun rod/eshell-setup-aliases ()
  (eshell/alias "node" (concat rod/nvm-dir "/nvm-exec node")))

(provide 'rod-eshell)
