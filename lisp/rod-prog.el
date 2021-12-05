(use-package editorconfig
  :hook
  (prog-mode . editorconfig-mode)
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode))

(use-package dumb-jump
  :defer 1
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package quickrun
  :custom
  (quickrun-focus-p nil)
  :bind
  ("C-c ," . quickrun))

(add-hook 'after-init-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            (setq-default show-trailing-whitespace t)

            (save-place-mode 1)
            (show-paren-mode 1)
            (electric-pair-mode 1)
            (delete-selection-mode 1)

            ;; better default functions
            (global-set-key (kbd "M-/") #'hippie-expand)
            (global-set-key (kbd "C-x C-b") #'ibuffer)
            (global-set-key (kbd "C-s") 'isearch-forward-regexp)
            (global-set-key (kbd "C-r") 'isearch-backward-regexp)
            (global-set-key (kbd "C-M-s") 'isearch-forward)
            (global-set-key (kbd "C-M-r") 'isearch-backward)))

(provide 'rod-prog)
