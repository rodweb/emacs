(use-package verb
  :defer t
  :hook (org-mode . rod/setup-verb-bindings))

(defun rod/setup-verb-bindings ()
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(provide 'rod-http)
