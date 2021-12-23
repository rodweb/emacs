(use-package verb
  :hook (org-mode . rod/setup-verb-mode))

(use-package verb-env
  :after verb
  :straight '(:host github :repo "rodweb/verb-env"))

(defun rod/setup-verb-mode ()
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((verb . t))))
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(provide 'rod-http)
