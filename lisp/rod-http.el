(use-package verb
  :hook (org-mode . rod/setup-verb-mode))

(use-package verb-env
  :straight (verb-env :host github :repo "rodweb/verb-env" :branch "main")
  :hook (verb-mode . verb-env-mode))

(use-package restclient :defer t)

(defun rod/setup-verb-mode ()
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((verb . t))))
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(provide 'rod-http)
