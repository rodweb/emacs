(use-package json-mode :defer t)
(use-package yaml-mode :defer t)
(use-package jq-mode :defer t)

(use-package npm-mode
  :straight (npm-mode :host github :repo "mojochao/npm-mode" :fork t)
  :hook ((typescript-mode js-mode) . npm-mode))

(defun rod/setup-js ()
  (add-to-list 'compilation-error-regexp-alist-alist
               '(javascript-stack-trace
                 "(\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\))"
                 1 2 3 1))

  (add-to-list 'compilation-error-regexp-alist 'javascript-stack-trace)

  (add-to-list 'compilation-transform-file-match-alist
               '("internal/.*\\.js" . nil)))
(add-hook 'js-mode #'rod/setup-js)

(provide 'rod-js)
