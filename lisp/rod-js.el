(use-package json-mode :defer t)
(use-package yaml-mode :defer t)
(use-package jq-mode :defer t)

(use-package npm-mode
  :straight (npm-mode :host github :repo "mojochao/npm-mode" :fork t)
  :hook ((typescript-mode js-mode) . npm-mode))

(use-package nvm
  :hook ((typescript-mode js-mode) . nvm-use-for-buffer))

(use-package prettier-js
  :hook ((typescript-mode js-mode) . enable-prettier))

(defun enable-prettier ()
  (if-let ((dir (locate-dominating-file default-directory ".prettierrc.js")))
      (progn (prettier-js-mode +1)
             (setq-local prettier-js-command (concat dir "node_modules/.bin/prettier")))))

(use-package indium :defer t)

(add-to-list 'compilation-error-regexp-alist-alist
             '(javascript-stack-trace
               "(\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\))"
               1 2 3 1))

(add-to-list 'compilation-error-regexp-alist 'javascript-stack-trace)

(add-to-list 'compilation-transform-file-match-alist
             '("internal/.*\\.js" . nil))

(defun rod/setup-js ()
  (setq-local tab-width 2
              javascript-indent-level 2
              typescript-indent-level 2))

(add-hook 'js-mode-hook #'rod/setup-js)
(add-hook 'typescript-mode-hook #'rod/setup-js)

(provide 'rod-js)
