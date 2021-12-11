(use-package json-mode :defer t)
(use-package yaml-mode :defer t)
(use-package jq-mode :defer t)

(use-package npm-mode
  :straight (npm-mode :host github :repo "mojochao/npm-mode" :fork t)
  :hook ((typescript-mode js-mode) . npm-mode))

(provide 'rod-js)
