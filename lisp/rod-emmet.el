(use-package emmet-mode
  :hook
  ((sgml-mode css-mode tsx-ts-mode) . (emmet-mode))
  :config
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode))

(provide 'rod-emmet)
