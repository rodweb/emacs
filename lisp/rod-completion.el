(use-package company
  :defer 1
  :custom
  (tab-always-indent 'complete)
  (company-dabbrev-downcase nil)
  (company-tooltip-align-annotations t)
  :bind
  (:map company-mode-map
        ([remap completion-at-point] . #'company-complete)
        ([remap indent-for-tab-command] . #'company-indent-or-complete-common))
  :config
  (global-company-mode))

(provide 'rod-completion)
