(use-package corfu
  :custom
  (completion-cycle-threshold 3)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  (corfu-auto t)
  (completion-styles '(basic))
  :init
  (global-corfu-mode))

(provide 'rod-corfu)
