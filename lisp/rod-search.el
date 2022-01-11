(use-package rg
  :defer 1
  :config
  (rg-enable-default-bindings)
  (add-to-list 'rg-required-command-line-flags "--max-columns=2000"))

(rg-define-search rg-no-tests
  :format regexp
  :dir project
  :files "all"
  :flags ("--type-add 'notest:*.test.*'"
	  "--type-not notest")
  :menu ("Custom" "n" "No tests"))

(use-package comb
  :hook ((comb-configure-mode comb-buffer-setup) . turn-off-evil-mode)
  :bind
  (:map comb-keymap
        ("r" . #'comb-reject-next)
        ("a" . #'comb-approve-next))
  :defer t)

(provide 'rod-search)
