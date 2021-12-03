(use-package rg
  :defer 1
  :config
  (rg-enable-default-bindings))

(rg-define-search rg-no-tests
  :format regexp
  :dir project
  :files "all"
  :flags ("--type-add 'notest:*.test.js'"
	  "--type-add 'notest:*.test.ts'"
	  "--type-not notest")
  :menu ("Custom" "n" "No tests"))

(provide 'rod-search)
