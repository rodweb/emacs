(use-package gcmh
  :custom (gcmh-verbose t)
  :config (gcmh-mode))

(use-package fnhh
  :straight (:host github :repo "a13/fnhh")
  :config (fnhh-mode))

;; disable bidirectional display
(setq-default bidi-paragraph-direction 'left-to-right
	      bidi-inhibit-bpa t)

(use-package so-long
  :straight nil
  :defer 1
  :config (global-so-long-mode))

(use-package too-long-lines-mode
  :straight (too-long-lines-mode :host github :repo "rakete/too-long-lines-mode" :fork t)
  :defer 1
  :config (too-long-lines-mode))

(provide 'rod-perf)
