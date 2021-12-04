(use-package gcmh
  :config (gcmh-mode))

;; disable bidirectional display
(setq-default bidi-paragraph-direction 'left-to-right
	      bidi-inhibit-bpa t)

(provide 'rod-performance)
