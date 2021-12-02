(use-package vertico
  :defer 0.1
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode))

(defun rod/use-orderless-in-minibuffer ()
  (setq-local completion-styles '(orderless)))

(use-package orderless
  :after vertico
  :custom
  (orderless-matching-styles '(orderless-literal
			       orderless-regexp
			       orderless-initialism))
  :hook
  (minibuffer-setup . rod/use-orderless-in-minibuffer))

(use-package marginalia
  :after vertico
  :bind
  (:map minibuffer-local-map
	("M-S-<return>" . marginalia-cycle))
  :config
  (marginalia-mode))

(provide 'rod-minibuffer)
