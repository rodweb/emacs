(use-package vertico
  :defer 0.1
  :custom
  (vertico-cycle t)
  (enable-recursive-minibuffers t)
  :bind
  (:map minibuffer-local-map
	("<escape>" . abort-minibuffers))
  :config
  (minibuffer-depth-indicate-mode)
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

(use-package embark
  :after vertico
  :bind
  (("M-<return>" . embark-act)
   ("C-h b" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :defer t
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap goto-line] . consult-goto-line)
   ("M-g l" . consult-line))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-buffer :preview-key nil
   consult-recent-file :preview-key nil))

(use-package embark-consult
  :after consult)

(use-package savehist
  :straight nil
  :after vertico
  :config
  (savehist-mode))

(use-package recentf
  :straight nil
  :after vertico
  :custom
  (recentf-max-saved-items 50)
  :config
  ;; saves every 5 minutes
  (run-at-time nil (* 5 60) #'recentf-save-list)
  (recentf-mode))

(provide 'rod-minibuffer)
