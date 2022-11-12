(use-package flyspell
  :straight nil
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(use-package auto-dicionary-mode
  :straight (auto-dicitonary-mode :host github :repo "nschum/auto-dictionary-mode" :branch "master")
  :hook ((flyspell-mode . auto-dictionary-mode)))

(provide 'rod-flyspell)
