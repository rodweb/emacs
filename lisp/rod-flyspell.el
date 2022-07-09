(use-package flyspell
  :straight nil
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(provide 'rod-flyspell)
