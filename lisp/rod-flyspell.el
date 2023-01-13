(use-package flyspell
  :straight nil
  :custom
  (ispell-program-name "aspell")
  (ispell-list-command "--list")
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(provide 'rod-flyspell)
