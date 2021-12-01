;; disable some ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; set a nice monospace font
(set-frame-font "JetBrains Mono-13" nil t)

;; set a nice dark theme
(load-theme 'modus-vivendi)

(provide 'rod-ui)
