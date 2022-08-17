;; disable some ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; disable gui dialogs
(setq use-dialog-box nil
      use-file-dialog nil)

;; disable bell
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable title
(setq frame-resize-pixelwise t)

;; set a nice monospace font
(set-frame-font "JetBrains Mono-13" nil t)

;; set a nice dark theme
(load-theme 'modus-vivendi)

(use-package default-text-scale
  :defer 15
  :config (default-text-scale-mode 1))

(provide 'rod-ui)
