(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(customize-set-variable 'initial-buffer-choice (expand-file-name (concat user-emacs-directory "init.el")))

(require 'rod-ui)
