(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(customize-set-variable 'initial-buffer-choice (expand-file-name (concat user-emacs-directory "init.el")))

(require 'rod-ui)
(require 'rod-bootstrap)
(require 'rod-env)
(require 'rod-session)
(require 'rod-git)
(require 'rod-qol)
(require 'rod-backup)
(require 'rod-dired)
(require 'rod-minibuffer)
