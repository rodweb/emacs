(setq straight-use-package-by-default t)
(setq straight-default-vc 'git)
(setq straight-vc-git-default-protocol 'ssh)
(setq straight-host-usernames '((github . "rodweb")
                                (gitlab . "rodweb")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-enable-imenu-support t)
(straight-use-package 'use-package)

(use-package default-text-scale
  :defer 15
  :custom (default-text-scale-amount 20)
  :config (default-text-scale-mode 1))

(provide 'rod-bootstrap)
