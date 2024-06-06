(defconst rod/project-dir (expand-file-name "~/dev/"))
(defconst rod/tmp-dir (expand-file-name "~/tmp/"))
(defconst rod/backup-dir (concat rod/tmp-dir "emacs/backups/"))
(defconst rod/ts-modules-dir (concat rod/project-dir "tree-sitter-module/dist/"))

(defvar native-comp-deferred-compilation-deny-list nil)

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;;; user interface

;; disable some ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; disable GUI dialog
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;; disable bell
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable title
(setq frame-resize-pixelwise t)

;; set a nice monospace font
(set-frame-font "JetBrains Mono-13" nil t)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 135))

;; set a nice dark theme
(load-theme 'modus-vivendi)

;;; package manager

;; set some defaults for the package manager
(setq straight-use-package-by-default t)
(setq straight-default-vc 'git)
(setq straight-vc-git-default-protocol 'ssh)
(setq straight-host-usernames '((github . "rodweb")
                                (gitlab . "rodweb")))

;; standard straight.el bootstrap
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

;; install use-package before anything else
(setq use-package-enable-imenu-support t)
(straight-use-package 'use-package)

;; increase font-size for all buffers at once
(use-package default-text-scale
  :defer 15
  :custom (default-text-scale-amount 20)
  :config (default-text-scale-mode 1))

;;; performance

;; show how long it took to load Emacs
(add-hook 'after-init-hook #'(lambda () (message "Emacs took %s to load." (emacs-init-time))))

;; little hack to improve garbage collection
(use-package gcmh
  :straight (:host github :repo "emacsmirror/gcmh")
  :custom (gcmh-verbose t)
  :config (gcmh-mode))

;; little hack to speed up startup
(use-package fnhh
  :straight (:host github :repo "a13/fnhh")
  :config (fnhh-mode))

;; disable bidirectional display because it's known to be slow
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

;; performance mitigation for large files
(use-package so-long
  :straight nil
  :defer 1
  :config (global-so-long-mode))

;; long lines are known to be slow, so skip showing them
(use-package too-long-lines-mode
  :straight (too-long-lines-mode :host github :repo "rakete/too-long-lines-mode" :fork t)
  :defer 1
  :config (too-long-lines-mode))

;; load environment variables from shell
(use-package exec-path-from-shell
  :defer 0.1
  :config
  (add-to-list 'exec-path-from-shell-variables "AWS_SHARED_CREDENTIALS_FILE")
  (add-to-list 'exec-path-from-shell-variables "NPM_TOKEN")
  (add-to-list 'exec-path-from-shell-variables "LSP_USE_PLISTS")
  (exec-path-from-shell-initialize))

;;; bindings

;; general.el for keybindings
(use-package general
  :config
  (general-evil-setup)
  (general-override-mode))

;; which-key.el for keybinding hints
(use-package which-key
  :after general
  :config
  (which-key-mode))

;; TODO: Save data and deprecate?
;; record key frequency to optimize keybindings
(use-package keyfreq
  :defer 1
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands '(self-insert-command
                                    org-self-insert-command
                                    vterm--self-insert
                                    mwheel-scroll
                                    lsp-ui-doc--handle-mouse-movement
                                    ignore)))

;; define space as leader key
(general-create-definer leader-def
  :prefix "SPC"
  :states '(normal visual emacs)
  :keymaps 'override)

;; TODO: Move custom functions to their own file
;;; custom functions
(defun rod/alternate-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun rod/indent-buffer ()
  "Indent whole buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun rod/format-buffer ()
  "Format whole buffer."
  (interactive)
  (cond ((string-equal major-mode "json-mode") (json-mode-beautify (point-min) (point-max)))
        ((string-equal major-mode "sql-mode") (sqlformat-buffer))
        ((string-equal major-mode "terraform-mode") (terraform-format-buffer))
        ((bound-and-true-p eglot--managed-mode) (eglot-format-buffer))
        ((bound-and-true-p lsp-mode) (lsp-format-buffer))
        (t (rod/indent-buffer))))

(defun rod/kill-most-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (string= (buffer-name buffer) "*scratch*")
                (string= (buffer-name buffer) "*Messages*"))
      (kill-buffer buffer)))
  (delete-other-windows))

(defun rod/copy-filename ()
  "Copy `buffer-file-name'."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (message (kill-new filename))
      (error "Buffer not visiting file."))
    filename))

(defun rod/copy-directory ()
  "Copy `buffer-file-name's directory. If it's a dired buffer, copy `dired-current-directory's instead."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((path (dired-current-directory)))
        (when path
          (message "Copied path to clipboard: %s" path)
          (kill-new path)))
    (if-let ((filename (let ((inhibit-message t))
                         (rod/copy-filename))))
        (message (kill-new (url-file-directory filename))))))

(defun rod/rename ()
  "Rename symbol."
  (interactive)
  (cond ((bound-and-true-p tide-mode) (tide-rename-symbol))
        ((bound-and-true-p eglot--managed-mode) (call-interactively #'eglot-rename))
        ((bound-and-true-p lsp-mode) (call-interactively #'lsp-rename))
        (t (call-interactively #'rod/rename-symbol))))

(defun rod/rename-symbol (new)
  "Rename symbol at point."
  (interactive (list
                (read-string (format "Rename %s to: " (thing-at-point 'symbol)))))
  (let ((old (thing-at-point 'symbol)))
    (mark-defun)
    (replace-string old new)))

(defun rod/make-module (name)
  "Create a new module at `user-emacs-directory' lisp directory.
   NAME is the module suffix."
  (interactive "sName: ")
  (let* ((filename (concat user-emacs-directory "lisp/" "rod-" name ".el"))
         (buffer (get-buffer-create filename)))
    (with-current-buffer (or (get-buffer "init.el") (get-buffer "init.el<emacs>"))
      (goto-char (point-max))
      (insert (concat "(require 'rod-" name ")"))
      (save-buffer))
    (with-current-buffer buffer
      (emacs-lisp-mode)
      (insert "(provide 'rod-" name ")")
      (goto-char (point-min))
      (open-line 2)
      (write-region nil nil filename)
      (set-buffer-modified-p nil)
      (set-visited-file-name (expand-file-name filename))
      (switch-to-buffer buffer))))

(defun rod/edit-emacs ()
  "Open init.el."
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

(defun rod/reload-dir-locals ()
  "Reload dir-locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun rod/reload-all-dir-locals-same-directory ()
  "For every buffer with the same `default-directory' as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (rod/reload-dir-locals))))))

(defun rod/dired-copy-path ()
  "Copy the current file path in dired."
  (interactive)
  (kill-new (dired-get-filename)))

(defun rod/diff-buffers-create ()
  (interactive)
  (let* ((bufa (get-buffer-create "A"))
         (bufb (get-buffer-create "B")))
    (ediff-buffers bufa bufb)))

(defun rod/diff-buffers-kill ()
  (interactive)
  (kill-buffer "A")
  (kill-buffer "B")
  (when (bound-and-true-p winner-mode)
    (winner-undo)))

(defun rod/disable-mode (mode-fn)
  "Disable MODE-FN in all buffers."
  (interactive "a")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (funcall mode-fn -1))))

(defun rod/find-org-file ()
  "Find .org or .gpg files recursively from `org-directory'."
  (interactive)
  (let ((files (directory-files-recursively org-directory "\\.\\(org\\|gpg\\)$" nil)))
    (find-file (completing-read "Find file: " files))))

(defun rod/eval-buffer ()
  (interactive)
  (case major-mode
        (emacs-lisp-mode (eval-buffer))
        (clojure-mode (cider-eval-buffer))
        (t (error "Unsupported mode: %s" major-mode))))

(defun rod/goto-implementation ()
  (interactive)
  (cond ((bound-and-true-p eglot--managed-mode) (call-interactively #'eglot-find-implementation))
        ((bound-and-true-p lsp-mode) (call-interactively #'lsp-find-implementation))
        (t (xref-find-definitions (thing-at-point 'symbol)))))

;; define most of the keybindings
(defun rod/setup-bindings ()
  "Setup custom bindings."
  (leader-def
    "SPC" 'execute-extended-command
    "TAB" 'rod/alternate-buffer
    "=" 'rod/format-buffer
    ";" 'eval-expression
    "/" 'rg-menu
    "a" 'projectile-toggle-between-implementation-and-test
    "b" 'consult-buffer
    "B" 'magit-blame
    "c" (general-simulate-key "C-c")
    "d" 'kill-buffer-and-window
    "e" nil
    "eb" 'rod/eval-buffer
    "ee" 'rod/edit-emacs
    "ef" 'eval-defun
    "er" 'eval-region
    "es" 'eshell
    "et" 'vterm
    "f" 'projectile-find-file
    "g" 'magit-status
    "h" 'consult-recent-file
    "i" 'consult-imenu
    "j" 'avy-goto-char-timer
    "k" 'kill-this-buffer
    "l" 'consult-line
    "L" 'magit-log-buffer-file
    "m" nil
    "ma" 'bookmark-set
    "mm" 'consult-bookmark
    "mj" 'bookmark-jump
    "md" 'bookmark-delete
    "n" nil
    "na" 'org-agenda
    "nc" 'org-capture
    "nf" 'org-roam-node-find
    "ni" #'(lambda () (interactive) (org-capture t "i"))
    "nj" 'org-journal-new-entry
    "nl" 'org-roam-buffer-toggle
    "nn" 'rod/find-org-file
    "o" 'delete-other-windows
    "p" 'projectile-command-map
    "q" 'consult-ripgrep
    "r" #'rod/rename
    "s" 'save-buffer
    "t" 'rgr-command-map
    "u" (general-simulate-key "C-x t")
    "v" (general-simulate-key "C-x 4")
    "w" 'ace-window
    "x" nil
    "y" nil
    "yg" 'git-link
    "yf" 'rod/copy-filename
    "yd" 'rod/copy-directory
    "z" nil
    "zp" 'prodigy
    "zd" 'docker
    "zk" 'kubernetes-overview)

  (general-nmap
    "K" 'eldoc-print-current-symbol-info
    ;; goto
    "ga" 'lsp-execute-code-action
    "gb" 'xref-pop-marker-stack
    "ghs" 'git-gutter:stage-hunk
    "ghr" 'git-gutter:revert-hunk
    "ghp" 'git-gutter:popup-hunk
    "gl" 'ace-link
    "go" 'lsp-organize-imports
    "gr" 'xref-find-references
    "gI" 'rod/goto-implementation
    "]e" 'flymake-goto-next-error
    "[e" 'flymake-goto-prev-error
    "]l" 'next-error
    "[l" 'previous-error
    "]g" 'git-gutter:next-hunk
    "[g" 'git-gutter:previous-hunk))

;; make sure to setup bindings after Emacs has started
(add-hook 'after-init-hook #'rod/setup-bindings)
(add-hook 'evil-mode-hook #'rod/setup-bindings)

;;; session management

;; session manager
(use-package desktop
  :custom
  (desktop-dirname (expand-file-name user-emacs-directory))
  (desktop-restore-eager 2)
  :config
  (desktop-save-mode 1))

;; remember window configurations
(use-package winner
  :defer 1
  :config (winner-mode 1))

;;; git

;; the best git interface out there
(use-package magit
  :custom
  (vc-follow-symlinks t)
  :commands (magit-status)
  :hook (magit-blame-mode . evil-emacs-state)
  :config
  (setq magit-process-finish-apply-ansi-colors t)
  (setq magit-clone-default-directory rod/project-dir)
  (setq magit-clone-set-remote.pushDefault t))

(use-package git-modes)

;; little hack to make blame play nice with evil
(defun rod/magit-blame-quit ()
  "Return to normal state after quitting `magit-blame-mode'."
  (when (not (bound-and-true-p magit-blame-mode))
    (evil-normal-state)))
;; advice to make sure we return to normal state after quitting
(advice-add 'magit-blame-quit :after #'rod/magit-blame-quit)

;; this is good for seeing what has changed
(use-package diff-hl
  :defer 1
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1))

;; this is good for navigating between hunks
(use-package git-gutter
  :defer 1
  :custom
  (git-gutter:ask-p nil)
  (git-gutter:display-p nil)
  :config
  (global-git-gutter-mode 1)
  (add-to-list 'git-gutter:update-commands #'other-window))

;; get link to a git file
(use-package git-link
  :defer t
  :custom
  (git-link-use-commit t))

;; my package for cloning git repositories
(use-package clone
  :straight (clone :host github :repo "rodweb/clone.el" :branch "main")
  :commands (clone-repo)
  :custom
  (clone-protocol 'ssh)
  (clone-directory rod/project-dir))

;;; qualify of life
;; ignore native compilation warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; set and load custom file
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; confirm with y/n instead of yes/no
(defalias 'yes-or-no-p #'y-or-n-p)
;; (setq confirm-kill-emacs #'y-or-n-p)

;;; backups
(make-directory rod/backup-dir t)
(setq backup-by-copying t)
(setq backup-directory-alist (list (cons "." rod/backup-dir)))

;;; file management

;; the best file manager out there
(use-package dired
  :straight nil
  :custom
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-dwim-target t)
  :commands (dired dired-jump)
  :init (put 'dired-find-alternate-file 'disabled nil)
  :bind
  (:map dired-mode-map
        ("-" . #'dired-up-directory)
        ("+" . #'dired-create-empty-file)
        ("C-c C-q" . #'wdired-change-to-wdired-mode)
        ("<return>" . #'dired-find-alternate-file))
  :general
  (general-nvmap
    "-" #'dired-jump)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode)
         (dired-mode . diff-hl-dired-mode)))

;; omit some files from dired
(use-package dired-hide-dotfiles
  :after dired
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :config
  (dired-hide-dotfiles-mode))

;; minibuffer completion
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

;; filter minibuffer candidates in any order
(use-package orderless
  :after vertico
  :custom
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp
                               orderless-initialism))
  :hook
  (minibuffer-setup . rod/use-orderless-in-minibuffer))

;; display additional information in the minibuffer
(use-package marginalia
  :after vertico
  :bind
  (:map minibuffer-local-map
        ("M-a" . marginalia-cycle))
  :config
  (advice-add #'marginalia-cycle :after
              (lambda ()
                (let ((inhibit-message t))
                  (customize-save-variable 'marginalia-annotator-registry
                                           marginalia-annotator-registry))))
  (add-to-list 'marginalia-command-categories '(projectile-find-file . file))
  (marginalia-mode 1))

;; contextual actions in the minibuffer
(use-package embark
  :after vertico
  :bind
  (("C-h b" . embark-bindings)
   (:map minibuffer-local-map
         ("M-<return>" . embark-act)))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

;; additional minibuffer functions
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

;; integrate embark with consult
(use-package embark-consult
  :after consult)

;; save minibuffer history
(use-package savehist
  :straight nil
  :after vertico
  :config
  (savehist-mode))

;; keep track of recently opened files
(use-package recentf
  :straight nil
  :after vertico
  :custom
  (recentf-max-saved-items 50)
  :config
  ;; saves every 5 minutes
  (run-at-time nil (* 5 60) #'recentf-save-list)
  (recentf-mode))

;; completion overlay
(use-package corfu
  :custom
  (completion-cycle-threshold 3)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-auto-delay 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match nil)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package cape :after corfu)

;; vim-like tab management for multiple window configurations
(use-package tab-bar
  :straight nil
  :custom
  (tab-bar-show 1)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  :config
  (tab-bar-mode 1))

;; ripgrep utility
(use-package rg
  :defer 1
  :config
  (rg-enable-default-bindings)
  (add-to-list 'rg-required-command-line-flags "--max-columns=2000"))

;; custom search to ignore test files
(rg-define-search rg-no-tests
  :format regexp
  :dir project
  :files "all"
  :flags ("--glob '!*.(spec|test).(js|ts|tsx|jsx)'"
          "--glob '!*_test.go'")
  :menu ("Custom" "n" "No tests"))

;; custom toggle for word wrap
(rg-define-toggle "-w" "w")
;; custom toggle for ignoring test files
(rg-define-toggle "--glob '!*.(spec|test).(js|ts|tsx|jsx)' --glob '!*_test.go'" "N")

;; enable JavaScript related modes
(use-package json-mode :defer t)
(use-package yaml-mode :defer t)
(use-package jq-mode :defer t)
(use-package typescript-mode :defer t)

;; use the correct version of node for the current buffer based on nvm
(use-package nvm
  :hook ((typescript-mode js-mode js-ts-mode tsx-ts-mode) . nvm-use-for-buffer))

;; navigate between errors on compilation buffers
;; TODO: Does this really work?
(use-package compile-eslint
  :straight (compile-eslint :host github :repo "Fuco1/compile-eslint")
  :config (push 'eslint compilation-error-regexp-alist))

;; prettier integration
(use-package prettier-js
  :hook ((typescript-mode js-mode) . rod/maybe-enable-prettier))

(defun rod/maybe-enable-prettier ()
  "Enable `prettier-js-mode' if there is a configuration file for it."
  (if-let* ((files '(".prettierrc.js" ".prettierrc" "prettier.config.js"))
            (dir (cl-some (lambda (file) (locate-dominating-file default-directory file)) files)))
      (progn (prettier-js-mode +1)
             (setq-local prettier-js-command (concat dir "node_modules/.bin/prettier")))))

;; custom compilation rule for JavaScript stack traces
(add-to-list 'compilation-error-regexp-alist-alist
             '(javascript-stack-trace
               "at [^ ]+ (\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
               1 2 3 1))
(add-to-list 'compilation-error-regexp-alist 'javascript-stack-trace)
(add-to-list 'compilation-transform-file-match-alist
             '("internal/.*\\.js" . nil))

(defun rod/setup-js ()
  "Setup JavaScript defaults."
  (setq-local tab-width 2
              js-indent-level 2
              javascript-indent-level 2
              typescript-indent-level 2))

(dolist (hook '(js-mode-hook
                js-ts-mode-hook
                typescript-mode-hook
                typescript-ts-mode-hook
                tsx-ts-mode-hook))
  (add-hook hook #'rod/setup-js))

;; project management
(use-package projectile
  :defer 1
  :custom
  (projectile-generic-command "rg --files --hidden")
  (projectile-project-search-path (list rod/project-dir))
  (projectile-create-missing-test-files t)
  :config
  (projectile-mode))

;; add syntax highlighting for more languages
(use-package jenkinsfile-mode :defer t)
(use-package markdown-mode :defer t)

(use-package nginx-mode
  :mode "\\.j2\\'")

;; smart code format
(use-package ws-butler
  :defer t)

;; editorconfig integration
(use-package editorconfig
  :hook
  (prog-mode . editorconfig-mode)
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode))

;; tree-sitter integration
(use-package tree-sitter
  :defer 1
  :straight nil
  :custom
  (treesit-extra-load-path '(rod/ts-modules-dir))
  :config
  (setq tree-sitter-debug-jump-buttons t
        tree-sitter-debug-highlight-jump-region t)
  (global-tree-sitter-mode))

;; enable tree-sitter syntax highlighting
(use-package tree-sitter-langs
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))

;; dumb jump to definition that actually works
;; TODO: Make it play nice with evil and eglot
(use-package dumb-jump
  :hook
  (prog-mode . dumb-jump-mode)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; quickly run single file scripts (even for compiled languages)
(use-package quickrun
  :custom
  (quickrun-focus-p nil)
  :bind
  ("C-c ," . quickrun))

;; quick vertical jump
(use-package avy
  :commands
  (avy-goto-char-timer)
  :custom
  (avy-background t)
  (avy-timeout-seconds 0.3))

;; jump to links
(use-package ace-link :defer t)

;; contextual selection expansion
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(defun rod/show-trailing-whitespace-in-prog-mode ()
  "Show trailing whitespace in programming modes."
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'rod/show-trailing-whitespace-in-prog-mode)

(defun rod/setup-programming-defaults ()
  "Setup programming defaults."
  (setq-default indent-tabs-mode nil)

  (save-place-mode 1)
  (show-paren-mode 1)
  (column-number-mode 1)
  (electric-pair-mode 1)
  (global-hl-line-mode 1)
  (delete-selection-mode 1)

  ;; better default functions
  (global-set-key [remap isearch-forward] #'isearch-forward-regexp)
  (global-set-key [remap isearch-backward] #'isearch-backward-regexp)
  (global-set-key [remap list-buffers] #'ibuffer)
  (global-set-key [remap dabbrev-expand] #'hippie-expand))
(add-hook 'after-init-hook #'rod/setup-programming-defaults)

;; diff tool
(use-package ediff
  :straight nil
  :custom
  (ediff-diff-options "-w")
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))


;; service management
(use-package prodigy
  :commands prodigy)

;; note taking
(use-package org
  :defer t
  :hook (org-capture-mode . evil-insert-state)
  :init
  (setq org-directory "~/org")
  :bind (("C-M-<return>" . org-meta-return))
  :custom
  (org-imenu-depth 4)
  (org-agenda-files '("~/org/inbox.org" "~/org/work.org" "~/org/personal.gpg" "~/org/habits.gpg"))
  (org-archive-location "~/org/archive.org::datetree/* Finished Tasks")
  (org-crypt-key "rod.apd@gmail.com")
  (org-capture-templates '(("i" "Inbox" entry (file+headline "inbox.org" "Inbox") "** TODO %?")))
  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)
                              (sql . t)
                              (js . t)
                              (calc .t)
                              (shell . t))))

;; journaling
;; TODO: Replace with org-roam?
(use-package org-journal
  :defer t
  :custom
  (org-journal-dir "~/org/journal")
  (org-journal-encrypt-journal t)
  (org-journal-file-type 'monthly)
  (org-journal-date-format "%F"))

;; zettelkasten note taking
(use-package org-roam
  :defer t
  :init (setq org-roam-v2-ack t)
  :hook ((org-mode . auto-fill-mode)
         (org-capture-mode . auto-fill-mode))
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-capture-templates
   '(("d" "default" plain "\n\n* ${title}\n\n%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

;; little hack for org-roam problem that I don't even remember
(defun my/sleep (_ &rest _)
  (sleep-for 0 1))
(advice-add 'org-roam-db-query :before #'my/sleep)

;; snippet management
(use-package yasnippet
  :hook ((org-mode prog-mode) . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("C-<tab>" . yas-next-field))
  :config (yas-reload-all))

;; snippet collection
(use-package yasnippet-snippets
  :after yasnippet)

;; I like this one, but it's not working on Mac OS for some reason
;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :custom (doom-modeline-height 30))

;; this is needed for general.el, I believe?
(setq evil-want-keybinding nil)

;; the best vim emulation out there
(use-package evil
  :defer 1
  :general
  (general-nmap
    :keymaps 'org-mode-map
    "TAB" #'org-cycle)
  :custom
  (evil-want-C-i-jump t)
  (evil-want-Y-yank-to-eol t)
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-redo)
  (evil-symbol-word-search t)
  (evil-default-state 'emacs)
  (evil-kill-on-visual-paste nil)
  :config
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'vterm-mode 'insert)
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'text-mode 'normal)
  (evil-set-initial-state 'conf-mode 'normal)
  (evil-set-initial-state 'go-dot-mod-mode 'normal)
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'ert-results-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'cider-repl-mode 'emacs)
  (evil-mode))

;; port of vim-commentary
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

;; tree-sitter aware text objects
(use-package evil-textobj-tree-sitter
  :after (evil tree-sitter)
  :config
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer")))

;; go to last change
(use-package goto-chg
  :after evil)

;; terminal emulator
(use-package vterm
  :commands vterm)

;; pass integration
(use-package pass :defer t)

;; auth source integration, mostly useful for magit and forge I believe
(use-package auth-source-pass
  :custom (auth-sources '(password-store)))

;; HTTP requests
(use-package verb
  :hook (org-mode . rod/setup-verb-mode))

;; my package for managing environment variables in verb-mode
(use-package verb-env
  :straight (verb-env :host github :repo "rodweb/verb-env" :branch "main")
  :custom (verb-env-default "tst")
  :hook (verb-mode . verb-env-mode))

(defun rod/setup-verb-mode ()
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((verb . t))))
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))


;; keep the cursor centered, please
(advice-add 'evil-next-line :after #'evil-scroll-line-to-center)
(advice-add 'evil-previous-line :after #'evil-scroll-line-to-center)
(use-package centered-cursor-mode :defer t)

;; structural editing for lisp
(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

;; basic auto completion
(use-package abbrev
  :straight nil
  :custom (abbrev-file-name (expand-file-name (concat user-emacs-directory "abbrev_defs")))
  :hook (prog-mode . abbrev-mode))

;; my unfinished, but kinda working package for running tests
(use-package rgr
  :straight (:host github :repo "rodweb/rgr.el")
  :hook ((typescript-mode javascript-mode) . rgr-mode))

;; SQL formatter
(use-package sqlformat
  :defer t
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-s2")))

(use-package company-terraform
  :defer 1
  :init (company-terraform-init))
;; corfu integration through cape
(add-hook 'terraform-mode-hook (lambda ()
                                 (setq-local completion-at-point-functions
                                             (mapcar #'cape-company-to-capf
                                                     (list #'company-terraform)))))

;; automatic encryption and decryption for gpg buffers
(setq epa-file-select-keys nil)
(setq epa-file-encrypt-to '("264F7C10AC662AE2"
                            "2E0A40A7E26A8A64E057F36F8FC45538900F31F6"))
(epa-file-enable)
(fset 'epg-wait-for-status 'ignore)

;; k8s integration
(use-package kubernetes
  :commands (kubernetes-overview)
  :custom
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600))

;; docker integration
(use-package docker
  :commands (docker))
(use-package dockerfile-mode :defer t)

;; spell checker
(use-package flyspell
  :straight nil
  :custom
  (ispell-program-name "aspell")
  (ispell-list-command "--list")
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

;; jump to a window
(use-package ace-window
  :defer 1
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-ignore-current t))

;; better help buffers
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key))
  :defer 10)

;; fast way to write HTML style tags
(use-package emmet-mode
  :hook
  ((sgml-mode css-mode) . (emmet-mode))
  :config
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode))

;; LSP client
(use-package eglot
  :hook
  ((typescript-mode tsx-ts-mode go-mode go-ts-mode clojure-mode) . eglot-ensure)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  :config
  (add-to-list 'eglot-server-programs `(clojure-mode . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs `(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs `(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs `(go-ts-mode . ("gopls"))))

;; eglot and corfu integration through cape
(defun rod/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-super-capf
                     #'eglot-completion-at-point
                     #'cape-file))))

(add-hook 'eglot-managed-mode-hook #'rod/eglot-capf)

;; TODO: Make it work without evaluating the function twice
(defun rod/eglot-bindings ()
  "Setup eglot bindings."
  (general-nmap
    "g." 'eglot-code-action-quickfix
    "ga" 'eglot-code-actions
    "go" 'eglot-code-action-organize-imports
    "gs" 'consult-eglot-symbols))
(add-hook 'eglot-managed-mode-hook #'rod/eglot-bindings)

;; consult integration for eglot
(use-package consult-eglot :after eglot)

;; enable server mode if not running already
(use-package server
  :straight nil
  :hook (after-init . server-mode))

;; GitHub copilot
;; TODO: Use a variable
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :custom (copilot-node-executable "~/.nvm/versions/node/v16.19.0/bin/node")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("C-<return>" . copilot-accept-completion)))

(require 'rod-command)

;;; other programming languages
(use-package rust-mode :defer t)
(use-package go-mode :defer t)
(use-package protobuf-mode :defer t)
(use-package gotest :after go-mode)
(use-package terraform-mode :defer t)
(use-package lua-mode :defer t)

;; Clojure
(use-package clojure-mode :defer t)
(use-package cider :after clojure-mode)

;;; misc

;; Consider _ as part of a word
(modify-syntax-entry ?_ "w")

;; Do not consider = as part of a word
(modify-syntax-entry ?= ".")

(require 'rod-messagebird nil t)

;; set frame title to buffer name
(setq frame-title-format
      '(:eval
        (if (buffer-file-name)
            (abbreviate-file-name (buffer-file-name))
          "%b")))

;; add recompile binding
(global-set-key (kbd "M-g r") 'recompile)
