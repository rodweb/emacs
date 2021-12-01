(use-package exec-path-from-shell
  :defer 0.1
  :config
  (add-to-list 'exec-path-from-shell-variables "AWS_SHARED_CREDENTIALS_FILE")
  (exec-path-from-shell-initialize))

(provide 'rod-env)
