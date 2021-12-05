(use-package simple-modeline
  :hook
  (after-init . simple-modeline-mode)
  :config
  (setq simple-modeline-segments
   (list (car simple-modeline-segments)
         (remove 'simple-modeline-segment-minor-modes
                 (car (cdr simple-modeline-segments))))))

(provide 'rod-modeline)
