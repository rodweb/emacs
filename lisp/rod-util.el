(defun rod/diff-new-buffers ()
  (interactive)
  (dolist (b (list (get-buffer-create "A")
                   (get-buffer-create "B")))
    (with-current-buffer b
      (json-mode))
    (switch-to-buffer "A")
    (switch-to-buffer-other-window "B")))

(defun rod/kill-diff-buffers ()
  (interactive)
  (kill-buffer "A")
  (kill-buffer "B"))

(provide 'rod-util)
