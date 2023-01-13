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

(provide 'rod-util)
