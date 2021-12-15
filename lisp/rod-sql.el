(use-package sqlformat
  :defer t
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-s2")))

(provide 'rod-sql)
