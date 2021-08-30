;;; yh-make.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh-make-insert-var (var-name)
  "Insert reference to VAR-NAME."
  (interactive "svariable name:")
  (insert "$(" var-name ")"))

(provide 'yh-make)
;;; yh-make.el ends here
