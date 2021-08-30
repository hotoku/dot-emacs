;;; yh-sh.el ---  -*- lexical-binding: t -*-


;;; Commentary:
;; Personal utilities for editing shell script.


;;; Code:

(defun yh-sh-insert-var (var-name)
  "Insert reference to VAR-NAME."
  (interactive "svariable name:")
  (insert "${" var-name "}"))

(provide 'yh-sh)
;;; yh-sh.el ends here
