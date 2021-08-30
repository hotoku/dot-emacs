;;; yh-save.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(require 'yh-space)

(defun yh-before-save (&rest config)
  "Configure before save hooks.
CONFIG should be a list of the following symbols.
- :indent
- :space
- :gap"
  (let ((key2func '((:indent . yh/indent-buffer)
                    (:space . delete-trailing-whitespace)
                    (:gap . yh-space-make-gap-buffer))))
    (mapcar #'(lambda (key)
                (let ((func (cdr (assoc key key2func))))
                  (add-hook 'before-save-hook func nil t)))
            config)))

(provide 'yh-save)
;;; yh-save.el ends here
