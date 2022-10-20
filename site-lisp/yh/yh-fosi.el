;;; yh-fosi.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh-fosi ()
  "Launch fosi process and watch the file."
  (interactive)
  (let ((orig-buf (current-buffer))
        (buffer (generate-new-buffer "*fosi*")))
    (start-process "fosi" buffer "fosi" "-f" "-i" (buffer-file-name))
    (sit-for 0.5)
    (switch-to-buffer buffer)
    (search-backward "htmlPort")
    (message "%s %s"
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position))
             (buffer-name))
    (switch-to-buffer orig-buf)))

(provide 'yh-fosi)
;;; yh-fosi.el ends here
