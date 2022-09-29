;;; yh-docker.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(require 'thingatpt)

(defun yh-docker-upcase-command ()
  "Make command upper case."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     while (< (point) (point-max)) do
     (let* ((word (word-at-point))
            (bound (bounds-of-thing-at-point 'word)))
       (when (member word '("from" "run" "cmd" "label" "expose" "env"
                            "add" "copy" "entrypoint" "volume" "user"
                            "workdir" "arg" "onbuild" "stopsignal" "healthcheck"
                            "shell"))
         (delete-region (car bound) (cdr bound))
         (insert (upcase word))))
     (forward-line))))

(provide 'yh-docker)
;;; yh-docker.el ends here
