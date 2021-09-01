;;; yh-pyenv.el ---  -*- lexical-binding: t -*-


;;; Commentary:
(defun yh-pyenv-version ()
  "Get current acitve version of pyenv."
  (let* ((curbuf (current-buffer))
         (outbuf-name "*yh-pyenv*")
         (outbuf (get-buffer outbuf-name))
         version)
    (when outbuf
      (save-excursion
        (switch-to-buffer outbuf)
        (erase-buffer)))
    (call-process-shell-command
     "pyenv version" nil outbuf-name)
    (switch-to-buffer outbuf)
    (goto-char 1)
    (search-forward " ")
    (setq version (buffer-substring 1 (1- (point))))
    (message "%s" version)
    (switch-to-buffer curbuf)
    version))

(defun yh-pyenv-directory ()
  "Get the directory of current virtual environment of pyenv."
  (let ((version (yh-pyenv-version))
        (pyenv-home (getenv "PYENV_ROOT")))
    (expand-file-name (format "versions/%s" version) pyenv-home)))


;;; Code:

(provide 'yh-pyenv)
;;; yh-pyenv.el ends here
