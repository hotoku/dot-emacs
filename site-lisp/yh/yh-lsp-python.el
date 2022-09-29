;;; yh-lsp-python.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(require 'pyenv-mode-auto)
(require 'poetry)
(require 'yh-pyenv)

(defun yh-lsp-pyright-setup ()
  "Setup python environment.

Inspired from ncaq's initel.
cf: https://github.com/ncaq/.emacs.d/blob/d1c8651f2683e110a6e4f7e3cd89c025812a6d0e/init.el#L1321"
  (pyenv-mode-auto-hook)
  (cond
   ;; in poetry project
   ((locate-dominating-file default-directory "pyproject.toml")
    (message "opening python file. this is in a project of poetry")
    (setq-local lsp-pyright-venv-path
                (poetry-get-virtualenv))
    (lsp))
   ;; with pyenv
   ((executable-find "pyenv")
    (message "opening python file. pyenv command was found")
    (let ((path (yh-pyenv-directory)))
      (message "pyenv directory is %s" path)
      (setq-local
       lsp-pyright-venv-path path)
      (lsp)))
   (t
    (lsp)))  )

(provide 'yh-lsp-python)
;;; yh-lsp-python.el ends here
