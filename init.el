;;; init.el -- initial setting up process -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


;;; package
(defun yh-doit-if-not-yet-today (func filename)
  "Call FUNC if it is not executed today.
The last executing date is recorded in the FILENAME in `user-emacs-directory.'"
  (let* ((path (expand-file-name filename user-emacs-directory))
         (today (format-time-string "%Y-%m-%d"))
         (last-date (when (file-exists-p path)
                      (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-substring 1 11))))
         (should-update (or (not last-date)
                            (string< last-date today))))
    (when should-update
      (funcall func)
      (with-temp-buffer
        (insert today)
        (write-region (point-min) (point-max) path)))))

(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(let ((orig-value package-check-signature))
  (setq package-check-signature nil)

  (yh-doit-if-not-yet-today 'package-refresh-contents
                            ".date-of-last-package-refresh-contents")
  (package-install 'gnu-elpa-keyring-update)

  (setq package-check-signature orig-value))


;;; my utilities
(let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))


(setq dired-listing-switches "-alh")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zetasql-formatter yatex yaml-mode which-key use-package undo-tree terraform-mode stan-snippets smartparens session pyenv-mode-auto py-autopep8 projectile prettier-js ppp poetry open-junk-file magit lsp-ui lsp-pyright lsp-docker json-par js2-mode ivy-hydra highlight-indentation helpful helm-lsp helm-ag haskell-mode gnu-elpa-keyring-update gitignore-mode git-ps1-mode git-modes flymake-yaml flycheck-stan exec-path-from-shell emojify eldoc-stan dockerfile-mode direx dired-k dap-mode counsel company-stan color-moccur biblio bazel afternoon-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
