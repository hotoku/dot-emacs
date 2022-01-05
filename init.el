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





;;; use-package initialize
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ; automatically install missing packages


;;; configuration of packagseo
(unless (featurep 'zetasql-formatter)
  (ignore-errors
    (message "zetasql-formatter is not installed.")
    (call-process-shell-command
     "git clone git@github.com:hotoku/zetasql-formatter-el /tmp/zetasql-formatter-el"
     nil "*install-zetasql-formatter-el*")
    (package-install-file "/tmp/zetasql-formatter-el/zetasql-formatter.el")))

(use-package gnu-elpa-keyring-update
  :config
  (unless (file-directory-p package-gnupghome-dir)
    (make-directory package-gnupghome-dir))
  (gnu-elpa-keyring-update))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package yh :ensure nil)

(use-package yh-blog :ensure nil)

(use-package yh-fef :ensure nil
  :hook
  (emacs-lisp-mode . (lambda () (add-hook 'before-save-hook 'yh-fef-format-buffer nil t))))

(use-package yh-docker :ensure nil
  :hook
  (dockerfile-mode
   .
   (lambda ()
     (add-hook 'before-save-hook 'yh-docker-upcase-command nil t))))

(use-package yh-font :ensure nil
  :config
  (yh-font-initialize))

(use-package yh-make :ensure nil)

(use-package yh-sh :ensure nil)

(use-package yh-save :ensure nil)

(use-package yh-pyenv :ensure nil)


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
