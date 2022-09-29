;;; init.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; memo: byte compile all .el files
;; (byte-recompile-directory "~/.emacs.d" 0 t)

;;; Code:

(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))
(require 'yh)
(yh/refresh-package)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


(use-package gnu-elpa-keyring-update
  :config
  (unless (file-directory-p package-gnupghome-dir)
    (make-directory package-gnupghome-dir))
  (gnu-elpa-keyring-update))






(use-package s)

(use-package f)

(use-package poetry)

(use-package pyenv-mode)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode 1))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gnu-elpa-keyring-update poetry pyenv-mode f s yasnippet use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
