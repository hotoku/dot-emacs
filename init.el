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
  :init
  (unless (file-directory-p package-gnupghome-dir)
    (make-directory package-gnupghome-dir))
  (gnu-elpa-keyring-update))



;;; image-typesにsvg, gifが入っておらずtreemacsのrequireに失敗するのでworkaround
;;; todo: linuxのときだけで良い気がする
;;; todo: 29に上がったら外しても大丈夫か確認する
(add-to-list 'image-types 'svg)
(add-to-list 'image-types 'gif)

(use-package yh :ensure nil)

(use-package yh-todo :ensure nil
  :commands
  (yh-todo-open)
  :bind
  (("C-c t" . yh-todo-open)))

(use-package yh-fosi :ensure nil)

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

(use-package yh-lsp-python :ensure nil
  :hook
  (python-mode . yh-lsp-pyright-setup))

(use-package yh-markdown :ensure nil
  :commands
  (yh-md-insert-br
   yh-md-insert-link
   yh-md-insert-tag)

  :hook
  (markdown-mode . (lambda ()
                     (local-set-key (kbd "C-c n") #'yh-md-insert-br)
                     (local-set-key (kbd "C-c l") #'yh-md-insert-link)
                     (local-set-key (kbd "C-c @") #'yh-md-insert-tag))))

(use-package yh-fosi :ensure nil)



(use-package magit
  :bind (("C-c g" . magit))
  :custom
  (magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18) "show time of the commits")
  (magit-refresh-verbose t))



;;; misc
;; make backup files in a specific directory
(setq make-backup-files t)
(add-to-list 'backup-directory-alist
	           `("\\.*\\'" . ,(expand-file-name "~/backup")))

;; global key
(progn
  (global-set-key (kbd "M-u") 'revert-buffer)
  (global-set-key (kbd "C-M-/") 'comment-region)
  (global-set-key (kbd "C-M--") 'uncomment-region)
  (global-set-key [?¥] [?\\])
  (global-set-key (kbd "C-.") 'yh/other-window-or-split)
  ;; C-t is used by tmux prefix key.
  (global-unset-key (kbd "C-t"))
  (global-unset-key (kbd "s-t")))

;; tab
(setq-default tab-width 2
              indent-tabs-mode nil)

;; truncate lines
(setq-default truncate-lines t)

;; always answer in y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; emacsclient
(server-start)

;; aes
(progn
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (when (window-system) (scroll-bar-mode -1))
  (column-number-mode)
  (blink-cursor-mode -1))

;; language
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment "Japanese")

;; Theme loading should be after registration of it as safe by custom-set-variables
(use-package afternoon-theme
  :config
  (load-theme 'afternoon))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" default))
 '(package-selected-packages
   '(zetasql-formatter yatex yaml-mode which-key web-mode tree-sitter terraform-mode swiper stan-snippets smartparens session pyenv-mode py-autopep8 projectile prettier-js ppp poetry origami open-junk-file nginx-mode magit lsp-ui lsp-pyright json-mode ivy-hydra highlight-indentation helpful haskell-mode graphql-mode gnu-elpa-keyring-update git-ps1-mode git-modes flymake-yaml flycheck-stan emojify eldoc-stan dockerfile-mode direx dired-k dap-mode coverlay company-stan color-moccur biblio bazel afternoon-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
