;;; init.el -- initial setting up process -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


;;; package
(require 'package)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


;;; package-refresh-contents
;; Package-refresh-contents if last update is older than or equal to yesterday.
(let* ((path (expand-file-name ".date-of-last-package-refresh-contents" user-emacs-directory))
       (today (format-time-string "%Y-%m-%d"))
       (last-date (when (file-exists-p path)
                    (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-substring 1 11))))
       (should-update (or (not last-date)
                          (string< last-date today))))
  (when should-update
    (package-refresh-contents)
    (with-temp-buffer
      (insert today)
      (write-region (point-min) (point-max) path))))
(package-initialize)


;;; my utilities
(defconst yh/additional-loadpath (expand-file-name "yh" user-emacs-directory))
(add-to-list 'load-path yh/additional-loadpath)


;;; use-package initialize
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ; automatically install missing packages


;;; configuration of packages
(use-package gnu-elpa-keyring-update) ; This should be first.

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package yh :ensure nil)

(use-package yh-blog :ensure nil)

(use-package yh-fef :ensure nil
  :hook
  (emacs-lisp-mode . (lambda () (add-hook 'before-save-hook 'yh-fef-format-buffer nil t))))

(use-package yh-docker :ensure nil)

(use-package yh-font :ensure nil
  :config
  (yh-font-initialize))

(use-package yh-make :ensure nil)

(use-package yh-sh :ensure nil)

(use-package yh-sql :ensure nil)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((js-mode . lsp))
  :commands
  (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-docker
  :defer t
  :config
  (defvar lsp-docker-client-packages
    '(lsp-clangd))

  (defvar lsp-docker-client-configs
    '((:server-id clangd :docker-server-id clangd-docker :server-command "/ccls/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04/bin/clangd")
      (:server-id pyls :docker-server-id pyls-docker :server-command "pyls")))

  ;; (lsp-docker-init-clients
	;;  :path-mappings '(("/Users/hotoku/projects/hotoku/lineage" . "/projects"))
	;;  :client-packages lsp-docker-client-packages
	;;  :client-configs lsp-docker-client-configs)
  :commands lsp-docker-init-clients)

(use-package lsp-pyright
  :config
  (defvar python-shell-virtualenv-root "")
  (defvar python-shell-interpreter "")
  (defvar python-shell-interpreter-args "")
  (defvar pyvenv-activate "")
  (defun yh/lsp-pyright-setup ()
    "Setup python environment. Inspired from ncaq's init.el.
https://github.com/ncaq/.emacs.d/blob/d1c8651f2683e110a6e4f7e3cd89c025812a6d0e/init.el#L1321"
    (cond
     ;; in poetry project
     ((locate-dominating-file default-directory "pyproject.toml")
      (setq-local lsp-pyright-venv-path
                  (poetry-get-virtualenv)
                  ;; (expand-file-name "site-packages"
                  ;;                   (car (file-expand-wildcards
                  ;;                         (expand-file-name "lib/python*" (poetry-get-virtualenv)))))
                  )
      (lsp))
     ;; in Pipenv project. but this branch is not tested.
     ((locate-dominating-file default-directory "Pipfile")
      (pyvenv-track-virtualenv)
      (pipenv--force-wait (pipenv-venv))
      (when python-shell-virtualenv-root
        (setq-local pyvenv-activate (directory-file-name python-shell-virtualenv-root))
        (setq-local python-shell-interpreter "pipenv")
        (setq-local python-shell-interpreter-args "run jupyter console --simple-prompt")
        (setq-local lsp-pyright-venv-path python-shell-virtualenv-root))
      (lsp))
     (t
      (lsp))))

  :hook
  (python-mode . yh/lsp-pyright-setup))

(use-package flycheck
  :config
  (setq
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-idle-change-delay 1
   flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package magit
  :bind (("C-c g" . magit))
  :custom
  (magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18) "show time of the commits"))

(use-package company
  :config
  (global-company-mode)
  (setq-default company-idel-delay 0.01))

(use-package desktop
  :init
  (desktop-save-mode)
  :custom
  (desktop-path (let ((path (expand-file-name ".desktop" user-emacs-directory)))
                  (unless (file-exists-p path)
                    (make-directory path))
                  `(,path))))

(use-package paren
  :init
  (show-paren-mode))

(use-package dired
  :ensure nil
  :bind
  ("C-x C-j" . yh/dired)
  :config
  (bind-key "z" 'yh/dired-do-open dired-mode-map))

(use-package savehist
  :init
  (savehist-mode)
  :custom
  (savehist-additional-variables '(kill-ring)))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package bazel)

(use-package biblio)

(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
                (add-hook 'before-save-hook #'(lambda () (indent-region (point-min) (point-max))) nil t))))

(use-package company-stan
  :hook (stan-mode . company-stan-setup)
  :config
  (setq company-stan-fuzzy nil))

(use-package dap-mode)

(use-package dap-python :ensure nil)

(use-package dired-x :ensure nil)

(use-package dockerfile-mode)

(use-package eldoc-stan
  :hook (stan-mode . eldoc-stan-setup))

(use-package flycheck-stan
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup))
  :config
  (setq flycheck-stanc-executable nil)
  (setq flycheck-stanc3-executable nil))

(use-package flymake-yaml
  :hook (yaml-mode . flymake-yaml-load))

(use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode
   .
   (lambda ()
     (add-hook 'before-save-hook 'yh/indent-buffer nil t)
     (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
     (add-hook 'before-save-hook 'yh-fef-format-buffer nil t)
     (local-set-key (kbd "RET") 'yh/ret-hs)
     (add-hook 'after-save-hook
               #'(lambda ()
                   (save-excursion
                     (hs-hide-all)
                     (hs-show-block))) nil t)
     (emojify-mode -1))))

(use-package emojify
  :if (display-graphic-p)
  :hook (after-init . global-emojify-mode)
  :bind
  ("C-x e" . 'emojify-insert-emoji))

(use-package git-ps1-mode
  :config
  ;; Only when __git_ps1 is found by git-ps1-mode-find-ps1-file or site-local/git_ps1_location.el.
  ;; Site-local/git_ps1_location.el should iclude (setq git-ps1-mode-ps1-file "path/to/git/ps1/function").
  (when (or (git-ps1-mode-find-ps1-file)
            (let ((path (expand-file-name "site-local/git-ps1-location.el")))
              (and (file-exists-p path)
                   (load-file path))))
    (add-hook 'dired-mode-hook 'git-ps1-mode)))

(use-package gitignore-mode
  :defer t
  :config
  :hook (gitignore-mode
         .
         (lambda ()
           (setq-local require-final-newline t)
           (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))

(use-package haskell-mode
  :mode
  (("\\.hs\\'" . haskell-mode)))

(use-package helm
  :bind (("M-x" . helm-M-x)
	       ("M-y" . helm-show-kill-ring)
	       ("C-x C-f" . helm-find-files)
	       ("C-c h o" . helm-occur)
	       ("C-c m" . helm-mini))
  :hook
  ((dired-mode . (lambda () (define-key dired-mode-map (kbd "j") 'helm-find-files))))
  :config
  (helm-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (setq-default
   helm-idle-delay 0.1
   helm-input-idle-delay 0.1
   helm-delete-minibuffer-contents-from-point t
   helm-ff-auto-update-initial-value nil))

(use-package helm-ag
  :defer t
  :init
  (setq helm-ag-base-command "rg"))

(use-package hideshow
  :init
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-hide-all 100)
  (add-hook 'python-mode-hook 'hs-minor-mode))

(use-package highlight-indentation
  :hook ((yaml-mode . highlight-indentation-mode)
         (yaml-mode . highlight-indentation-current-column-mode))
  :config
  (set-face-background 'highlight-indentation-face "gray36")
  (set-face-background 'highlight-indentation-current-column-face "SteelBlue3"))

(use-package js
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . javascript-mode))
  :config
  (setq js-indent-level 2))

(use-package json-mode
  :mode
  (("\\.json\\'" . json-mode)
   ("\\.geojson\\'" . json-mode)))

(use-package markdown-mode
  :mode
  (("\\.md\\'" . markdown-mode))
  :config
  (setq
   ;; commonmarker command can be installed by "gem install -V commonmarker -n <destination directory>"
   markdown-command "commonmarker --extension=autolink --extension=strikethrough --extension=table --extension=tagfilter --extension=tasklist"
   markdown-command-needs-filename t
   markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css"
                        "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
   markdown-xhtml-header-content (mapconcat 'identity
                                            '("<style><!-- CSS HERE --></style>"
                                              "<script src=\"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js\"></script>"
                                              "<script>hljs.initHighlightingOnLoad();</script>")
                                            "\n")
   markdown-xhtml-body-preamble "<div class=\"markdown-body\">"
   markdown-xhtml-body-epilogue "</div>")
  (bind-keys :map markdown-mode-map
             ("C-c d" . yh/insert-date)
             ("C-c t" . yh/insert-time)))

(use-package open-junk-file
  :commands open-junk-file)

(use-package poetry)

(use-package prettier-js
  :hook
  ((js-mode . prettier-js-mode)))

(use-package projectile
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package pyenv-mode-auto)

(use-package python
  :ensure nil
  :hook
  (python-mode . (lambda ()
                   (add-hook 'after-save-hook 'yh/make-executable nil t)))
  (python-mode . (lambda ()
                   (set (make-local-variable 'compile-command)
                        (concat "pysen run_files lint --error-format gnu  " buffer-file-name)))))

(use-package sh-script
  :mode
  (("\\.sh\\'" . shell-script-mode)
   ("\\.envrc\\'" . shell-script-mode))
  :config
  (add-hook 'sh-mode-hook
            #'(lambda () (add-hook 'after-save-hook 'yh/make-executable nil t))))

(use-package smartparens
  :init
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  :hook
  ((c-mode-common . turn-on-smartparens-strict-mode)
   (emacs-lisp-mode . turn-on-smartparens-strict-mode)
   (python-mode . turn-on-smartparens-strict-mode)
   (haskell-mode . turn-on-smartparens-strict-mode))
  :bind (("C-M-f" . sp-forward-slurp-sexp)
         ("C-M-g" . sp-forward-barf-sexp)))

(use-package smartparens-config :ensure nil)

(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  :config
  (setq stan-indentation-offset 2))

(use-package stan-snippets
  :hook (stan-mode . stan-snippets-initialize))

(use-package undo-tree
  :config
  (global-undo-tree-mode t))

(use-package which-key
  :config
  (which-key-mode))

(use-package yaml-mode
  :mode
  (("\\.ya?ml\\'" . yaml-mode)))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package yatex

  ;; cf. https://zenn.dev/maswag/books/latex-on-emacs/viewer/yatex
  :commands (yatex-mode)
  :mode (("\\.tex$" . yatex-mode)
         ("\\.ltx$" . yatex-mode)
         ("\\.cls$" . yatex-mode)
         ("\\.sty$" . yatex-mode)
         ("\\.clo$" . yatex-mode)
         ("\\.bbl$" . yatex-mode))
  :init
  (setq YaTeX-inhibit-prefix-letter t)
  ;; :config キーワードはライブラリをロードした後の設定などを記述します。
  :config
  (setq YaTeX-kanji-code nil)
  (setq YaTeX-latex-message-code 'utf-8)
  (setq YaTeX-use-LaTeX2e t)
  (setq YaTeX-use-AMS-LaTeX t)
  (setq tex-command "mytex")
  (setq tex-pdfview-command "/usr/bin/open -a Skim")
  (auto-fill-mode 0)
  ;; company-tabnineによる補完。companyについては後述
  (set (make-local-variable 'company-backends) '(company-tabnine))
  ;; keys
  (add-hook 'yatex-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-f") 'yh/insert-subscript)
               (local-set-key (kbd "C-c C-g") 'yh/insert-superscript))))

(use-package direx)

(use-package dired-k
  :hook
  (dired-after-readin . dired-k-no-revert)
  (dired-initial-position . dired-k))


;;; misc
;; make backup files in a specific directory
(setq make-backup-files t)
(add-to-list 'backup-directory-alist
	           `("\\.*\\'" . ,(expand-file-name "~/backup")))

;; meta key
(setq ns-command-modifier (quote meta)
      ns-alternate-modifier (quote super))

;; global key
(progn
  (global-set-key (kbd "M-u") 'revert-buffer)
  (global-set-key (kbd "C-M-/") 'comment-region)
  (global-set-key (kbd "C-M--") 'uncomment-region)
  (global-set-key [?¥] [?\\])
  (global-set-key (kbd "C-.") 'yh/other-window-or-split))

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
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode)
  (blink-cursor-mode -1))

;; language
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment "Japanese")

;; change default directory for C-x C-f
(when (version< "27.0" emacs-version)
  (defun ad:helm-find-files (f prompt)
    "In Emacs 27.1 on Mac OS X, default directory for buffers like *Emacs* changes from ~ to /.
This is inconvinient when opening file at the beginning of Emacs session."
    (when (equal default-directory "/")
      (setq default-directory "~/"))
    (funcall f prompt))
  (advice-add 'helm-find-files :around 'ad:helm-find-files))


;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "246cd0eb818bfd347b20fb6365c228fddf24ab7164752afe5e6878cb29b0204e" default))
 '(package-selected-packages
   '(direx dired-k exec-path-from-shell dired yh-make il lsp-docker poetry gitignore-mode helm-ag pyenv afuternoon-theme afternoon-theme bazel-mode pyenv-mode-auto prettier-js dap-python py-autopep8 flymake-yaml dockerfile-mode biblio elpy haskell-mode yaml-mode json-mode gnu-elpa-keyring-update undo-tree git-ps1-mode ace-window flycheck yasnippet open-junk-file dakrone-theme smartparens helm company use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit fixed-pitch :background "SkyBlue1" :foreground "gray13"))))
 '(markdown-header-delimiter-face ((t (:foreground "indian red"))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :family "MeiryoKe_UIGothic"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0 :underline t))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-language-keyword-face ((t (:foreground "dark green"))))
 '(markdown-markup-face ((t (:foreground "indian red"))))
 '(sp-pair-overlay-face ((t (:inherit fixed-pitch :background "SkyBlue1" :foreground "gray13")))))

;; Theme loading should be after registration of it as safe by custom-set-variables
(use-package afternoon-theme
  :config
  (load-theme 'afternoon))

(provide 'init)
;;; init.el ends here
