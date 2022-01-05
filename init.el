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

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((js-mode . lsp)
   (c++-mode . lsp))
  :commands
  (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)

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
      (pyenv-mode-auto-hook)
      (setq-local lsp-pyright-venv-path
                  (poetry-get-virtualenv))
      (lsp))
     ;; with pyenv
     ((executable-find "pyenv")
      (setq-local
       lsp-pyright-venv-path
       (yh-pyenv-directory))
      (lsp))
     (t
      (lsp))))
  (defun yh/reset-pyright-poetry ()
    "Reset lsp setting with poetry."
    (interactive)
    (setq-local lsp-pyright-venv-path
                (poetry-get-virtualenv))
    (lsp))

  :hook
  (python-mode . yh/lsp-pyright-setup))

(use-package flycheck
  :config
  (setq
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-idle-change-delay 1
   flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package conf-mode
  :commands
  (conf-toml-mode)
  :hook
  (conf-toml-mode . (lambda ()
                      (yh-before-save :space :gap :indent))))

(use-package magit
  :bind (("C-c g" . magit))
  :custom
  (magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18) "show time of the commits"))

(use-package company
  :config
  (global-company-mode)
  (setq-default company-idel-delay 0.01))

(use-package session)

(use-package paren
  :init
  (show-paren-mode))

(use-package dired
  :ensure nil
  :bind
  ("C-x C-j" . yh/dired)
  :config
  (bind-key "z" 'yh/dired-do-open dired-mode-map)
  (unless (equal (system-name) "JMB20200036.local")
    ;; todo: この変数にhが入っていると、diredでディレクトリの色がおかしくなるときがある
    ;;       ディレクトリがズラッと並んだディレクトリを表示すると、一行ごとに`default'と`dired-directory'が並んでいるように見える
    (setq dired-listing-switches "-alh")))

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

(use-package git-modes
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

(use-package js2-mode
  :hook
  ((js-mode . js2-minor-mode)))

(use-package json-mode
  :mode
  (("\\.json\\'" . json-mode)
   ("\\.geojson\\'" . json-mode))

  :hook
  (json-mode . (lambda ()
                 (yh-before-save :space :gap :indent))))

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
             ("C-c t" . yh/insert-time))
  :hook
  (markdown-mode . (lambda ()
                     (yh-before-save :space :gap)
                     (setq-local yh-space-width 1)
                     (toggle-truncate-lines -1))))

(use-package open-junk-file
  :commands open-junk-file)

(use-package poetry)

(use-package prettier-js
  :hook
  (((js-mode css-mode) . prettier-js-mode)))

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

(use-package smartparens
  :init
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  (setq sp-no-reindent-after-kill-modes (cons 'makefile-bsdmake-mode sp-no-reindent-after-kill-modes))

  :hook
  ((c-mode-common
    emacs-lisp-mode
    python-mode
    haskell-mode
    hcl-mode
    bazel-mode
    js-mode
    json-mode) .
    turn-on-smartparens-strict-mode)

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

(use-package hcl-mode)

(use-package ppp)

(use-package make-mode
  :defer t
  :config
  (define-key makefile-mode-map (kbd "C-c C-j") 'yh-make-insert-var)
  :hook
  ((makefile-bsdmake-mode makefile-gmake-mode) .
   (lambda ()
     (yh-before-save :space :gap))))

(use-package sh-script
  :mode
  (("\\.sh\\'" . shell-script-mode)
   ("\\.envrc\\'" . shell-script-mode))
  :config
  (define-key sh-mode-map (kbd "C-c C-j") 'yh-sh-insert-var)
  :hook
  (sh-mode . (lambda ()
               (yh-before-save :space :gap :indent)))
  (sh-mode . (lambda ()
               (add-hook 'after-save-hook 'yh/make-executable nil t))))

(use-package json-par
  :hook
  (json-mode . json-par-mode))

(use-package swiper
  :bind
  (("M-s M-s" . swiper-thing-at-point)))

(use-package ivy-hydra
  :config
  (setq ivy-read-action-function 'ivy-hydra-read-action))

(use-package ivy
  :bind
  (("C-c m" . ivy-switch-buffer))
  :init
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  :config
  (setq ivy-height 30)
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :bind
  (("C-c a" . counsel-ag))
  :config
  (setq counsel-ag-base-command '("rg"  "--vimgrep" "--no-heading" "--smart-case" "%s"))
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (counsel-mode 1))

(use-package terraform-mode)

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-c F" . helpful-function))
  :init
  (setq counsel-describe-function-function 'helpful-callable)
  (setq counsel-describe-variable-function 'helpful-variable))

(use-package color-moccur)


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
  (global-unset-key (kbd "C-t")))

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
  (scroll-bar-mode -1)
  (column-number-mode)
  (blink-cursor-mode -1))

;; language
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment "Japanese")


;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "246cd0eb818bfd347b20fb6365c228fddf24ab7164752afe5e6878cb29b0204e" default))
 '(package-selected-packages
   '(zetasql-formatter yatex yaml-mode which-key use-package undo-tree terraform-mode stan-snippets smartparens session pyenv-mode-auto py-autopep8 projectile prettier-js ppp poetry open-junk-file magit lsp-ui lsp-pyright lsp-docker json-par js2-mode ivy-hydra highlight-indentation helpful helm-lsp helm-ag haskell-mode gnu-elpa-keyring-update gitignore-mode git-ps1-mode git-modes flymake-yaml flycheck-stan exec-path-from-shell emojify eldoc-stan dockerfile-mode direx dired-k dap-mode counsel company-stan color-moccur biblio bazel afternoon-theme)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-error ((t (:underline nil))))
 '(js2-warning ((t (:underline nil))))
 '(lsp-face-highlight-textual ((t (:inherit highlight :foreground "dark cyan"))))
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

(setq dired-listing-switches "-alh")



