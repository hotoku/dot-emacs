;;; yh-blog.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:
(require 'dash)

(defconst yh-blog-posts-dir
  (let ((dic `(("hotoku-macmini-2020" . ,(expand-file-name "~/projects/hotoku/blog/_posts"))
               ("hotoku-macbookair-2019" . ,(expand-file-name "~/projects/hotoku/blog/_posts"))
               ("JMB20200036" . ,(expand-file-name "~/projects/blog/_posts"))
               ("hotoku-macbookair-2019" . ,(expand-file-name "~/projects/hotoku/blog/_posts"))
               ("hotoku-macbook-air-2022" . ,(expand-file-name "~/projects/hotoku/blog/_posts"))
               ;;; sequoiaになって、hostnameがバグっているので苦肉の策
               ("Macmini" . ,(expand-file-name "~/projects/hotoku/blog/_posts")))))
    (cdr (assoc (car (split-string (system-name) "\\.")) dic))))

(defun yh-blog-header (title)
  "Header generation function for hotoku blog.
TITLE: article's title."
  (format "---
layout: post
title: %s
date: %s +0900
categories: blog
tags:
---
" title (format-time-string "%Y-%m-%d %H:%M:%S")))

(defconst yh-blog-inctore-posts-dir
  (let ((dic `(("hotoku-macmini-2020" . ,(expand-file-name "~/projects/inctore/inctore.github.io/_posts"))
               ("hotoku-macbook-air-2022" . ,(expand-file-name "~/projects/inctore/inctore.github.io/_posts")))))
    (cdr (assoc (car (split-string (system-name) "\\.")) dic))))

(defun yh-blog-inctore-header (title)
  "Header generation function for inctore blog.
TITLE: article's title, Y, M, D are date values."
  (format "---
title: %s
last_modified_at: %s
categories:
  - blog
tags:
---
" title (format-time-string "%Y-%m-%dT%H:%M:%S")))

(defun yh-blog-publish ()
  "Commit change and push to remote."
  (interactive)
  (message "pushing")
  (let ((fn (buffer-file-name))
        (process-connection-type nil)
        (buf (get-buffer-create "*yh/publish-blog*"))
        (ret-val nil))
    (call-process "git" nil buf t "add" fn)
    (call-process "git" nil buf t "commit" "-m" "publish")
    (setq ret-val (call-process "git" nil buf t "push"))
    (if (= ret-val 0)
        (message "pushed")
      (message "push failed"))))

(defun yh-blog-new-impl (dir title url header-fn)
  "Open new blog post of TITLE and URL in DIR."
  (let* ((url2 (replace-regexp-in-string " " "-" url))
         (fn (format "%s-%s.md" (format-time-string "%Y-%m-%d") url2)))
    (find-file (expand-file-name fn dir))
    (insert (apply header-fn (list title)))
    (goto-char (point-min))
    (search-forward "tags:")
    (insert " ")))

(defun yh-blog-new (prefix title url)
  "Open new blog post of TITLE and URL.
If PREFIX is given, posts are created in inctore's blog."
  (interactive "P\nsblog title: \nsurl: ")
  (let* ((dir (if prefix yh-blog-inctore-posts-dir yh-blog-posts-dir))
         (url2 (replace-regexp-in-string " " "-" url))
         (header-fn (if prefix 'yh-blog-inctore-header 'yh-blog-header)))
    (message "prefix=%s" prefix)
    (when (not yh-blog-posts-dir) (error "The value of yh-blog-posts-dir is nil.
It can be registered in the file yh-blog.el"))
    (yh-blog-new-impl dir title url2 header-fn)))

(defun yh-blog-to-other (dir-nm)
  "Move current post to directory DIR-NM."
  (let*  ((path (buffer-file-name))
          (ls (split-string path "/"))
          (fn (car (last ls)))
          (jekyll-root (seq-take ls (- (length ls) 2)))
          (new-fn (-reduce (lambda (x y) (concat x "/" y))
                           (append jekyll-root (list dir-nm fn)))))
    (write-file new-fn)
    (when (file-exists-p path)
      (delete-file path))))

(defun yh-blog-to-draft ()
  "Move posts as draft."
  (interactive)
  (yh-blog-to-other "_drafts"))

(defun yh-blog-to-post ()
  "Move post as post."
  (interactive)
  (yh-blog-to-other "_posts"))

(defun yh-blog-preview ()
  "Preview a post."
  (interactive)
  (let* ((fpath (buffer-file-name))
         (fn (file-name-nondirectory fpath))
         (y-m-d (replace-regexp-in-string
                 "-" "/"
                 (replace-regexp-in-string
                  "^\\([0-9]+-[0-9]+-[0-9]+\\).*" "\\1" fn)))
         (body (replace-regexp-in-string
                "^[0-9]+-[0-9]+-[0-9]+-\\(.+\\)\\.md\\'" "\\1" fn))
         (url (concat "http://localhost:4000/" y-m-d  "/" body))
         (buf (get-buffer-create "*yh/publish-blog*")))
    (call-process "open" nil buf t url)))

(defun yh-blog-insert-code (lang)
  "Insert code block whose syntax is LANG."
  (interactive "slanguage: ")
  (insert "```")
  (insert lang)
  (insert "\n")
  (insert "```")
  (beginning-of-line)
  (open-line 1))

(defun yh-blog-compile ()
  "Execute build.sh in the blog project."
  (interactive)
  (compile (format "%s/../_plist/build.sh" yh-blog-posts-dir)))

(defun yh-blog-diary ()
  "Open new entry for diary."
  (interactive)
  (yh-blog-new nil "日記" (format-time-string "diary-%Y-%m-%d")))

(provide 'yh-blog)
;;; yh-blog.el ends here
