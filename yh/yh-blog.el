;;; yh-blog.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:
(defcustom yh-blog-posts-dir (expand-file-name "~/projects/blog/_posts")
  "Absolute path of the `_posts' directory for blos."
  :group 'yh-blog
  :type '(directory :tag "Single directory"))

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

(defun yh-blog-new (title url)
  "Open new blog post of TITLE and URL."
  (interactive "sblog title: \nsurl: ")
  (let* ((y (format-time-string "%Y"))
         (m (format-time-string "%m"))
         (d (format-time-string "%d"))
         (url2 (replace-regexp-in-string " " "-" url))
         (fn (format "%s-%s-%s-%s.md" y m d url2)))
    (find-file (expand-file-name fn yh-blog-posts-dir))
    (insert (format "---
layout: post
title: %s
date: %s-%s-%s %s +0900
tags:
---
" title y m d (format-time-string "%H:%M:%S")))
    (goto-char (point-min))
    (search-forward "tags:")
    (insert " ")))

(defun yh-blog-to-other (dir-nm)
  "Move current post to directory DIR-NM."
  (let*  ((path (buffer-file-name))
          (ls (split-string path "/"))
          (fn (car (last ls)))
          (jekyll-root (seq-take ls (- (length ls) 2)))
          (new-fn (reduce (lambda (x y) (concat x "/" y))
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

(provide 'yh-blog)
;;; yh-blog.el ends here
