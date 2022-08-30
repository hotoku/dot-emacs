;;; yh-markdown.el --- utilities for markdown mode -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh-md-insert-link (refnum url)
  "Insert link with reference mark REFNUM and URL."
  (interactive "srefnum: \nsurl: ")
  (insert (format "[%s]: %s" refnum url)))

(provide 'yh-markdown)
;;; yh-markdown.el ends here
