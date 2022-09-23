;;; yh-markdown.el --- utilities for markdown mode -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh-md-insert-link (refnum url)
  "Insert link with reference mark REFNUM and URL."
  (interactive "srefnum: \nsurl: ")
  (save-excursion
    (goto-char (point-max))
    (unless (= (char-before) 10)
      (insert "\n"))
    (insert (format "[%s]: %s" refnum url))))

(provide 'yh-markdown)
;;; yh-markdown.el ends here
