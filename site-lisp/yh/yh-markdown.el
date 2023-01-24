;;; yh-markdown.el --- utilities for markdown mode -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh-md-insert-link (refnum url)
  "Insert link with reference mark REFNUM and URL."
  (interactive "srefnum: \nsurl: ")
  (save-excursion
    (goto-char (point-max))
    (let ((link-keyword "<!-- link -->"))
      (unless (search-backward link-keyword nil t)
        (insert link-keyword "\n"))))

  (save-excursion
    (goto-char (point-max))
    (unless (= (char-before) 10)
      (insert "\n"))
    (insert (format "[%s]: %s\n" refnum url))))

(defun yh-md-insert-br ()
  "Insert br tagl."
  (interactive)
  (insert "<br>"))

(defun yh-md-insert-tag (tag &optional class)
  "Insert TAG around region.  Optionally with CLASS."
  (interactive "stag: \nsclass: ")
  (insert (format "</%s>" tag))
  (save-excursion
    (goto-char (mark))
    (insert (format "<%s" tag))
    (unless (equal class "") (insert (format " class=\"%s\"" class)))
    (insert ">")))

(provide 'yh-markdown)
;;; yh-markdown.el ends here
