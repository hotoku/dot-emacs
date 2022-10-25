;;; yh-sort-use-package.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:
(eval-when-compile (require 'subr-x))

(defconst yh-sup-begin "yh-sup-begin")
(defconst yh-sup-end "yh-sup-end")

(defun yh-sup-extract-block (buf)
  "Extract use package block from BUF."
  (save-window-excursion
    (save-excursion
      (let ((begin) (end))
        (switch-to-buffer buf)
        (goto-char (point-min))
        (search-forward yh-sup-begin)
        (end-of-line)
        (setq begin (1+ (point)))
        (search-forward yh-sup-end)
        (beginning-of-line)
        (setq end (1- (point)))
        (buffer-substring-no-properties begin end)))))

(defun yh-sup-make-stream (s)
  "Make stream which provides contents of S."
  (lambda (&optional unread)
    (if unread (setq s (concat unread s))
      (prog1 (substring s 0 1)
        (setq s (substring s 1))))))

(defun yh-sup-extract-sexps (buf)
  "Extract use package expressions from the buffer BUF."
  (let* ((curbuf (current-buffer))
         (obj)
         (last-pos)
         (cur-pos)
         (begin)
         (end)
         (hash (make-hash-table))
         (name)
         (str))
    (switch-to-buffer buf)
    (save-excursion
      (goto-char (point-min))
      (setq begin (search-forward yh-sup-begin)
            end (search-forward yh-sup-end))
      (goto-char begin)
      (ignore-errors
        (setq last-pos (point-min))
        (while (< (point) end)
          (setq obj (read buf)
                cur-pos (point))
          (when (and (listp obj) (eq (car obj) 'use-package))
            (setq name (cadr obj)
                  str (string-trim (buffer-substring-no-properties last-pos cur-pos)))
            (puthash name str hash))
          (setq last-pos cur-pos))))
    (switch-to-buffer curbuf)
    hash))

(defun yh-sort-use-package ()
  "Sort use-package expression in init.el buffer."
  (unless (get-buffer "init.el") (cl-return))
  (let* ((buf (get-buffer "init.el"))
         (_ (switch-to-buffer buf)))
    buf))

(provide 'yh-sort-use-package)
;;; yh-sort-use-package.el ends here
