;;; package -- Summary
;;; Make empty lines equal height.


;;; Commentary:


;;; Code:

(defcustom equally-spaced-width 2
	"Number of empty lines inserted."
	:safe 'integerp
	:risky nil
  :group 'equally-spaced
  :type 'integer)

(defun equally-spaced-blank-line-p ()
  (string-match
   "^[ \t]*$" (buffer-substring
	             (line-beginning-position)
	             (line-end-position))))

(defun equally-spaced-contents-line-p ()
  (not (equally-spaced-blank-line-p)))

(defun equally-spaced-goto-next (predicate)
  (let ((buf 0))
    (while (and (= buf 0)
		            (not (funcall predicate)))
      (setq buf (forward-line)))))

(defun equally-spaced-goto-top ()
  (goto-char (point-min))
  (equally-spaced-goto-next
   (function equally-spaced-contents-line-p)))

(defun equally-spaced-goto-next-blank ()
  (beginning-of-line)
  (equally-spaced-goto-next
   (function equally-spaced-blank-line-p)))

(defun equally-spaced-goto-next-contents ()
  (beginning-of-line)
  (equally-spaced-goto-next
   (function equally-spaced-contents-line-p)))

(defun equally-spaced-make-gap-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n")
    (equally-spaced-goto-top)
    (let ((buf 0))
      (while (= buf 0)
	      (let* ((begin-pos
		            (progn
		              (equally-spaced-goto-next-blank)
		              (point)))
	             (end-pos
		            (progn
		              (equally-spaced-goto-next-contents)
		              (point))))
	        (goto-char begin-pos)
	        (delete-region begin-pos end-pos)
	        (if (< (point) (point-max))
	            (open-line equally-spaced-width))
	        (setq buf (forward-line equally-spaced-width)))))))

(provide 'yh-space)
;;; yh-space.el ends here
