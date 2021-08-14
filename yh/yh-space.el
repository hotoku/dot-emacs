;;; package -- Summary
;;; Make empty lines equal height.


;;; Commentary:


;;; Code:

(defcustom yh-space-width 2
	"Number of empty lines inserted."
	:safe 'integerp
	:risky nil
  :group 'yh-space
  :type 'integer)

(defun yh-space-blank-line-p ()
  (string-match
   "^[ \t]*$" (buffer-substring
	             (line-beginning-position)
	             (line-end-position))))

(defun yh-space-contents-line-p ()
  (not (yh-space-blank-line-p)))

(defun yh-space-goto-next (predicate)
  (let ((buf 0))
    (while (and (= buf 0)
		            (not (funcall predicate)))
      (setq buf (forward-line)))))

(defun yh-space-goto-top ()
  (goto-char (point-min))
  (yh-space-goto-next
   (function yh-space-contents-line-p)))

(defun yh-space-goto-next-blank ()
  (beginning-of-line)
  (yh-space-goto-next
   (function yh-space-blank-line-p)))

(defun yh-space-goto-next-contents ()
  (beginning-of-line)
  (yh-space-goto-next
   (function yh-space-contents-line-p)))

(defun yh-space-make-gap-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n")
    (yh-space-goto-top)
    (let ((buf 0))
      (while (= buf 0)
	      (let* ((begin-pos
		            (progn
		              (yh-space-goto-next-blank)
		              (point)))
	             (end-pos
		            (progn
		              (yh-space-goto-next-contents)
		              (point))))
	        (goto-char begin-pos)
	        (delete-region begin-pos end-pos)
	        (if (< (point) (point-max))
	            (open-line yh-space-width))
	        (setq buf (forward-line yh-space-width)))))))

(provide 'yh-space)
;;; yh-space.el ends here
