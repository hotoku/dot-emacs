;;; yh-sql.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh-sql-format ()
  "Format buffer with zetasql-formatter.
https://github.com/Matts966/zetasql-formatter."
  (interactive)
  (let* ((curbuf (current-buffer))
         (curpnt (point))
         (outbuf-name "*zetasql*")
         (outbuf (get-buffer outbuf-name))
         (fpath (buffer-file-name)))
    (when outbuf
      (save-excursion
        (switch-to-buffer outbuf)
        (erase-buffer)))
    (let ((ret (call-process-shell-command (format "zetasql-formatter %s" fpath)
                                           nil outbuf-name))
          ;; DIRTY HACK.
          ;; zetasql-formatter return non-zero value when the formatted string is
          ;; different from the original even if it was syntactically correct.
          ;; Here, we want to change to *zetasql* buffern only when it is syntactically
          ;; incorrect. We format the file twice and check the output of second run.
          (ret2 (call-process-shell-command (format "zetasql-formatter %s" fpath)
                                            nil outbuf-name)))
      (if (= ret2 0)
          (progn (switch-to-buffer curbuf)
                 (revert-buffer t t)
                 (goto-char curpnt))))))

(provide 'yh-sql)
;;; yh-sql.el ends here
