;;; yh-sql.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh-sql-format ()
  "Format buffer with zetasql-formatter.
https://github.com/Matts966/zetasql-formatter."
  (interactive)
  (unless (= 0 (call-process-shell-command "which zetasql-formatter"))
    (error "Zetasql-formatter is not available"))
  (let* ((curbuf (current-buffer))
         (curpnt (point))
         (outbuf-name "*zetasql*")
         (outbuf (get-buffer outbuf-name))
         (fpath (buffer-file-name)))
    (when outbuf
      (save-excursion
        (switch-to-buffer outbuf)
        (erase-buffer)))
    ;; DIRTY HACK.
    ;; zetasql-formatter return non-zero value when the formatted string is
    ;; different from the original even if it was syntactically correct.
    ;; Here, we want to change to *zetasql* buffern only when it is syntactically
    ;; incorrect. We format the file twice and check the output of second run.
    (let ((ret (progn (call-process-shell-command (format "zetasql-formatter %s" fpath)
                                                  nil outbuf-name)
                      (call-process-shell-command (format "zetasql-formatter %s" fpath)
                                                  nil outbuf-name))))
      (if (= ret 0)
          (progn (switch-to-buffer curbuf)
                 (revert-buffer t t)
                 (goto-char curpnt))))))

(provide 'yh-sql)
;;; yh-sql.el ends here
