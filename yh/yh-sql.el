;;; yh-sql.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh-sql-format-command (fpath)
  "Return zetasql-formatter command for FPATH."
  (if (= 0 (call-process-shell-command "which zetasql-formatter"))
      (format "zetasql-formatter %s" fpath)
    (let ((fname (file-name-nondirectory fpath))
          (dname (file-name-directory fpath)))
      (format "docker run --rm -v %s:/home:Z matts966/zetasql-formatter:latest %s"
              dname fname))))

(defun yh-sql-format ()
  "Format buffer with zetasql-formatter.
https://github.com/Matts966/zetasql-formatter."
  (interactive)
  (let* ((curbuf (current-buffer))
         (curpnt (point))
         (outbuf-name "*zetasql*")
         (outbuf (get-buffer outbuf-name))
         (fpath (buffer-file-name))
         (command (yh-sql-format-command fpath)))
    (when outbuf
      (save-excursion
        (switch-to-buffer outbuf)
        (erase-buffer)))
    ;; DIRTY HACK.
    ;; zetasql-formatter return non-zero value when the formatted string is
    ;; different from the original even if it was syntactically correct.
    ;; Here, we want to change to *zetasql* buffern only when it is syntactically
    ;; incorrect. We format the file twice and check the output of second run.
    (let ((ret (progn (call-process-shell-command command
                                                  nil outbuf-name)
                      (call-process-shell-command command
                                                  nil outbuf-name))))
      (if (= ret 0)
          (progn (switch-to-buffer curbuf)
                 (revert-buffer t t)
                 (goto-char curpnt))))))

(provide 'yh-sql)
;;; yh-sql.el ends here
