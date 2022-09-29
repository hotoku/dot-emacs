;;; yh.el --- personal utility functions -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(require 'package)
(require 'dired)
(require 'hideshow)


;;; indent buffer
(defun yh/indent-buffer ()
  "Indent whole buffer."
  (indent-region (point-min) (point-max)))


;;; other-window
(defun yh/other-window-or-split ()
  "Move to another window.  If the frame has only 1 window, split first."
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))


;;; dired
(defun yh/dired-do-open (&optional arg)
  "In dired, invoke /usr/bin/open on the marked files.
If no files are marked or a specific numeric prefix arg is given,
the next ARG files are used.  Just \\[universal-argument] means the current file."
  (interactive "P")
  (let ((files (dired-get-marked-files nil arg)))
    (apply 'start-process "open_ps" nil "open" files)))

(defun yh/dired ()
  "Open current directory in dired."
  (interactive)
  (if (derived-mode-p 'dired-mode) (dired-jump)
    (dired default-directory)))


;;; insert date, time
(defun yh/insert-date ()
  "Inisert date."
  (interactive)
  (insert (format-time-string "[%Y-%m-%d]" (current-time))))

(defun yh/insert-time ()
  "Inisert time."
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %H:%M:%S]" (current-time))))


;;; hideshow
(defun yh/ret-hs ()
  "Open current block."
  (interactive)
  (if (ignore-errors (hs-already-hidden-p)) (hs-show-block)
    (newline)))


;;; automatically add x permission to file
(defun yh/make-executable ()
  "Make file executable if it begins with a shebang."
  (ignore-errors
    (when (string-equal (buffer-substring 1 3) "#!")
      (set-file-modes (buffer-file-name) #o755))))


;;; insert script
(defun yh/insert-script (prefix script)
  "Insert SCRIPT with PREFIX."
  (let ((len (length script)))
    (cond ((= 1 len) (insert (concat prefix script)))
          ((< 1 len) (insert (concat prefix "{" script "}"))))))

(defun yh/insert-subscript (script)
  "Insert SCRIPT."
  (interactive "sscript: ")
  (yh/insert-script "_" script))

(defun yh/insert-superscript (script)
  "Insert SCRIPT."
  (interactive "sscript: ")
  (yh/insert-script "^" script))

;;; refresh package only once in a day
(defun yh/refresh-package ()
  (let* ((filename ".package-refreshed-date")
	 (path (expand-file-name filename user-emacs-directory))
         (today (format-time-string "%Y-%m-%d"))
         (last-date (when (file-exists-p path)
                      (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-substring 1 11))))
         (should-update (or (not last-date)
                            (string< last-date today))))
    
    (when should-update
      (let ((orig-value package-check-signature))
	(setq package-check-signature nil)
	(package-refresh-contents)
	(package-install 'gnu-elpa-keyring-update)
	(setq package-check-signature orig-value))
      (with-temp-buffer
        (insert today)
        (write-region (point-min) (point-max) path)))))

(provide 'yh)
;;; yh.el ends here
