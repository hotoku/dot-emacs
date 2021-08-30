;;; yh-font.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defun yh-font-use-huge-font (arg)
  "Change font size.  When ARG, the setting is saved for future session."
  (interactive "P")
  (yh-font-set-font "Monospace-20" arg))

(defun yh-font-use-large-font (arg)
  "Change font size.  When ARG, the setting is saved for future session."
  (interactive "P")
  (yh-font-set-font "Monospace-16" arg))

(defun yh-font-use-medium-font (arg)
  "Change font size.  When ARG, the setting is saved for future session."
  (interactive "P")
  (yh-font-set-font "Monospace-14" arg))

(defun yh-font-use-small-font (arg)
  "Change font size.  When ARG, the setting is saved for future session."
  (interactive "P")
  (yh-font-set-font "Monospace-12" arg))

(defun yh-font-set-font (font arg)
  "Actually set FONT.  If ARG is non nil, the setting is perpetuated."
  (when arg (yh-font-perpetuate font))
  (setf (alist-get 'font default-frame-alist) font)
  (yh-font-recreate-frame))

(defconst yh-font-config-file  (expand-file-name ".yh-font" user-emacs-directory))

(defun yh-font-perpetuate (font)
  "Write value of FONT in config file."
  (save-excursion
    (let* ((obj `((font . ,font)))
           (content (format "%S" obj))
           (buf (find-file yh-font-config-file)))
      (kill-region (point-min) (point-max))
      (insert content)
      (save-buffer buf)
      (kill-buffer buf))))

(defun yh-font-initialize ()
  "Read conofig file and appliy it."
  (when (file-exists-p yh-font-config-file)
    (save-excursion
      (let* ((buf (find-file yh-font-config-file))
             (obj (read (buffer-substring (point-min) (point-max))))
             (font (cdr (assoc 'font obj))))
        (kill-buffer buf)
        (yh-font-set-font font nil)))))

(defun yh-font-recreate-frame (&optional w h)
  "Recreate frame.  W, H are frame size (width and height) in pixel."
  (let* ((f (selected-frame))
         (width (or w (frame-pixel-width f)))
         (height (or h (frame-pixel-height f))))
    (make-frame `((width . (text-pixels . ,width))
                  (height . (text-pixels . ,height))))
    (delete-frame f)))

(provide 'yh-font)
;;; yh-font.el ends here
