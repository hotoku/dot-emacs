;;; yh-todo.el ---  -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(defgroup yh-todo ()
  "Personal utility managing todo file."
  :group 'tools
  :prefix "yh-todo-")

(defcustom yh-todo-file-path "~/junk/todo.org"
  "Path to todo file."
  :group 'yh-todo
  :type 'string)


;;;###autoload
(defun yh-todo-open ()
  "Open todo file."
  (interactive)
  (find-file yh-todo-file-path))

(provide 'yh-todo)
;;; yh-todo.el ends here
