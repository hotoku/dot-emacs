;;; yh-fef.el --- format elisp file -*- lexical-binding: t -*-


;;; Commentary:
;; elispバッファを整形するパッケージ。
;; `yh-fef-format-buffer'が唯一のエントリポイント。
;; todo:
;; 現状は、パースする際に空行の連続とコードの連続という区別しかしておらず、
;; 出力を組み立てる際に、出力空白行数を調整している。
;; 本来は、パースする際に、文法の中で大きいスキップなのか小さいスキップを
;; 要素として区別しておけば、出力処理がシンプルになる。
;; パースする処理は、典型的なLL(1)パーサーになる。


;;; Code:
(require 'yh)

(defun yh-fef-format-buffer ()
  "Format buffer."
  (interactive)
  (let* ((string-pos (yh-fef-format-from-string
                      (buffer-substring-no-properties (point-min) (point-max))
                      (point)))
         (string (car string-pos))
         (pos (cdr string-pos)))
    (erase-buffer)
    (insert string)
    (goto-char pos)))

(defun yh-fef-format-from-string (string pos)
  "Format program STRING, with cursor position POS."
  (let* ((lines (yh-fef-parse-program string))
         (blocks (yh-fef-split-to-blocks lines)))
    (yh-fef-format-blocks blocks pos)))

(defun yh-fef-format-blocks (blocks pos)
  "Format BLOCKS.  And calculate a new cursor position.
The new position is calculated from POS."
  ;; remove first block if it's a blank lines block.
  (when (yh-fef-line-blank-p (yh-fef-line-at (car blocks) 0))
    (setq
     blocks (cdr blocks)
     pos (max (- pos (yh-fef-size (car blocks)) 1)
              1)))
  (let* ((transformed (yh-fef-transform-blanks blocks))
         (index-pos (yh-fef-find-cursor-position blocks pos))
         (index (car index-pos))
         (pos2 (cdr index-pos))
         (sizes (mapcar 'yh-fef-size transformed))
         (sizes1 (mapcar '1+ sizes))
         (sizes2 (yh-fef-cumsum sizes1))
         (sizes3 (cons 0 sizes2))
         (pos3 (min pos2 (seq-elt sizes1 index)))
         (new-pos (+ pos3 (seq-elt sizes3 index)))
         (string (yh-fef-blocks-to-string transformed)))
    (cons string new-pos)))

(defun yh-fef-transform-blanks (blocks)
  "Transform block of blank lies in BLOCKS.
If a block is of blank lines and precedes the section header block,
it will be converted to 2 blank lines, otherwise, to 1 blank lines.
The first block should be code block and the subsequent block must
alternate between blank and code blocks."
  (let ((i 0)
        (ret ()))
    (while blocks
      (if (= (% i 2) 0)
          (setq ret (cons (car blocks) ret))
        (if (cdr blocks)
            (let* ((next-block (cadr blocks))
                   (type (yh-fef-line-type (yh-fef-line-at next-block 0))))
              (if (eq type 'section-header)
                  (setq ret (cons (yh-fef-blank-lines 2) ret))
                (setq ret (cons (yh-fef-blank-lines 1) ret))))
          (setq ret (cons (yh-fef-blank-lines 1) ret))))
      (setq i (1+ i)
            blocks (cdr blocks)))
    (seq-reverse ret)))

(defun yh-fef-find-cursor-position (blocks pos)
  "Find cursor POS in BLOCKS.  Return cons of index and pos.
Cursor is pos-th character of index-th block."
  (let* ((sizes (mapcar 'yh-fef-size blocks))
         (sizes2 (mapcar '1+ sizes))
         (i 0))
    (while (and sizes2 (< (car sizes2) pos))
      (setq pos (- pos (car sizes2))
            i (1+ i)
            sizes2 (cdr sizes2)))
    (cons i pos)))

(defun yh-fef-split-to-blocks (lines)
  "Split LINES into blocks."
  (let ((ret))
    (while lines
      (let ((block-rest (yh-fef-read-block lines)))
        (setq lines (cdr block-rest)
              ret (cons (car block-rest) ret))))
    (reverse ret)))

(defun yh-fef-read-block (lines)
  "Read leading blanks or codes from LINES."
  (when lines
    (let* ((l1 (car lines))
           (t1 (yh-fef-line-type l1))
           (pred (if (eq t1 'blank) 'yh-fef-line-blank-p
                   #'(lambda (x) (not (yh-fef-line-blank-p x)))))
           (block (yh-fef-block (yh-fef-take-while lines pred)))
           (rest (yh-fef-drop-while lines pred)))
      (cons block rest))))

(defun yh-fef-parse-program (string)
  "Parse program code represented by STRING."
  (let ((splitted (split-string string "\n")))
    (mapcar 'yh-fef-parse-line splitted)))


;;; parsers
(defun yh-fef-parse-line (string)
  "Parse 1 line from STRING."
  (let ((blank-line (rx string-start (zero-or-more blank) string-end))
        (section-header (rx string-start ";;;" (zero-or-more any) string-end))
        (subsection-header (rx string-start ";;" (zero-or-more any) string-end)))
    (cond ((string-match blank-line string) (yh-fef-line 'blank string))
          ((string-match section-header string) (yh-fef-line 'section-header string))
          ((string-match subsection-header string) (yh-fef-line 'subsection-header string))
          (t (yh-fef-line 'code string)))))

(defun yh-fef-blocks-to-string (blocks)
  "Convert BLOCKS to string."
  (mapconcat 'yh-fef-block-to-string blocks "\n"))


;;; transformers
(defun yh-fef-block-to-string (block)
  "Concat lines in BLOCK with \\n."
  (let ((lines (yh-fef-lines block)))
    (mapconcat 'yh-fef-line-value lines "\n")))

(defun yh-fef-line-at (block n)
  "Return N th line of BLOCK."
  (seq-elt (yh-fef-lines block) n))

(defun yh-fef-line-value (line)
  "Get value of LINE."
  (alist-get ':value (yh-fef-data line)))

(defun yh-fef-line-type (line)
  "Get type of LINE."
  (alist-get ':type (yh-fef-data line)))

(defun yh-fef-lines (block)
  "Get lines of BLOCK."
  (alist-get ':lines (yh-fef-data block)))

(defun yh-fef-length (block)
  "Get number of lines of this BLOCK."
  (alist-get ':length (yh-fef-data block)))

(defun yh-fef-size (obj)
  "Get number of characters for representing this OBJ."
  (alist-get ':size (yh-fef-data obj)))

(defun yh-fef-data (obj)
  "Get data of OBJ."
  (cdr obj))


;;; accessors
(defun yh-fef-label (obj)
  "Get label of OBJ."
  (car obj))

(defun yh-fef-code-block-p (block)
  "Is BLOCK of code lines ?"
  (not (yh-fef-blank-block-p block)))

(defun yh-fef-blank-block-p (block)
  "Is BLOCK of blank lines ?"
  (eq (yh-fef-line-type (yh-fef-line-at block 0)) 'blank))

(defun yh-fef-line-blank-p (line)
  "Is LINE blank ?"
  (eq (yh-fef-line-type line) 'blank))

(defun yh-fef-block-p (obj)
  "Is OBJ block ?"
  (and (listp obj)
       (eq (car obj) 'block)))


;;; predicates
(defun yh-fef-line-p (obj)
  "Is OBJ line ?"
  (and (listp obj)
       (eq (car obj) 'line)))

(defun yh-fef-blank-lines (n)
  "Construct a block of N blank lines."
  (let ((ret ()))
    (while (< 0 n)
      (setq n (1- n)
            ret (cons (yh-fef-parse-line "") ret)))
    (yh-fef-block ret)))

(defun yh-fef-block (lines)
  "Construct block from LINES."
  (let ((size (apply '+ (mapcar 'yh-fef-size lines)))
        (length (seq-length lines)))
    `(block
      (:size . ,(+ size (- length 1)))
      (:length . ,length)
      (:lines . ,lines))))


;;; constructors
(defun yh-fef-line (type s)
  "Construct line of type TYPE and value S."
  (let ((length (seq-length s)))
    `(line
      (:type . ,type)
      (:value . ,s)
      (:size . ,length))))

(defun yh-fef-cumsum (ls)
  "Cumulative sum of LS."
  (when ls
    (let ((ret ())
          (s 0))
      (while ls
        (setq s (+ s (car ls))
              ret (cons s ret)
              ls (cdr ls)))
      (seq-reverse ret))))

(defun yh-fef-drop-while (ls pred)
  "Drop leading elements from LS that satisfies PRED."
  (and ls
       (if (funcall pred (car ls))
           (yh-fef-drop-while (cdr ls) pred)
         ls)))

(defun yh-fef-take-while (ls pred)
  "Take leading elements from LS that satisfies PRED."
  (and ls
       (if (funcall pred (car ls))
           (cons (car ls) (yh-fef-take-while (cdr ls) pred))
         nil)))


;;; list utils
(defun yh-fef-mapcar* (f &rest xs)
  "Apply F on each value of XS."
  (when (not (memq nil xs))
    (cons (apply f (mapcar 'car xs))
          (apply 'yh-fef-mapcar* f (mapcar 'cdr xs)))))

(provide 'yh-fef)
;;; yh-fef.el ends here
