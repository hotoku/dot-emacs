;;; yh-fef-test.el --- test for yh-fef -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(require 'yh-fef)
(require 'ert)


;;; list utils
(ert-deftest yh-fef-test-mapcar* ()
  (should (equal (yh-fef-mapcar* 'cons '(1 2) '(3 4))
                 '((1 . 3) (2 . 4))))
  (should (equal (yh-fef-mapcar* 'cons '(1 2) '(3))
                 '((1 . 3)))))

(ert-deftest yh-fef-test-take-while ()
  (should (equal (yh-fef-take-while
                  '(1 2 3 4 5)
                  '(lambda (x) (< x 3)))
                 '(1 2))))

(ert-deftest yh-fef-test-drop-while ()
  (should (equal (yh-fef-drop-while
                  '(1 2 3 4 5)
                  '(lambda (x) (< x 3)))
                 '(3 4 5))))


;;; constructors
(ert-deftest yh-fef-test-line ()
  (let ((blank (yh-fef-line 'blank "   "))
        (code (yh-fef-line 'code "abc")))
    (should (yh-fef-line-p blank))
    (should (equal (yh-fef-line-type blank) 'blank))
    (should (equal (yh-fef-line-value blank) "   "))
    (should (equal (yh-fef-size blank) 3))
    (should (yh-fef-line-p code))
    (should (equal (yh-fef-line-type code) 'code))
    (should (equal (yh-fef-line-value code) "abc"))
    (should (equal (yh-fef-size code) 3))))

(ert-deftest yh-fef-test-block ()
  (let* ((blank (yh-fef-line 'blank "   "))
         (code (yh-fef-line 'code "abc"))
         (block (yh-fef-block (list blank code))))
    (should (yh-fef-block-p block))
    (should (equal (yh-fef-size block) 7))
    (should (equal (yh-fef-length block) 2))
    (should (equal (yh-fef-lines block) (list blank code)))))

(ert-deftest yh-fef-test-blank-lines ()
  "Should make n blank lines"
  (should (equal (yh-fef-length (yh-fef-blank-lines 2)) 2)))


;;; parsers
(ert-deftest yh-fef-test-parse-line-blank ()
  "Test of parsing blank line."
  (let ((blank (yh-fef-parse-line "   ")))
    (should (equal (yh-fef-line-type blank) 'blank))
    (should (equal (yh-fef-size blank) 3))
    (should (equal (yh-fef-line-value blank) "   "))))

(ert-deftest yh-fef-test-parse-line-section-header ()
  "Test of parsing section header."
  (let ((line (yh-fef-parse-line ";;; comment")))
    (should (equal (yh-fef-line-type line) 'section-header))
    (should (equal (yh-fef-size line) 11))
    (should (equal (yh-fef-line-value line) ";;; comment"))))

(ert-deftest yh-fef-test-parse-line-subsection-header ()
  "Test of parsing section header."
  (let ((line (yh-fef-parse-line ";; comment")))
    (should (equal (yh-fef-line-type line) 'subsection-header))
    (should (equal (yh-fef-size line) 10))
    (should (equal (yh-fef-line-value line) ";; comment"))))

(ert-deftest yh-fef-test-parse-line-code ()
  "Test of parsing section header."
  (let ((line (yh-fef-parse-line "abc")))
    (should (equal (yh-fef-line-type line) 'code))
    (should (equal (yh-fef-size line) 3))
    (should (equal (yh-fef-line-value line) "abc"))))

(ert-deftest yh-fef-test-parse-program ()
  "Test of parsing program."
  (let* ((program (mapconcat 'identity '(";;; comment"
                                         ";; comment2"
                                         ""
                                         ""
                                         "abc") "\n"))
         (lines (yh-fef-parse-program program)))
    (should (equal (seq-length lines) 5))
    (should (equal (mapcar 'yh-fef-line-type lines)
                   '(section-header subsection-header blank blank code)))))

(ert-deftest yh-fef-test-read-block-blank ()
  "Test of leading blank lines"
  (let* ((lines (list (yh-fef-parse-line "")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "abc")))
         (block-rest (yh-fef-read-block lines))
         (block (car block-rest)))
    (should (equal (yh-fef-length block) 2))
    (should (equal (mapcar 'yh-fef-line-type (yh-fef-lines block)) '(blank blank)))))

(ert-deftest yh-fef-test-read-block-code ()
  "Test of leading blank lines"
  (let* ((lines (list (yh-fef-parse-line ";;; abc")
                      (yh-fef-parse-line ";; def")
                      (yh-fef-parse-line "ghi")
                      (yh-fef-parse-line "")))
         (block-rest (yh-fef-read-block lines))
         (block (car block-rest)))
    (should (equal (yh-fef-length block) 3))
    (should (equal (mapcar 'yh-fef-line-type (yh-fef-lines block))
                   '(section-header subsection-header code)))))

(ert-deftest yh-fef-test-split-to-blocks ()
  "Test of leading blank lines"
  (let* ((lines (list (yh-fef-parse-line "abc")
                      (yh-fef-parse-line "def")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "ghi")
                      (yh-fef-parse-line "jkl")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "mno")))
         (blocks (yh-fef-split-to-blocks lines)))
    (should (equal (seq-length blocks) 5))
    (should (equal (mapcar 'yh-fef-length blocks) '(2 1 2 1 1)))))

(ert-deftest yh-fef-test-find-cursor-position ()
  "Test find cursor position"
  (let* ((lines (list (yh-fef-parse-line "abc")
                      (yh-fef-parse-line "def")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "ghi")
                      (yh-fef-parse-line "jkl")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "mno")))
         (blocks (yh-fef-split-to-blocks lines)))
    (should (equal (yh-fef-find-cursor-position blocks 1) '(0 . 1)))
    (should (equal (yh-fef-find-cursor-position blocks 5) '(0 . 5)))
    (should (equal (yh-fef-find-cursor-position blocks 9) '(1 . 1)))
    (should (equal (yh-fef-find-cursor-position blocks 10) '(2 . 1)))
    (should (equal (yh-fef-find-cursor-position blocks 18) '(3 . 1)))
    (should (equal (yh-fef-find-cursor-position blocks 19) '(4 . 1)))))

(ert-deftest yh-fef-test-transform-blanks ()
  "Should transform blanks."
  (let* ((lines (list (yh-fef-parse-line "abc")
                      (yh-fef-parse-line "def")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line ";;; ghi")
                      (yh-fef-parse-line "jkl")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "mno")))
         (blocks (yh-fef-split-to-blocks lines))
         (converted (yh-fef-transform-blanks blocks)))
    (should (equal (seq-length converted) 5))
    (should (equal (mapcar 'yh-fef-length converted) '(2 2 2 1 1)))))

(ert-deftest yh-fef-test-format-blocks ()
  (let* ((lines (list (yh-fef-parse-line "abc")
                      (yh-fef-parse-line "def")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line ";;; ghi")
                      (yh-fef-parse-line "jkl")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "")
                      (yh-fef-parse-line "mno")))
         (blocks (yh-fef-split-to-blocks lines))
         (ret1 (yh-fef-format-blocks blocks 1))
         (ret9 (yh-fef-format-blocks blocks 9))
         (ret11 (yh-fef-format-blocks blocks 11))
         (ret12 (yh-fef-format-blocks blocks 12))
         (ret28 (yh-fef-format-blocks blocks 28))
         (exp (mapconcat 'identity '("abc"
                                     "def"
                                     ""
                                     ""
                                     ";;; ghi"
                                     "jkl"
                                     ""
                                     "mno") "\n")))
    (should (equal (car ret1) exp))
    (should (equal (cdr ret1) 1))
    (should (equal (car ret9) exp))
    (should (equal (cdr ret9) 9))
    (should (equal (car ret11) exp))
    (should (equal (cdr ret11) 10))
    (should (equal (car ret12) exp))
    (should (equal (cdr ret12) 11))
    (should (equal (car ret28) exp))
    (should (equal (cdr ret28) 24))))

;; (provide 'yh-fef-test)
;;; yh-fef-test.el ends here
