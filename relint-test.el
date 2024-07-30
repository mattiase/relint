;;; relint-test.el --- Tests for relint.el        -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Mattias Engdegård <mattiase@acm.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'relint)
(require 'ert)

;; Required for some of the source in test/
(require 'subr-x)

(defconst relint-test--this-file
  (or load-file-name buffer-file-name))

;; A nonsense test value to exercise the location code.
(defconst relint-test--value
  '(t1 t2
    13 -7 14.0 1.0e+INF "ab\"\\" ?a ?? ?\\ ?\( ?\)
    [xy 4 uv]
    (t3 (((t4) t5) (t6))
        ('t7 't8 '(t9 () (()))))
    (r . s)
    (t10 t11 . t12)
    #'zz
    '''qqq
    `(t13 ,t14 ,(t15 t16) ,@t17 ,@(t18 t19))))

(defun relint-test--enumerate-nodes (form path)
  "List of (NODE . PATH-TO-NODE) for each node in FORM, starting at PATH.
A node is either an atom or a list, but not a proper tail of a list."
  (if (consp form)
      (let ((r (list (cons form path)))
            (i 0))
        (while (consp form)
          (let ((node (car form)))
            (setq r (append
                     (reverse (relint-test--enumerate-nodes node (cons i path)))
                     r))
            (setq i (1+ i))
            (setq form (cdr form))))
        (reverse r))
    (list (cons form path))))

(defun relint-test--find-toplevel-form (pred)
  "Find first toplevel form satisfying PRED. Return (FORM . POSITION)."
  (catch 'found
    (while t
      (let* ((pos (point))
             (form (read (current-buffer))))
        (when (funcall pred form)
          (throw 'found (cons form pos)))))))

(ert-deftest relint-find-pos ()
  "Test the mechanism that computes a position from a toplevel-position
and a path."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert-file-contents relint-test--this-file)
    (let* ((form-pos (relint-test--find-toplevel-form
                      (lambda (x) (pcase x
                                    (`(defconst relint-test--value . ,_) t)))))
           (toplevel-form (car form-pos))
           (toplevel-pos (cdr form-pos)))
      ;; We have a toplevel form and position. Enumerate its parts.
      (dolist (item (relint-test--enumerate-nodes toplevel-form nil))
        (let* ((node (car item))
               (path (cdr item))
               (pos (relint--pos-from-start-pos-path toplevel-pos path)))
          ;; Skip sugared items; they cannot be read in isolation.
          (unless (memq node '(quote function \` \, \,@))
            (goto-char pos)
            ;; The position should not be at whitespace or comment.
            (should-not (looking-at (rx (any " \t\n;"))))
            ;; Read what we find at the position to check.
            (let ((thing-here (read (current-buffer))))
              (should (equal thing-here node)))))))))

(defun relint-test--insert-file (file)
  (insert-file-contents
   (expand-file-name file (file-name-directory relint-test--this-file))))

(defun relint-test--scan-file (file)
  "Scan FILE and return the results as a string."
  (with-temp-buffer
    ;; The reference files (*.expected) are kept in the `grave' style,
    ;; to make the test independent of `text-quoting-style'.
    (let ((text-quoting-style 'grave)
          (relint--force-batch-output t)
          (relint-batch-highlight 'caret))
      (relint--buffer (find-file-noselect file t) (current-buffer) t))
    (buffer-string)))

(defun relint-test--read-file (file)
  (with-temp-buffer
    (relint-test--insert-file file)
    (buffer-string)))

;; The scan tests are divided more-or-less arbitarily into chunks
;; instead of having one big file, to make it easier to find errors.

(defmacro relint-test--deftest (basename)
  (let* ((testfile (concat "test/" basename ".elisp"))
         (expected (concat "test/" basename ".expected"))
         (name (intern (concat "relint-check-file-" basename))))
    `(ert-deftest ,name ()
       (should (equal (relint-test--scan-file ,testfile)
                      (relint-test--read-file ,expected))))))

(dolist (f (directory-files
            (concat (file-name-directory relint-test--this-file) "/test")
            nil (rx ".elisp" eos)))
  (let ((base (string-remove-suffix ".elisp" f)))
    (eval `(relint-test--deftest ,base))))

(ert-deftest relint-buffer ()
  (let ((buf (get-buffer-create " *relint-test*"))
        (text-quoting-style 'grave))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (emacs-lisp-mode)
            (insert ";hello\n(looking-at \"broken**regexp\")\n")
            (insert "(looking-at (make-string 2 ?^))\n")
            (insert "(looking-at (concat \"ab\" \"cdef\" \"[gg]\"))\n")
            (insert "(string-match \"[xy\" s)\n"))
          (should
           (equal
            (relint-buffer buf)
            '(["In call to looking-at: Repetition of repetition"
               20 28 28 "broken**regexp" 7 7 warning]
              ["This is the inner expression"
               20 26 27 "broken**regexp" 5 6 info]
              ["In call to looking-at: Unescaped literal `^'"
               50 nil nil "^^" 1 1 warning]
              ["In call to looking-at: Duplicated `g' inside character alternative"
               82 105 105 "abcdef[gg]" 8 8 warning]
              ["Previous occurrence here" 82 104 104 "abcdef[gg]" 7 7 info]
              ["In call to string-match: Unterminated character alternative"
               125 126 128 "[xy" 0 2 error]))))
      (kill-buffer buf))))

(ert-deftest relint-buffer-huge ()
  ;; Regression test for regexp stack overflow in the scan for ineffective
  ;; backslashes.
  (should (equal
           (with-temp-buffer
             (emacs-lisp-mode)
             (insert "(message \"hello\\? anyone?\")\n")
             (insert "(defconst my-const '(")
             (dotimes (i 200000)
               (insert (format "%d " i)))
             (insert "))\n")
             (insert "(message \"goodbye\\! everyone!\")\n")
             (let ((text-quoting-style 'grave))
               (relint-buffer (current-buffer))))
           '(["Ineffective string escape `\\?'" nil 16 17 nil nil nil warning]
             ["Ineffective string escape `\\!'" nil 1288960 1288961 nil nil nil
              warning]))))

(ert-deftest relint-bad-hex-escape ()
  ;; Test the bad \x character escape warning. We do this separately because
  ;; it signals an error in newer Emacs.
  (let ((buf (get-buffer-create " *relint-test*"))
        (text-quoting-style 'grave))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (emacs-lisp-mode)
            (insert "(print \"c \\xf \\xg \\x d\")\n"))
          (let* ((diags (relint-buffer buf))
                 (err (condition-case err
                          (read-from-string "?\\x")
                        (error (prin1-to-string err)))))
            (should (equal
                     ;; Ignore 'invalid escape char syntax' error.
                     (remove (assoc err diags) diags)
                     '(["Character escape `\\x' not followed by hex digit"
                        nil 15 16  nil nil nil warning]
                       ["Character escape `\\x' not followed by hex digit"
                        nil 19 20 nil nil nil warning]
                       )))))
      (kill-buffer buf))))

(ert-deftest relint-xr-checks ()
  ;; Test optional checks enabled with `relint-xr-checks'.
  (dolist (checks '(nil all))
    (let* ((relint-xr-checks checks)
           (warnings
            (with-temp-buffer
              (emacs-lisp-mode)
              (insert (format "(looking-at %S)\n"
                              "\\(:?xy\\)+"))
              (let ((text-quoting-style 'grave))
                (relint-buffer (current-buffer)))))
           (msg
            "In call to looking-at: Possibly mistyped `:?' at start of group"))
      (should (equal warnings
                     (and checks
                          `([,msg 13 17 18 "\\(:?xy\\)+" 2 3 warning])))))))

(defun relint-test--batch (prog)
  (with-temp-buffer
    (let ((errbuf (current-buffer)))
      (let ((progbuf (get-buffer-create "relint--test.el")))
        (unwind-protect
            (with-current-buffer progbuf
              (insert prog)
              (emacs-lisp-mode)
              (let ((text-quoting-style 'grave)
                    (relint--force-batch-output t))
                (relint--buffer (current-buffer) errbuf t)))
          (kill-buffer progbuf)))
      (buffer-string))))

(ert-deftest relint-batch-highlight ()
  (let ((prog "(looking-at \"[pqrf-az]\")\n")
        (msg (concat "relint--test.el:1:18-20: "
                     "In call to looking-at: "
                     "Reversed range `f-a' matches nothing (pos 4..6)\n")))
    (let ((relint-batch-highlight nil))
      (should (equal (relint-test--batch prog)
                     (concat msg
                             "  \"[pqrf-az]\"\n"))))
    (let ((relint-batch-highlight 'caret))
      (should (equal (relint-test--batch prog)
                     (concat msg
                             "  \"[pqrf-az]\"\n"
                             "   ....^^^\n"))))
    (let ((relint-batch-highlight '("{" . "}")))
      (should (equal (relint-test--batch prog)
                     (concat msg
                             "  \"[pqr{f-a}z]\"\n"))))))

(provide 'relint-test)
