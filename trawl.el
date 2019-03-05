;;; trawl.el --- Scan elisp files for regexp errors -*- lexical-binding: t -*-

;; Author: Mattias Engdegård <mattiase@acm.org>
;; Version: 1.0
;; Package-Requires: ((xr "1.4"))
;; Keywords: lisp, maint, regexps

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

;;; Commentary:

;; Scan one or more elisp files for potential regexp strings and
;; reports potential errors in them, using `xr-lint' from the `xr'
;; package.
;;
;; To use:  M-x trawl-file       (check a single elisp file)
;;      or  M-x trawl-directory  (check all .el files in a directory tree)
;;
;; Since there is no sure way to know whether a particular string is a
;; regexp, the code has to guess a lot, and will likely miss quite a
;; few. It looks at calls to known functions with regexp arguments,
;; and at variables with regexp-sounding names.
;;
;; In other words, it is a nothing but a hack.

;;; Code:

(require 'xr)

(defconst trawl--error-buffer-name "*trawl-catch*")

(defun trawl--error-buffer ()
  (let ((buf (get-buffer trawl--error-buffer-name)))
    (or buf
        (let ((buf (get-buffer-create trawl--error-buffer-name)))
          (with-current-buffer buf
            (compilation-mode))
          buf))))

(defvar trawl--error-count)

(defun trawl--add-to-error-buffer (string)
  (with-current-buffer (trawl--error-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert string))))

;; Compute (LINE . COLUMN) from POS (toplevel position)
;; and PATH (reversed list of list indices to follow to target).
(defun trawl--line-col-from-pos-path (pos path)
  (save-excursion
    (goto-char pos)
    (let ((p (reverse path)))
      (while p
        (when (looking-at (rx (1+ (or blank "\n" "\f"
                                      (seq ";" (0+ nonl))))))
          (goto-char (match-end 0)))
        (let ((skip (car p)))
          (cond
           ((looking-at (rx (any "'`,")))
            (forward-char 1)
            (setq skip (1- skip)))
           ((looking-at (rx "("))
            (forward-char 1)))
          (forward-sexp skip)
          (setq p (cdr p))))
      (when (looking-at (rx (1+ (or blank "\n" "\f"
                                    (seq ";" (0+ nonl))))))
        (goto-char (match-end 0)))
      (cons (line-number-at-pos (point) t)
            (1+ (current-column))))))

(defun trawl--report (file pos path message)
  (let ((line-col (trawl--line-col-from-pos-path pos path)))
    (trawl--add-to-error-buffer
     (format "%s:%d:%d: %s\n" file (car line-col) (cdr line-col) message)))
  (setq trawl--error-count (1+ trawl--error-count)))

(defun trawl--quote-string (str)
  (concat "\""
          (replace-regexp-in-string
           (rx (any cntrl "\177-\377" ?\\ ?\"))
           (lambda (s)
             (let ((c (logand (string-to-char s) #xff)))
               (or (cdr (assq c
                              '((?\" . "\\\"")
                                (?\\ . "\\\\")
                                (?\b . "\\b")
                                (?\t . "\\t")
                                (?\n . "\\n")
                                (?\v . "\\v")
                                (?\f . "\\f")
                                (?\r . "\\r")
                                (?\e . "\\e"))))
                   (format "\\%03o" c))))
           str t t)
          "\""))

(defun trawl--check-re-string (re name file pos path)
  (let ((complaints
         (condition-case err
	     (mapcar (lambda (warning)
                       (format "In %s: %s (pos %d): %s"
                               name (cdr warning) (car warning)
			       (trawl--quote-string re)))
		     (xr-lint re))
	   (error (list (format "In %s: Error: %s: %s"
				name  (cadr err)
                                (trawl--quote-string re)))))))
    (mapc (lambda (msg) (trawl--report file pos path msg))
          complaints)))
  
;; Alist of variable definitions seen so far.
(defvar trawl--variables)

;; List of variables that have been checked, so that we can avoid
;; checking direct uses of it.
(defvar trawl--checked-variables)

(defun trawl--remove-comma (form)
  (cond
   ((not (consp form)) form)
   ((eq (car form) '\,) (trawl--remove-comma (cadr form)))
   (t
    (cons (trawl--remove-comma (car form))
          (trawl--remove-comma (cdr form))))))

;; Return a value peeled of irrelevancies.
(defun trawl--peel (form)
  (cond
   ((and form (symbolp form))
    (let ((val (cdr (assq form trawl--variables))))
      (and val (trawl--peel val))))
   ((not (consp form)) form)
   ((eq (car form) 'list)
    (trawl--peel (cdr form)))
   ((memq (car form) '(quote purecopy))
    (trawl--peel (cadr form)))
   ((eq (car form) 'eval-when-compile)
    (trawl--peel (car (last form))))
   ((eq (car form) '\`)
    (trawl--peel (trawl--remove-comma (cadr form))))
   (t form)))

;; A list peeled of irrelevancies, or nil.
(defun trawl--peel-list (form)
  (let ((peeled (trawl--peel form)))
    (and (consp peeled) peeled)))

;; Convert something to a list of strings, or nil.
(defun trawl--get-string-list (form)
  (let ((parts (mapcar #'trawl--get-string (trawl--peel-list form))))
    (if (memq nil parts)
        nil
      parts)))

;; Convert something to a string, or nil.
(defun trawl--get-string (form)
  (setq form (trawl--peel form))
  (cond
   ((stringp form) form)
   ((not (consp form)) nil)
   ((eq (car form) 'concat)
    (let ((parts (trawl--get-string-list (cdr form))))
      (and parts (apply #'concat parts))))
   ((eq (car form) 'regexp-opt)
    (let ((arg (trawl--get-string-list (cadr form))))
      (and arg (regexp-opt arg))))
   ((eq (car form) 'regexp-quote)
    (let ((arg (trawl--get-string (cadr form))))
      (and arg (regexp-quote arg))))))

(defun trawl--check-re (form name file pos path)
  (let ((re (trawl--get-string form)))
    (when re
      (trawl--check-re-string re name file pos path))))

(defun trawl--check-list (form name file pos path)
  (mapc (lambda (elem) (trawl--check-re-string elem name file pos path))
        (trawl--get-string-list form)))

(defun trawl--check-list-car (form name file pos path)
  (mapc (lambda (elem)
          (cond
           ((not (consp elem)))
           ((eq (car elem) 'cons)
            (trawl--check-re (cadr elem) name file pos path))
           (t
            (trawl--check-re (car elem) name file pos path))))
        (trawl--peel-list form)))

(defun trawl--check-font-lock-keywords (form name file pos path)
  (mapc (lambda (elem)
          (let* ((thing (trawl--peel elem))
                 (str (trawl--get-string thing)))
            (cond (str
                   (trawl--check-re-string str name file pos path))
                  ((eq (car thing) 'cons)
                   (trawl--check-re (cadr thing) name file pos path))
                  ((consp thing)
                   (trawl--check-re (car thing) name file pos path)))))
        (trawl--peel-list form)))

(defun trawl--check-compilation-error-regexp-alist-alist
    (form name file pos path)
  (mapc (lambda (elem)
	  (trawl--check-re
           (cadr elem)
           (format "%s (%s)" name (car elem))
           file pos path))
        (trawl--peel-list form)))

(defun trawl--check-rules-list (form name file pos path)
  (mapc (lambda (rule)
          (when (and (consp rule)
                     (symbolp (car rule)))
            (let* ((rule-name (car rule))
                   (re-form (cdr (assq 'regexp (cdr rule))))
                   (re (trawl--get-string re-form)))
              (when (stringp re)
                (trawl--check-re-string 
                 re (format "%s (%s)" name rule-name) file pos path)))))
        (trawl--peel-list form)))

(defun trawl--check-form-recursively (form file pos path)
  (when (consp form)
    (pcase form
;      (`(apply ,(or `nconc `(quote nconc) `(function nconc)) (mapcar . ,_))
;       (trawl--report file pos path
;                     "use mapcan instead of (apply nconc (mapcar...))"))
;      (`(lambda (,var1) (,_ ,var2))
;       (when (eq var1 var2)
;        (trawl--report file pos path
;                       "lambda expression can be η-reduced")))
;      (`(lambda (,var1) ,var2)
;       (when (eq var1 var2)
;	 (trawl--report file pos path
;			"lambda expression is #'identity")))
;      (`(defun ,name ,_ . ,body)
;       (let ((f body))
;         (while (and f (consp (car f)) (eq (caar f) 'declare))
;           (setq f (cdr f)))
;         (when (and f (consp (car f)))
;           (setq f (cdr f))
;           (while (cdr f)
;             (when (stringp (car f))
;               (trawl--report file pos path
;			      (format "defun %s: misplaced doc string" name)))
;             (setq f (cdr f))))))
      (`(,(or `looking-at `re-search-forward `re-search-backward
              `string-match `string-match-p `looking-back `looking-at-p
              `replace-regexp-in-string 'replace-regexp
              `query-replace-regexp
              `posix-looking-at `posix-search-backward `posix-search-forward
              `posix-string-match)
         ,re-arg . ,_)
       (unless (and (symbolp re-arg)
                    (memq re-arg trawl--checked-variables))
         (trawl--check-re re-arg (format "call to %s" (car form))
                          file pos (cons 1 path))))
      (`(,(or `defvar `defconst 'defcustom)
	 ,name ,re-arg . ,rest)
       (when (symbolp name)
         (cond
          ((string-match-p (rx (or "-regexp" "-re" "-regex" "-pattern") eos)
			   (symbol-name name))
	   (trawl--check-re re-arg name file pos (cons 2 path))
           (push name trawl--checked-variables))
          ((string-match-p (rx (or "-regexps" "-regexes" "-patterns") eos)
			   (symbol-name name))
           (trawl--check-list re-arg name file pos (cons 2 path))
           (push name trawl--checked-variables))
          ((string-match-p (rx "-font-lock-keywords" eos)
                           (symbol-name name))
           (trawl--check-font-lock-keywords re-arg name file pos (cons 2 path))
           (push name trawl--checked-variables))
          ((eq name 'compilation-error-regexp-alist-alist)
           (trawl--check-compilation-error-regexp-alist-alist
            re-arg name file pos (cons 2 path))
           (push name trawl--checked-variables))
          ((string-match-p (rx (or "-regexp" "-re" "-regex" "-pattern")
                               "-alist" eos)
                           (symbol-name name))
           (trawl--check-list-car re-arg name file pos (cons 2 path))
           (push name trawl--checked-variables))
          ((string-match-p (rx "-rules-list" eos)
                           (symbol-name name))
           (trawl--check-rules-list re-arg name file pos (cons 2 path))
           (push name trawl--checked-variables))
          ;; Doc string starting with "regexp"?
          ((and (stringp (car rest))
                (let ((case-fold-search t))
                  (string-match-p (rx bos "regexp") (car rest))))
	   (trawl--check-re re-arg name file pos (cons 2 path))
           (push name trawl--checked-variables))
          )
         (push (cons name re-arg) trawl--variables)))
      (`(define-generic-mode ,name ,_ ,_ ,font-lock-list ,auto-mode-list . ,_)
       (let ((origin (format "define-generic-mode %s" name)))
         (trawl--check-font-lock-keywords font-lock-list origin
                                          file pos (cons 4 path))
         (trawl--check-list auto-mode-list origin file pos (cons 5 path))))
      )
    (let ((index 0))
      (while (consp form)
        (trawl--check-form-recursively (car form) file pos (cons index path))
        (setq form (cdr form))
        (setq index (1+ index))))))

(defun trawl--check-toplevel-form (form file pos)
  (trawl--check-form-recursively form file pos nil))
                      
(defun trawl--show-errors ()
  (let ((pop-up-windows t))
    (display-buffer (trawl--error-buffer))
    (sit-for 0)))

(defun trawl--single-file (file)
  (let ((errors-before trawl--error-count))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((pos nil)
            (read-circle nil)
            (trawl--variables nil)
            (trawl--checked-variables nil))
        (condition-case err
            (while t
              (setq pos (point))
              (let ((form (read (current-buffer))))
                (trawl--check-toplevel-form form file pos)))
          (end-of-file nil)
          (error (trawl--report file pos nil (prin1-to-string err))))))
    (when (> trawl--error-count errors-before)
      (trawl--show-errors))))
        
(defun trawl--init (file-or-dir dir)
  (with-current-buffer (trawl--error-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format ";; Trawling %s  -*- compilation -*-\n" file-or-dir)))
    (setq trawl--error-count 0)
    (cd dir)))

(defun trawl--finish ()
  (trawl--add-to-error-buffer "Finished.\n")
  (let ((errors trawl--error-count))
    (message "trawl: %d error%s found." errors (if (= errors 1) "" "s"))))


;;;###autoload
(defun trawl-file (file)
  "Scan FILE, an elisp file, for errors in regexp strings."
  (interactive "fTrawl elisp file: ")
  (trawl--init file (file-name-directory file))
  (trawl--single-file file)
  (trawl--finish))
        

;;;###autoload
(defun trawl-directory (dir)
  "Scan all *.el files in DIR for errors in regexp strings."
  (interactive "DTrawl directory: ")
  (trawl--init dir dir)
  (dolist (file (directory-files-recursively
                 dir (rx bos (not (any ".")) (* anything) ".el" eos)))
    (trawl--single-file file))
  (trawl--finish))
