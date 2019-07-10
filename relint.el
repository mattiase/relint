;;; relint.el --- Elisp regexp mistake finder   -*- lexical-binding: t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Mattias Engdegård <mattiase@acm.org>
;; Version: 1.8
;; Package-Requires: ((xr "1.12"))
;; URL: https://github.com/mattiase/relint
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

;; Scan elisp files for regexp strings and reports potential errors,
;; including deprecated syntax and bad practice.
;; Also check the regexp-like skip-set arguments to
;; `skip-chars-forward' and `skip-chars-backward'.
;;
;; How to use:
;;
;; * Inside Emacs:
;;
;;   M-x relint-file            (check a single elisp file)
;;   M-x relint-directory       (check all .el files in a directory tree)
;;   M-x relint-current-buffer  (check current buffer)
;;
;;   In the `*relint*' buffer, pressing "g" will re-run the same check.
;;
;; * From batch mode:
;;
;;   emacs -batch -l relint.el -f relint-batch FILES-AND-DIRS...
;;
;;   where options for finding relint and xr need to be added after
;;   `-batch', either `-f package-initialize' or `-L DIR'.

;; Bugs:
;;
;;   Since there is no sure way to know whether a particular string is a
;;   regexp, the code has to guess a lot, and will likely miss quite a
;;   few. It tries to minimise the amount of false positives.
;;   In other words, it is a nothing but a hack.

;;; News:

;; Version 1.8:
;; - Updated diagnostics list
;; - Requires xr 1.12
;; Version 1.7:
;; - Expanded regexp-generating heuristics
;; - Some `defalias' are now followed
;; - All diagnostics are now documented (see README.org)
;; Version 1.6:
;; - Add `relint-current-buffer'
;; - Show relative file names in *relint*
;; - Extended regexp-generating heuristics, warning about suspiciously-named
;;   variables used as skip-sets
;; - "-patterns" and "-pattern-list" are no longer interesting variable
;;   suffixes
;; Version 1.5:
;; - Substantially improved evaluator, able to evaluate some functions and
;;   macros defined in the same file, even when passed as parameters
;; - Detect regexps spliced into [...]
;; - Check bad skip-set provenance
;; - The *relint* buffer now uses a new relint-mode for better usability,
;;   with "g" bound to `relint-again'
;; Version 1.4:
;; - First version after name change to `relint'

;;; Code:

(require 'xr)
(require 'compile)
(require 'cl-lib)

(defconst relint--error-buffer-name "*relint*")

(defun relint--error-buffer ()
  (let ((buf (get-buffer relint--error-buffer-name)))
    (or buf
        (let ((buf (get-buffer-create relint--error-buffer-name)))
          (with-current-buffer buf
            (relint-mode))
          buf))))

(defvar relint--error-count)

(defun relint--add-to-error-buffer (string)
  (with-current-buffer (relint--error-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert string))))

(defun relint--skip-whitespace ()
  (when (looking-at (rx (1+ (or blank "\n" "\f"
                                (seq ";" (0+ nonl))))))
    (goto-char (match-end 0))))

(defun relint--line-col-from-pos-path (pos path)
  "Compute (LINE . COLUMN) from POS (toplevel position)
and PATH (reversed list of list indices to follow to target)."
  (save-excursion
    (goto-char pos)
    (let ((p (reverse path)))
      (while p
        (relint--skip-whitespace)
        (let ((skip (car p)))
          ;; Enter next sexp and skip past the `skip' first sexps inside.
          (cond
           ((looking-at (rx (or "'" "#'" "`" "," ",@")))
            (goto-char (match-end 0))
            (setq skip (1- skip)))
           ((looking-at (rx "("))
            (forward-char 1)))
          (while (> skip 0)
            (relint--skip-whitespace)
            (if (looking-at (rx "."))
                (progn
                  (goto-char (match-end 0))
                  (relint--skip-whitespace)
                  (cond
                   ((looking-at (rx (or "'" "#'" "`" "," ",@")))
                    ;; Sugar after dot represents one sexp.
                    (goto-char (match-end 0))
                    (setq skip (1- skip)))
                   ((looking-at (rx "("))
                    ;; `. (' represents zero sexps.
                    (goto-char (match-end 0)))))
              (forward-sexp)
              (setq skip (1- skip)))))
        (setq p (cdr p))))
    (relint--skip-whitespace)
    (cons (line-number-at-pos (point) t)
          (1+ (current-column)))))

(defun relint--output-error (string)
  (if noninteractive
      (message "%s" string)
    (relint--add-to-error-buffer (concat string "\n"))))

(defun relint--report (file pos path message)
  (let ((line-col (relint--line-col-from-pos-path pos path)))
    (relint--output-error
     (format "%s:%d:%d: %s" file (car line-col) (cdr line-col) message)))
  (setq relint--error-count (1+ relint--error-count)))

(defun relint--quote-string (str)
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

(defun relint--caret-string (string pos)
  (let ((quoted-pos
         (- (length (relint--quote-string (substring string 0 pos)))
            2)))                        ; Lop off quotes
    (concat (make-string quoted-pos ?.) "^")))

(defun relint--check-string (string checker name file pos path)
  (let ((complaints
         (condition-case err
             (mapcar (lambda (warning)
                       (let ((ofs (car warning)))
                         (format "In %s: %s (pos %d)\n  %s\n   %s"
                                 name (cdr warning) ofs
                                 (relint--quote-string string)
                                 (relint--caret-string string ofs))))
                     (funcall checker string))
           (error (list (format "In %s: Error: %s: %s"
                                name  (cadr err)
                                (relint--quote-string string)))))))
    (dolist (msg complaints)
      (relint--report file pos path msg))))

(defun relint--check-skip-set (skip-set-string name file pos path)
  (relint--check-string skip-set-string #'xr-skip-set-lint name file pos path))

(defun relint--check-re-string (re name file pos path)
  (relint--check-string re #'xr-lint name file pos path))
  
(defvar relint--variables nil
  "Alist of variable definitions seen so far.
 The variable names map to unevaluated forms.")


;; List of variables that have been checked, so that we can avoid
;; checking direct uses of it.
(defvar relint--checked-variables)

;; Alist of functions taking regexp argument(s).
;; The names map to a list of the regexp argument indices.
(defvar relint--regexp-functions)

;; List of functions defined in the current file, each element on the
;; form (FUNCTION ARGS BODY), where ARGS is the lambda list and BODY
;; its body expression list.
(defvar relint--function-defs)

;; List of macros defined in the current file, each element on the
;; form (MACRO ARGS BODY), where ARGS is the lambda list and BODY its
;; body expression list.
(defvar relint--macro-defs)

;; Alist of alias definitions in the current file.
(defvar relint--alias-defs)

(defconst relint--safe-functions
  '(cons list append
    concat
    car cdr caar cadr cdar cddr car-safe cdr-safe nth nthcdr
    format format-message
    regexp-quote regexp-opt regexp-opt-charset
    reverse
    member memq memql remove remq member-ignore-case
    assoc assq rassoc rassq
    identity
    string make-string make-list
    substring
    length safe-length
    symbol-name
    null not
    eq eql equal
    string-equal string= string< string-lessp string> string-greaterp
    char-equal string-match-p
    string-match split-string
    wildcard-to-regexp
    combine-and-quote-strings split-string-and-unquote
    string-to-multibyte string-as-multibyte string-to-unibyte string-as-unibyte
    string-join string-trim-left string-trim-right string-trim
    string-prefix-p string-suffix-p
    string-blank-p string-remove-prefix string-remove-suffix
    vector aref elt vconcat
    char-to-string string-to-char
    number-to-string string-to-number int-to-string
    string-to-list string-to-vector string-or-null-p
    upcase downcase capitalize
    purecopy copy-sequence copy-alist copy-tree
    member-ignore-case
    last butlast number-sequence
    plist-get plist-member
    1value
    consp atom stringp symbolp listp nlistp booleanp
    integerp numberp natnump fixnump bignump characterp zerop
    sequencep vectorp arrayp
    + - * / % mod 1+ 1- max min < <= = > >= /= abs)
  "Functions that are safe to call during evaluation.
Except for altering the match state, these are side-effect-free
and reasonably pure (some depend on variables in fairly uninteresting ways,
like `case-fold-search').
More functions could be added if there is evidence that it would
help in evaluating more regexp strings.")

(defconst relint--safe-alternatives
  '((nconc    . append)
    (delete   . remove)
    (delq     . remq)
    (nreverse . reverse)
    (nbutlast . butlast))
"Alist mapping non-safe functions to semantically equivalent safe
alternatives.")

(defconst relint--safe-cl-alternatives
  '((cl-delete-duplicates . cl-remove-duplicates)
    (cl-delete            . cl-remove)
    (cl-delete-if         . cl-remove-if)
    (cl-delete-if-not     . cl-remove-if-not)
    (cl-nsubstitute       . cl-substitute)
    (cl-nunion            . cl-union)
    (cl-nintersection     . cl-intersection)
    (cl-nset-difference   . cl-set-difference)
    (cl-nset-exclusive-or . cl-set-exclusive-or)
    (cl-nsublis           . cl-sublis))
"Alist mapping non-safe cl functions to semantically equivalent safe
alternatives. They may still require wrapping their function arguments.")

(defun relint--rx-safe (rx)
  "Return RX safe to translate; throw 'relint-eval 'no-value if not."
  (cond
   ((atom rx) rx)
   ;; These cannot contain rx subforms.
   ((memq (car rx) '(any in char not-char not backref
                     syntax not-syntax category))
    rx)
   ;; We ignore the differences in evaluation time between `eval' and
   ;; `regexp', and just use what environment we have.
   ((memq (car rx) '(literal eval regexp regex))
    (let ((arg (relint--eval (cadr rx))))
      (if (stringp arg)
          (list (car rx) arg)
        (throw 'relint-eval 'no-value))))
   (t (cons (car rx) (mapcar #'relint--rx-safe (cdr rx))))))

(define-error 'relint--eval-error "relint expression evaluation error")

(defun relint--eval-rx (args)
  "Evaluate an `rx-to-string' expression."
  (let ((safe-args (cons (relint--rx-safe (car args))
                         (cdr args))))
    (condition-case err
        (apply #'rx-to-string safe-args)
      (error (signal 'relint--eval-error
                     (format "rx error: %s" (cadr err)))))))

(defun relint--apply (formals actuals expr)
  "Bind FORMALS to ACTUALS and evaluate EXPR."
  (let ((bindings nil))
    (while formals
      (cond
       ((eq (car formals) '&rest)
        (push (cons (cadr formals) (list 'quote actuals)) bindings)
        (setq formals nil))
       ((eq (car formals) '&optional)
        (setq formals (cdr formals)))
       (t
        (push (cons (car formals) (list 'quote (car actuals))) bindings)
        (setq formals (cdr formals))
        (setq actuals (cdr actuals)))))
    ;; This results in dynamic binding, but that doesn't matter for our
    ;; purposes.
    (let ((relint--variables (append bindings relint--variables)))
      (relint--eval expr))))

(defun relint--no-value (&rest _)
  "A function that fails when called."
  (throw 'relint-eval 'no-value))

(defun relint--wrap-function (form)
  "Transform an evaluated function (typically a symbol or lambda expr)
into something that can be called safely."
  (cond
   ((symbolp form)
    (if (memq form relint--safe-functions)
        form
      (or (cdr (assq form relint--safe-alternatives))
          (let ((def (cdr (assq form relint--function-defs))))
            (if def
                (let ((formals (car def))
                      (body (cadr def)))
                  (if (= (length body) 1)
                      (lambda (&rest args)
                        (relint--apply formals args (car body)))
                    'relint--no-value))
              'relint--no-value)))))
   ((and (consp form) (eq (car form) 'lambda))
    (let ((formals (cadr form))
          (body (cddr form)))
      (if (= (length body) 1)
          (lambda (&rest args)
            (relint--apply formals args (car body)))
        'relint--no-value)))
   (t 'relint--no-value)))

(defun relint--wrap-cl-keyword-args (args)
  "Wrap the function arguments :test, :test-not, :key in ARGS."
  (let ((test     (plist-get args :test))
        (test-not (plist-get args :test-not))
        (key      (plist-get args :key))
        (ret (copy-sequence args)))
    (when test
      (plist-put ret :test     (relint--wrap-function test)))
    (when test-not
      (plist-put ret :test-not (relint--wrap-function test-not)))
    (when key
      (plist-put ret :key      (relint--wrap-function key)))
    ret))

(defun relint--eval (form)
  "Evaluate a form. Throw 'relint-eval 'no-value if something could
not be evaluated safely."
  (if (atom form)
      (cond
       ((booleanp form) form)
       ((symbolp form)
        (and form
             (let ((binding (assq form relint--variables)))
               (if binding
                   (relint--eval (cdr binding))
                 (throw 'relint-eval 'no-value)))))
       (t form))
    (let ((head (car form))
          (body (cdr form)))
      (cond
       ((eq head 'quote)
        (if (and (consp (car body))
                 (eq (caar body) '\,))     ; In case we are inside a backquote.
            (throw 'relint-eval 'no-value)
          (car body)))
       ((eq head 'function)
        (car body))
       ((eq head 'lambda)
        form)
       ((eq head 'eval-when-compile)
        (relint--eval (car (last body))))

       ;; Functions considered safe.
       ((memq head relint--safe-functions)
        (let ((args (mapcar #'relint--eval body)))
          ;; Catching all errors isn't wonderful, but sometimes a global
          ;; variable argument has an unsuitable default value which is
          ;; supposed to have been changed at the expression point.
          (condition-case nil
              (apply head args)
            (error (throw 'relint-eval 'no-value)))))

       ;; replace-regexp-in-string: wrap the rep argument if it's a function.
       ((eq head 'replace-regexp-in-string)
        (let ((all-args (mapcar #'relint--eval body)))
          (let* ((rep-arg (cadr all-args))
                 (rep (if (stringp rep-arg)
                          rep-arg
                        (relint--wrap-function rep-arg)))
                 (args (append (list (car all-args) rep) (cddr all-args))))
            (condition-case nil
                (apply head args)
              (error (throw 'relint-eval 'no-value))))))

       ;; alist-get: wrap the optional fifth argument (testfn).
       ((eq head 'alist-get)
        (let* ((all-args (mapcar #'relint--eval body))
               (args (if (< (length all-args) 5)
                         all-args
                       (append (butlast all-args (- (length all-args) 4))
                               (list (relint--wrap-function
                                      (nth 4 all-args)))))))
          (condition-case nil
              (apply head args)
            (error (throw 'relint-eval 'no-value)))))

       ((eq head 'if)
        (let ((condition (relint--eval (car body))))
          (let ((then-part (nth 1 body))
                (else-tail (nthcdr 2 body)))
            (cond (condition
                   (relint--eval then-part))
                  ((and else-tail (cdr else-tail))
                   ;; Ignore multi-expression else bodies
                   (throw 'relint-eval 'no-value))
                  (else-tail
                   (relint--eval (car else-tail)))))))

       ((eq head 'and)
        (if body
            (let ((val (relint--eval (car body))))
              (if (and val (cdr body))
                  (relint--eval (cons 'and (cdr body)))
                val))
          t))

       ((eq head 'or)
        (if body
            (let ((val (relint--eval (car body))))
              (if (and (not val) (cdr body))
                  (relint--eval (cons 'or (cdr body)))
                val))
          nil))
       
       ((eq head 'cond)
        (and body
             (let ((clause (car body)))
               (if (consp clause)
                   (let ((val (relint--eval (car clause))))
                     (if val
                         (if (cdr clause)
                             (if (= (length (cdr clause)) 1)
                                 (relint--eval (cadr clause))
                               ;; Ignore multi-expression clauses
                               (throw 'relint-eval 'no-value))
                           val)
                       (relint--eval (cons 'cond (cdr body)))))
                 ;; Syntax error
                 (throw 'relint-eval 'no-value)))))

       ((memq head '(progn ignore-errors))
        (cond ((null body) nil)
              ((null (cdr body)) (relint--eval (car body)))
              (t (throw 'relint-eval 'no-value))))

       ;; delete-dups: Work on a copy of the argument.
       ((eq head 'delete-dups)
        (let ((arg (relint--eval (car body))))
          (delete-dups (copy-sequence arg))))

       ;; Safe macros that expand to pure code, and their auxiliary macros.
       ((memq head '(when unless
                      \` backquote-list*
                      pcase pcase-let pcase-let* pcase--flip))
        (relint--eval (macroexpand form)))

       ;; Functions taking a function as first argument.
       ((memq head '(apply funcall mapconcat
                           cl-some cl-every cl-notany cl-notevery))
        (let ((fun (relint--wrap-function (relint--eval (car body))))
              (args (mapcar #'relint--eval (cdr body))))
          (condition-case nil
              (apply head fun args)
            (error (throw 'relint-eval 'no-value)))))
       
       ;; Functions with functions as keyword arguments :test, :test-not, :key
       ((memq head '(cl-remove-duplicates cl-remove cl-substitute cl-member
                     cl-find cl-position cl-count cl-mismatch cl-search
                     cl-union cl-intersection cl-set-difference
                     cl-set-exclusive-or cl-subsetp
                     cl-assoc cl-rassoc
                     cl-sublis))
        (let ((args (relint--wrap-cl-keyword-args
                     (mapcar #'relint--eval body))))
          (condition-case nil
              (apply head args)
            (error (throw 'relint-eval 'no-value)))))
       
       ;; Functions taking a function as first argument,
       ;; and with functions as keyword arguments :test, :test-not, :key
       ((memq head '(cl-reduce cl-remove-if cl-remove-if-not
                               cl-find-if cl-find-if not
                               cl-position-if cl-position-if-not
                               cl-count-if cl-count-if-not
                               cl-member-if cl-member-if-not
                               cl-assoc-if cl-assoc-if-not
                               cl-rassoc-if cl-rassoc-if-not))
        (let ((fun (relint--wrap-function (relint--eval (car body))))
              (args (relint--wrap-cl-keyword-args
                     (mapcar #'relint--eval (cdr body)))))
          (condition-case nil
              (apply head fun args)
            (error (throw 'relint-eval 'no-value)))))

       ;; mapcar, mapcan: accept missing items in the list argument.
       ((memq head '(mapcar mapcan))
        (let* ((fun (relint--wrap-function (relint--eval (car body))))
               (arg (relint--eval-list (cadr body)))
               (seq (if (listp arg)
                        (remq nil arg)
                      arg)))
          (condition-case nil
              (funcall head fun seq)
            (error (throw 'relint-eval 'no-value)))))

       ;; sort: accept missing items in the list argument.
       ((eq head 'sort)
        (let* ((arg (relint--eval-list (car body)))
               (seq (cond ((listp arg) (remq nil arg))
                          ((sequencep arg) (copy-sequence arg))
                          (arg)))
               (pred (relint--wrap-function (relint--eval (cadr body)))))
          (condition-case nil
              (sort seq pred)
            (error (throw 'relint-eval 'no-value)))))

       ;; rx, rx-to-string: check for lisp expressions in constructs first,
       ;; then apply.
       ((eq head 'rx)
        (relint--eval-rx (list (cons 'seq body) t)))

       ((eq head 'rx-to-string)
        (let ((args (mapcar #'relint--eval body)))
          (relint--eval-rx args)))

       ;; setq: Ignore its side-effect and just pass on the value (dubious)
       ((eq head 'setq)
        (relint--eval (cadr body)))

       ;; let and let*: do not permit multi-expression bodies, since they
       ;; will contain necessary side-effects that we don't handle.
       ((eq head 'let)
        (unless (= (length body) 2)
          (throw 'relint-eval 'no-value))
        (let ((bindings
               (mapcar (lambda (binding)
                         (if (consp binding)
                             (cons (car binding)
                                   (list 'quote (relint--eval (cadr binding))))
                           (cons binding nil)))
                       (car body))))
          (let ((relint--variables (append bindings relint--variables)))
            (relint--eval (car (last body))))))

       ((eq head 'let*)
        (unless (= (length body) 2)
          (throw 'relint-eval 'no-value))
        (let ((bindings (car body)))
          (if bindings
              (let* ((binding (car bindings))
                     (relint--variables
                      (cons
                       (if (consp binding)
                           (cons (car binding)
                                 (list 'quote (relint--eval (cadr binding))))
                         (cons binding nil))
                       relint--variables)))
                (relint--eval `(let* ,(cdr bindings) ,@(cdr body))))
            (relint--eval (car (last body))))))

       ;; Loose comma: can occur if we unwittingly stumbled into a backquote
       ;; form. Just eval the arg and hope for the best.
       ((eq head '\,)
        (relint--eval (car body)))

       ;; functionp: be optimistic, for determinism
       ((eq head 'functionp)
        (let ((arg (relint--eval (car body))))
          (cond
           ((symbolp arg) (not (memq arg '(nil t))))
           ((consp arg) (eq (car arg) 'lambda)))))

       ;; featurep: only handle features that we are reasonably sure about,
       ;; to avoid depending too much on the particular host Emacs.
       ((eq head 'featurep)
        (let ((arg (relint--eval (car body))))
          (cond ((eq arg 'xemacs) nil)
                ((memq arg '(emacs mule)) t)
                (t (throw 'relint-eval 'no-value)))))

       ;; Locally defined functions: try evaluating.
       ((assq head relint--function-defs)
        (let* ((fn (cdr (assq head relint--function-defs)))
               (formals (car fn))
               (fn-body (cadr fn)))
          (if (= (length body) 1)
              (let ((args (mapcar #'relint--eval body)))
                (relint--apply formals args (car fn-body)))
            (throw 'relint-eval 'no-value))))

       ;; Locally defined macros: try expanding.
       ((assq head relint--macro-defs)
        (let ((args body))
          (let* ((macro (cdr (assq head relint--macro-defs)))
                 (formals (car macro))
                 (macro-body (cadr macro)))
            (if (= (length macro-body) 1)
                (relint--eval (relint--apply formals args (car macro-body)))
              (throw 'relint-eval 'no-value)))))

       ;; Alias: substitute and try again.
       ((assq head relint--alias-defs)
        (relint--eval (cons (cdr (assq head relint--alias-defs))
                            body)))

       ((assq head relint--safe-alternatives)
        (relint--eval (cons (cdr (assq head relint--safe-alternatives))
                            body)))

       ((assq head relint--safe-cl-alternatives)
        (relint--eval (cons (cdr (assq head relint--safe-cl-alternatives))
                            body)))
       
       (t
        ;;(relint--add-to-error-buffer (format "eval rule missing: %S\n" form))
        (throw 'relint-eval 'no-value))))))

(defun relint--eval-or-nil (form)
  "Evaluate FORM. Return nil if something prevents it from being evaluated."
  (let ((val (catch 'relint-eval (relint--eval form))))
    (if (eq val 'no-value)
        nil
      val)))

(defun relint--eval-list (form)
  "Evaluate a form as far as possible, attempting to keep its list structure
even if all subexpressions cannot be evaluated. Parts that cannot be
evaluated are nil."
  (cond
   ((symbolp form)
    (and form
         (let ((val (cdr (assq form relint--variables))))
           (and val (relint--eval-list val)))))
   ((atom form)
    form)
   ((eq (car form) 'eval-when-compile)
    (relint--eval-list (car (last form))))

   ;; Pure structure-generating functions: Apply even if we cannot evaluate
   ;; all arguments (they will be nil), because we want a reasonable
   ;; approximation of the structure.
   ((memq (car form) '(list append cons reverse remove remq))
    (apply (car form) (mapcar #'relint--eval-list (cdr form))))

   ((eq (car form) 'delete-dups)
    (let ((arg (relint--eval-list (cadr form))))
      (delete-dups (copy-sequence arg))))

   ((memq (car form) '(purecopy copy-sequence copy-alist))
    (relint--eval-list (cadr form)))

   ((memq (car form) '(\` backquote-list*))
    (relint--eval-list (macroexpand form)))

   ((assq (car form) relint--safe-alternatives)
    (relint--eval-list (cons (cdr (assq (car form) relint--safe-alternatives))
                             (cdr form))))

   (t
    (relint--eval-or-nil form))))

(defun relint--get-list (form file pos path)
  "Convert something to a list, or nil."
  (condition-case err
      (let ((val (relint--eval-list form)))
        (and (consp val) val))
    (relint--eval-error (relint--report file pos path (cdr err))
                        nil)))
  

(defun relint--get-string (form file pos path)
  "Convert something to a string, or nil."
  (condition-case err
      (let ((val (relint--eval-or-nil form)))
        (and (stringp val) val))
    (relint--eval-error (relint--report file pos path (cdr err))
                        nil)))

(defun relint--check-re (form name file pos path)
  (let ((re (relint--get-string form file pos path)))
    (when re
      (relint--check-re-string re name file pos path))))

(defun relint--check-list (form name file pos path)
  "Check a list of regexps."
  ;; Don't use dolist -- mustn't crash on improper lists.
  (let ((l (relint--get-list form file pos path)))
    (while (consp l)
      (when (stringp (car l))
        (relint--check-re-string (car l) name file pos path))
      (setq l (cdr l)))))

(defun relint--check-list-any (form name file pos path)
  "Check a list of regexps or conses whose car is a regexp."
  (dolist (elem (relint--get-list form file pos path))
    (cond
     ((stringp elem)
      (relint--check-re-string elem name file pos path))
     ((and (consp elem)
           (stringp (car elem)))
      (relint--check-re-string (car elem) name file pos path)))))


(defun relint--check-font-lock-keywords (form name file pos path)
  (relint--check-list-any form name file pos path))

(defun relint--check-compilation-error-regexp-alist-alist (form name
                                                           file pos path)
  (dolist (elem (relint--get-list form file pos path))
    (if (cadr elem)
        (relint--check-re-string
         (cadr elem)
         (format "%s (%s)" name (car elem))
         file pos path))))

(defun relint--check-rules-list (form name file pos path)
  "Check a variable on `align-mode-rules-list' format"
  (dolist (rule (relint--get-list form file pos path))
    (when (and (consp rule)
               (symbolp (car rule)))
      (let* ((rule-name (car rule))
             (re-form (cdr (assq 'regexp (cdr rule))))
             (re (relint--get-string re-form file pos path)))
        (when (stringp re)
          (relint--check-re-string 
           re (format "%s (%s)" name rule-name) file pos path))))))

(defun relint--regexp-generators (expr expanded)
  "List of regexp-generating functions and variables used in EXPR.
EXPANDED is a list of expanded functions, to prevent recursion."
  (cond
   ((symbolp expr)
    (and (not (memq expr '(nil t)))
         ;; Check both variable contents and name.
         (or (let ((def (assq expr relint--variables)))
               (and def
                    (relint--regexp-generators (cdr def) expanded)))
             (and (or (memq expr '(page-delimiter paragraph-separate
                                   paragraph-start sentence-end))
                      ;; This is guesswork, but effective.
                      (string-match-p
                       (rx (or (seq bos (or "regexp" "regex"))
                               (or "-regexp" "-regex" "-re"))
                           eos)
                       (symbol-name expr)))
                  (list expr)))))
   ((atom expr) nil)
   ((memq (car expr) '(regexp-quote regexp-opt regexp-opt-charset
                       rx rx-to-string wildcard-to-regexp read-regexp
                       char-fold-to-regexp find-tag-default-as-regexp
                       find-tag-default-as-symbol-regexp sentence-end))
    (list (car expr)))
   ((memq (car expr) '(looking-at re-search-forward re-search-backward
                       string-match string-match-p looking-back looking-at-p))
    nil)
   ((null (cdr (last expr)))
    (let* ((head (car expr))
           (alias (assq head relint--alias-defs)))
      (if alias
          (relint--regexp-generators (cons (cdr alias) (cdr expr)) expanded)
        (append (mapcan (lambda (x) (relint--regexp-generators x expanded))
                        (cdr expr))
                (let ((fun (assq head relint--function-defs)))
                  (and fun (not (memq head expanded))
                       (mapcan (lambda (x)
                                 (relint--regexp-generators
                                  x (cons head expanded)))
                               (caddr fun))))))))))

(defun relint--check-skip-set-provenance (skip-function form file pos path)
  (let ((reg-gen (relint--regexp-generators form nil)))
    (when reg-gen
      (relint--report file pos path
                      (format "`%s' cannot be used for arguments to `%s'"
                              (car reg-gen) skip-function)))))

(defun relint--check-format-mixup (template args file pos path)
  "Look for a format expression that suggests insertion of a regexp
into a character alternative: [%s] where the corresponding format
parameter is regexp-generating."
  (let ((nargs (length args))
        (index 0)
        (start 0))
    (while (and (< index nargs)
                (string-match (rx
                                ;; An unescaped [, and some leading chars
                               (opt (or bos (not (any "\\")))
                                    (0+ "\\\\")
                                    (group "[")
                                    (0+ (not (any "]"))))
                               ;; Any %-sequence
                               "%"
                               (opt (1+ digit) "$")
                               (0+ digit)
                               (opt "." (0+ digit))
                               (group (any "%sdioxXefgcS")))
                              template start))
      (let ((bracket (match-beginning 1))
            (type (string-to-char (match-string 2 template)))
            (next (match-end 0)))
        (when (and bracket (eq type ?s))
          (let ((reg-gen (relint--regexp-generators (nth index args) nil)))
            (when reg-gen
              (relint--report
               file pos (cons (+ index 2) path)
               (format
                "Value from `%s' cannot be spliced into `[...]'"
                (car reg-gen))))))
        (unless (eq type ?%)
          (setq index (1+ index)))
        (setq start next)))))

(defun relint--check-concat-mixup (args file pos path)
  "Look for concat args that suggest insertion of a regexp into a
character alternative: `[' followed by a regexp-generating expression."
  (let ((index 1))
    (while (consp args)
      (let ((arg (car args)))
        (when (and (stringp arg)
                   (cdr args)
                   (string-match-p (rx (or bos (not (any "\\")))
                                       (0+ "\\\\")
                                       "["
                                       (0+ (not (any "]")))
                                       eos)
                                   arg))
          (let ((reg-gen (relint--regexp-generators (cadr args) nil)))
            (when reg-gen
              (relint--report
               file pos (cons (1+ index) path)
               (format
                "Value from `%s' cannot be spliced into `[...]'"
                (car reg-gen)))))))
      (setq index (1+ index))
      (setq args (cdr args)))))

(defun relint--check-form-recursively-1 (form file pos path)
  (pcase form
    (`(,(or `defun `defmacro `defsubst)
       ,name ,args . ,body)

     ;; Save the function or macro for possible use.
     (while (or (stringp (car body))
                (and (consp (car body))
                     (memq (caar body) '(interactive declare))))
       (setq body (cdr body)))          ; Skip doc and declarations.
     (push (list name args body)
           (if (eq (car form) 'defmacro)
               relint--macro-defs
             relint--function-defs))

     ;; If any argument looks like a regexp, remember it so that it can be
     ;; checked in calls.
     (when (consp args)
       (let ((indices nil)
             (index 0))
         (while args
           (let ((arg (car args)))
             (when (symbolp arg)
               (cond
                ((eq arg '&optional))   ; Treat optional args as regular.
                ((eq arg '&rest)
                 (setq args nil))       ; Ignore &rest args.
                (t
                 (when (string-match-p (rx (or (or "regexp" "regex" "-re"
                                                   "pattern")
                                               (seq bos "re"))
                                           eos)
                                       (symbol-name arg))
                   (push index indices))
                 (setq index (1+ index)))))
             (setq args (cdr args))))
         (when indices
           (push (cons name (reverse indices)) relint--regexp-functions)))))
    (`(defalias ,name-arg ,def-arg . ,_)
     (let ((name (relint--eval-or-nil name-arg))
           (def  (relint--eval-or-nil def-arg)))
       (when (and name def)
         (push (cons name def) relint--alias-defs))))
    (_
     (let ((index 0))
       (while (consp form)
         (when (consp (car form))
           (relint--check-form-recursively-1
            (car form) file pos (cons index path)))
         (setq form (cdr form))
         (setq index (1+ index)))))))

(defun relint--check-defcustom-type (type name file pos path)
  (pcase type
    (`(const . ,rest)
     ;; Skip keywords.
     (while (and rest (symbolp (car rest)))
       (setq rest (cddr rest)))
     (when rest
       (relint--check-re (car rest) name file pos path)))
    (`(,(or 'choice 'radio) . ,choices)
     (dolist (choice choices)
       (relint--check-defcustom-type choice name file pos path)))))

(defun relint--check-defcustom-re (form name file pos path)
  (let ((args (nthcdr 4 form))
        (index 5))
    (while (consp args)
      (pcase args
        (`(:type ,type)
         (relint--check-defcustom-type (relint--eval-or-nil type)
                                       name file pos (cons index path)))
        (`(:options ,options)
         (relint--check-list options name file pos (cons index path))))
      (setq index (+ index 2))
      (setq args (cddr args)))))

(defun relint--check-form-recursively-2 (form file pos path)
  (pcase form
    (`(,(or `looking-at `re-search-forward `re-search-backward
            `string-match `string-match-p `looking-back `looking-at-p
            `replace-regexp-in-string `replace-regexp
            `query-replace-regexp
            `posix-looking-at `posix-search-backward `posix-search-forward
            `posix-string-match
            `load-history-filename-element
            `kill-matching-buffers
            `keep-lines `flush-lines `how-many)
       ,re-arg . ,_)
     (unless (and (symbolp re-arg)
                  (memq re-arg relint--checked-variables))
       (relint--check-re re-arg (format "call to %s" (car form))
                         file pos (cons 1 path))))
    (`(,(or `split-string `split-string-and-unquote
            `string-trim-left `string-trim-right `string-trim
            `directory-files-recursively)
       ,_ ,re-arg . ,rest)
     (unless (and (symbolp re-arg)
                  (memq re-arg relint--checked-variables))
       (relint--check-re re-arg (format "call to %s" (car form))
                         file pos (cons 2 path)))
     ;; string-trim has another regexp argument (trim-right, arg 3)
     (when (and (eq (car form) 'string-trim)
                (car rest))
       (let ((right (car rest)))
         (unless (and (symbolp right)
                      (memq right relint--checked-variables))
           (relint--check-re right (format "call to %s" (car form))
                             file pos (cons 3 path)))))
     ;; split-string has another regexp argument (trim, arg 4)
     (when (and (eq (car form) 'split-string)
                (cadr rest))
       (let ((trim (cadr rest)))
         (unless (and (symbolp trim)
                      (memq trim relint--checked-variables))
           (relint--check-re trim (format "call to %s" (car form))
                             file pos (cons 4 path))))))
    (`(,(or `skip-chars-forward `skip-chars-backward)
       ,skip-arg . ,_)
     (let ((str (relint--get-string skip-arg file pos path)))
       (when str
         (relint--check-skip-set str (format "call to %s" (car form))
                                 file pos (cons 1 path))))
     (relint--check-skip-set-provenance
      (car form) skip-arg file pos (cons 1 path))
     )
    (`(concat . ,args)
     (relint--check-concat-mixup args file pos path))
    (`(format ,template-arg . ,args)
     (let ((template (relint--get-string template-arg file pos path)))
       (when template
         (relint--check-format-mixup template args file pos path))))
    (`(,(or `defvar `defconst `defcustom)
       ,name ,re-arg . ,rest)
     (let ((type (and (eq (car form) 'defcustom)
                          (relint--eval-or-nil (plist-get (cdr rest) :type)))))
       (when (symbolp name)
         (cond
          ((or (eq type 'regexp)
               (string-match-p (rx (or "-regexp" "-re" "-regex" "-pattern") eos)
                               (symbol-name name)))
           (relint--check-re re-arg name file pos (cons 2 path))
           (when (eq (car form) 'defcustom)
             (relint--check-defcustom-re form name file pos path))
           (push name relint--checked-variables))
          ((or (equal type '(repeat regexp))
               (string-match-p (rx (or (or "-regexps" "-regexes")
                                       (seq (or "-regexp" "-re" "-regex")
                                            "-list"))
                                   eos)
                               (symbol-name name)))
           (relint--check-list re-arg name file pos (cons 2 path))
           (push name relint--checked-variables))
          ((string-match-p (rx "-font-lock-keywords" eos)
                           (symbol-name name))
           (relint--check-font-lock-keywords re-arg name file pos (cons 2 path))
           (push name relint--checked-variables))
          ((eq name 'compilation-error-regexp-alist-alist)
           (relint--check-compilation-error-regexp-alist-alist
            re-arg name file pos (cons 2 path))
           (push name relint--checked-variables))
          ((or (and (consp type)
                    (eq (car type) 'alist)
                    (eq (plist-get (cdr type) :key-type) 'regexp))
               (string-match-p (rx (or "-regexp" "-re" "-regex" "-pattern")
                                   "-alist" eos)
                               (symbol-name name)))
           (relint--check-list-any re-arg name file pos (cons 2 path))
           (push name relint--checked-variables))
          ((string-match-p (rx "-mode-alist" eos)
                           (symbol-name name))
           (relint--check-list-any re-arg name file pos (cons 2 path))
           (push name relint--checked-variables))
          ((string-match-p (rx "-rules-list" eos)
                           (symbol-name name))
           (relint--check-rules-list re-arg name file pos (cons 2 path))
           (push name relint--checked-variables))
          ;; Doc string starting with "regexp"?
          ((and (stringp (car rest))
                (let ((case-fold-search t))
                  (string-match-p (rx bos "regexp") (car rest))))
           (relint--check-re re-arg name file pos (cons 2 path))
           (when (eq (car form) 'defcustom)
             (relint--check-defcustom-re form name file pos path))
           (push name relint--checked-variables))
          )
         (push (cons name re-arg) relint--variables))))
    (`(define-generic-mode ,name ,_ ,_ ,font-lock-list ,auto-mode-list . ,_)
     (let ((origin (format "define-generic-mode %s" name)))
       (relint--check-font-lock-keywords font-lock-list origin
                                         file pos (cons 4 path))
       (relint--check-list auto-mode-list origin file pos (cons 5 path))))
    (`(,name . ,args)
     (let ((alias (assq name relint--alias-defs)))
       (when alias
         (relint--check-form-recursively-2
          (cons (cdr alias) args) file pos path))))
    )

  ;; Check calls to remembered functions with regexp arguments.
  (when (consp form)
    (let ((indices (cdr (assq (car form) relint--regexp-functions))))
      (when indices
        (let ((index 0)
              (args (cdr form)))
          (while (and indices (consp args))
            (when (= index (car indices))
              (unless (and (symbolp (car args))
                           (memq (car args) relint--checked-variables))
                (relint--check-re (car args) (format "call to %s" (car form))
                                  file pos (cons (1+ index) path)))
              (setq indices (cdr indices)))
            (setq args (cdr args))
            (setq index (1+ index)))))))

  (let ((index 0))
    (while (consp form)
      (when (consp (car form))
        (relint--check-form-recursively-2
         (car form) file pos (cons index path)))
      (setq form (cdr form))
      (setq index (1+ index)))))

(defun relint--show-errors ()
  (unless noninteractive
    (let ((pop-up-windows t))
      (display-buffer (relint--error-buffer))
      (sit-for 0))))

(defun relint--read-buffer (file)
  "Read top-level forms from the current buffer.
Return a list of (FORM . STARTING-POSITION)."
  (goto-char (point-min))
  (let ((pos nil)
        (keep-going t)
        (read-circle nil)
        (forms nil))
    (while keep-going
      (setq pos (point))
      (let ((form nil))
        (condition-case err
            (setq form (read (current-buffer)))
          (end-of-file
           (setq keep-going nil))
          (invalid-read-syntax
           (cond
            ((equal (cadr err) "#")
             (goto-char pos)
             (forward-sexp 1))
            (t
             (relint--report file (point) nil (prin1-to-string err))
             (setq keep-going nil))))
          (error
           (relint--report file (point) nil (prin1-to-string err))
           (setq keep-going nil)))
        (when (consp form)
          (push (cons form pos) forms))))
    (nreverse forms)))

(defun relint--scan-current-buffer (file)
  (let ((errors-before relint--error-count))
    (let ((forms (relint--read-buffer file))
          (relint--variables nil)
          (relint--checked-variables nil)
          (relint--regexp-functions nil)
          (relint--function-defs nil)
          (relint--macro-defs nil)
          (relint--alias-defs nil)
          (case-fold-search nil))
      (dolist (form forms)
        (relint--check-form-recursively-1 (car form) file (cdr form) nil))
      (dolist (form forms)
        (relint--check-form-recursively-2 (car form) file (cdr form) nil)))
    (when (> relint--error-count errors-before)
      (relint--show-errors))))

(defun relint--scan-file (file base-dir)
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert-file-contents file)
    (relint--scan-current-buffer (file-relative-name file base-dir))))
        
(defvar relint-last-target nil
  "The last file, directory or buffer on which relint was run.")

(defun relint--init (target base-dir)
  (if noninteractive
      (setq relint--error-count 0)
    (with-current-buffer (relint--error-buffer)
      (let ((inhibit-read-only t))
        (compilation-forget-errors)
        (erase-buffer)
        (insert (format "Relint results for %s\n" target))
        (relint--show-errors))
      (setq relint-last-target target)
      (setq default-directory base-dir)
      (setq relint--error-count 0))))

(defun relint--finish ()
  (let* ((errors relint--error-count)
         (msg (format "%d error%s" errors (if (= errors 1) "" "s"))))
    (unless noninteractive
      (relint--add-to-error-buffer (format "\nFinished -- %s found.\n" msg)))
    (message "relint: %s found." msg)))

(defun relint-again ()
  "Re-run relint on the same file, directory or buffer as last time."
  (interactive)
  (cond ((bufferp relint-last-target)
         (with-current-buffer relint-last-target
           (relint-current-buffer)))
        ((file-directory-p relint-last-target)
         (relint-directory relint-last-target))
        ((file-readable-p relint-last-target)
         (relint-file relint-last-target))
        (t (error "No target"))))

(defvar relint-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "g" 'relint-again)
    map)
  "Keymap for relint buffers.")

(define-compilation-mode relint-mode "Relint"
  "Mode for relint output."
  (setq-local relint-last-target nil))

(defun relint--scan-files (files target base-dir)
  (relint--init target base-dir)
  (dolist (file files)
    ;;(relint--add-to-error-buffer (format "Scanning %s\n" file))
    (relint--scan-file file base-dir))
  (relint--finish))

(defun relint--tree-files (dir)
  (directory-files-recursively
   dir (rx bos (not (any ".")) (* anything) ".el" eos)))


;;;###autoload
(defun relint-file (file)
  "Scan FILE, an elisp file, for regexp-related errors."
  (interactive "fRelint elisp file: ")
  (relint--scan-files (list file) file (file-name-directory file)))

;;;###autoload
(defun relint-directory (dir)
  "Scan all *.el files in DIR for regexp-related errors."
  (interactive "DRelint directory: ")
  (message "Finding .el files in %s..." dir)
  (let ((files (relint--tree-files dir)))
    (message "Scanning files...")
    (relint--scan-files files dir dir)))

;;;###autoload
(defun relint-current-buffer ()
  "Scan the current buffer for regexp errors.
The buffer must be in emacs-lisp-mode."
  (interactive)
  (unless (eq major-mode 'emacs-lisp-mode)
    (error "Relint: can only scan elisp code (use emacs-lisp-mode)"))
  (relint--init (current-buffer) default-directory)
  (save-excursion
    (relint--scan-current-buffer (buffer-name)))
  (relint--finish))


(defun relint-batch ()
  "Scan elisp source files for regexp-related errors.
Call this function in batch mode with files and directories as
command-line arguments.  Files are scanned; directories are
searched recursively for *.el files to scan."
  (unless noninteractive
    (error "`relint-batch' is only for use with -batch"))
  (relint--scan-files (mapcan (lambda (arg)
                                (if (file-directory-p arg)
                                    (relint--tree-files arg)
                                  (list arg)))
                              command-line-args-left)
                      nil default-directory)
  (setq command-line-args-left nil))

(provide 'relint)

;;; relint.el ends here
