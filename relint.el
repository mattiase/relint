;;; relint.el --- Elisp regexp mistake finder   -*- lexical-binding: t -*-

;; Author: Mattias Engdegård <mattiase@acm.org>
;; Version: 1.4
;; Package-Requires: ((xr "1.7"))
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
;; Also check the regexp-like-but-not-quite skip-set arguments to
;; `skip-chars-forward' and `skip-chars-backward'.
;;
;; How to use:
;;
;; * Inside Emacs:
;;
;;   M-x relint-file       (check a single elisp file)
;;   M-x relint-directory  (check all .el files in a directory tree)
;;
;; * From batch mode:
;;
;;   emacs -batch -l relint.el -f relint-batch FILES-AND-DIRS...
;;
;; Since there is no sure way to know whether a particular string is a
;; regexp, the code has to guess a lot, and will likely miss quite a
;; few. It tries to minimise the amount of false positives.
;;
;; In other words, it is a nothing but a hack.

;;; Code:

(require 'xr)

(defconst relint--error-buffer-name "*relint*")

(defun relint--error-buffer ()
  (let ((buf (get-buffer relint--error-buffer-name)))
    (or buf
        (let ((buf (get-buffer-create relint--error-buffer-name)))
          (with-current-buffer buf
            (compilation-mode))
          buf))))

(defvar relint--error-count)

(defun relint--add-to-error-buffer (string)
  (with-current-buffer (relint--error-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert string))))

;; Compute (LINE . COLUMN) from POS (toplevel position)
;; and PATH (reversed list of list indices to follow to target).
(defun relint--line-col-from-pos-path (pos path)
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
    (mapc (lambda (msg) (relint--report file pos path msg))
          complaints)))

(defun relint--check-skip-set (skip-set-string name file pos path)
  (relint--check-string skip-set-string #'xr-skip-set-lint name file pos path))

(defun relint--check-re-string (re name file pos path)
  (relint--check-string re #'xr-lint name file pos path))
  
;; Alist of variable definitions seen so far.
;; The variable names map to unevaluated forms.
(defvar relint--variables)

;; List of variables that have been checked, so that we can avoid
;; checking direct uses of it.
(defvar relint--checked-variables)

;; Alist of functions taking regexp argument(s).
;; The names map to a list of the regexp argument indices.
(defvar relint--regexp-functions)

;; List of possibly safe functions defined in the current file, each
;; element on the form (FUNCTION ARGS BODY), where ARGS is the lambda list
;; and BODY its single body expression.
(defvar relint--function-defs)

;; List of possibly safe macros defined in the current file, each
;; element on the form (MACRO ARGS BODY), where ARGS is the lambda list
;; and BODY its single body expression.
(defvar relint--macro-defs)

;; Functions that are safe to call during evaluation.
;; Except for altering the match state, these are pure.
;; More functions could be added if there is evidence that it would
;; help in evaluating more regexp strings.
(defconst relint--safe-functions
  '(cons list append
    concat
    car cdr caar cadr cdar cddr car-safe cdr-safe nth nthcdr
    format format-message
    regexp-quote regexp-opt regexp-opt-charset
    reverse
    member memq remove remq member-ignore-case
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
    string-match split-string replace-regexp-in-string
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
    assoc-default member-ignore-case alist-get
    last butlast number-sequence
    plist-get plist-member
    1value
    consp atom stringp symbolp listp nlistp booleanp
    integerp numberp natnump fixnump bignump characterp zerop
    sequencep vectorp arrayp
    + - * / % mod 1+ 1- max min < <= = > >= /= abs))

;; Alist mapping non-safe functions to semantically equivalent safe
;; alternatives.
(defconst relint--safe-alternatives
  '((nconc . append)
    (delete . remove)
    (delq . remq)
    (nreverse . reverse)
    (nbutlast . butlast)))

;; Make an `rx' form safe to translate, by mutating (eval ...) subforms.
(defun relint--rx-safe (form)
  (cond
   ((atom form) t)
   ((eq (car form) 'eval)
    (let ((arg (relint--eval (cadr form))))
      (and (stringp arg)
           (setcar (cdr form) arg))))    ; Avoid double work.
   ;; Avoid traversing impure lists like (?A . ?Z).
   ((memq (car form) '(any in char not-char)) t)
   (t (not (memq nil (mapcar #'relint--rx-safe (cdr form)))))))

(define-error 'relint--eval-error "relint expression evaluation error")

;; Evaluate an `rx-to-string' expression if safe.
(defun relint--eval-rx (args)
  (if (relint--rx-safe (car args))
      (condition-case err
          (apply #'rx-to-string args)
        (error (signal 'relint--eval-error (format "rx error: %s" (cadr err)))))
    (throw 'relint-eval 'no-value)))

;; Bind FORMALS to ACTUALS and evaluate EXPR.
(defun relint--apply (formals actuals expr)
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

;; A function that fails when called.
(defun relint--no-value (&rest _)
  (throw 'relint-eval 'no-value))

;; Transform an evaluated function (typically a symbol or lambda expr)
;; into something that can be called safely.
(defun relint--wrap-function (form)
  (cond
   ((symbolp form)
    (if (memq form relint--safe-functions)
        form
      (let ((alt (cdr (assq form relint--safe-alternatives))))
        (if alt
            alt
          (let ((def (cdr (assq form relint--function-defs))))
            (if def
                (let ((formals (car def))
                      (expr (cadr def)))
                  (lambda (&rest args)
                    (relint--apply formals args expr)))
              'relint--no-value))))))
   ((and (consp form) (eq (car form) 'lambda))
    (let ((formals (cadr form))
          (body (cddr form)))
      (if (= (length body) 1)
          (lambda (&rest args)
            (relint--apply formals args (car body)))
        'relint--no-value)))
   (t 'relint--no-value)))

;; Evaluate a form. Throw 'relint-eval 'no-value if something could
;; not be evaluated safely.
(defun relint--eval (form)
  (cond
   ((memq form '(nil t)) form)
   ((symbolp form)
    (and form
         (let ((binding (assq form relint--variables)))
           (if binding
               (relint--eval (cdr binding))
             (throw 'relint-eval 'no-value)))))
   ((atom form)
    form)

   ((eq (car form) 'quote)
    (if (and (consp (cadr form))
             (eq (caadr form) '\,))     ; In case we are inside a backquote.
        (throw 'relint-eval 'no-value)
      (cadr form)))
   ((eq (car form) 'function)
    (cadr form))
   ((eq (car form) 'lambda)
    form)
   ((eq (car form) 'eval-when-compile)
    (relint--eval (car (last form))))

   ;; Reasonably pure functions: only call if all args can be fully evaluated.
   ((memq (car form) relint--safe-functions)
    (let ((args (mapcar #'relint--eval (cdr form))))
      ;; Catching all errors isn't wonderful, but sometimes a global
      ;; variable argument has an unsuitable default value which is supposed
      ;; to have been changed at the expression point.
      (condition-case nil
          (apply (car form) args)
        (error (throw 'relint-eval 'no-value)))))

   ;; Locally defined functions: try evaluating.
   ((assq (car form) relint--function-defs)
    (let ((args (mapcar #'relint--eval (cdr form))))
      (let* ((fn (cdr (assq (car form) relint--function-defs)))
             (formals (car fn))
             (expr (cadr fn)))
        (relint--apply formals args expr))))

   ;; Locally defined macros: try expanding.
   ((assq (car form) relint--macro-defs)
    (let ((args (cdr form)))
      (let* ((macro (cdr (assq (car form) relint--macro-defs)))
             (formals (car macro))
             (expr (cadr macro)))
        (relint--eval (relint--apply formals args expr)))))

   ;; replace-regexp-in-string: wrap the rep argument if it's a function.
   ((eq (car form) 'replace-regexp-in-string)
    (let ((all-args (mapcar #'relint--eval (cdr form))))
      (let* ((rep-arg (cadr all-args))
             (rep (if (stringp rep-arg)
                      rep-arg
                    (relint--wrap-function rep-arg)))
             (args (append (list (car all-args) rep) (cddr all-args))))
        (condition-case nil
            (apply (car form) args)
          (error (throw 'relint-eval 'no-value))))))

   ((eq (car form) 'if)
    (let ((condition (relint--eval (cadr form))))
      (let ((then-part (nth 2 form))
            (else-tail (nthcdr 3 form)))
        (cond (condition
               (relint--eval then-part))
              ((and else-tail (cdr else-tail))
               ;; Ignore multi-expression else bodies
               (throw 'relint-eval 'no-value))
              (else-tail
               (relint--eval (car else-tail)))))))

   ((eq (car form) 'and)
    (if (cdr form)
        (let ((val (relint--eval (cadr form))))
          (if (and val (cddr form))
              (relint--eval (cons 'and (cddr form)))
            val))
      t))

   ((eq (car form) 'or)
    (if (cdr form)
        (let ((val (relint--eval (cadr form))))
          (if (and (not val) (cddr form))
              (relint--eval (cons 'or (cddr form)))
            val))
      nil))
   
   ((eq (car form) 'cond)
    (and (cdr form)
         (let ((clause (cadr form)))
           (if (consp clause)
               (let ((val (relint--eval (car clause))))
                 (if val
                     (if (cdr clause)
                         (if (= (length (cdr clause)) 1)
                             (relint--eval (cadr clause))
                           ;; Ignore multi-expression clauses
                           (throw 'relint-eval 'no-value))
                       val)
                   (relint--eval (cons 'cond (cddr form)))))
             ;; Syntax error
             (throw 'relint-eval 'no-value)))))

   ((memq (car form) '(progn ignore-errors))
    (cond ((null (cdr form)) nil)
          ((null (cddr form)) (relint--eval (cadr form)))
          (t (throw 'relint-eval 'no-value))))

   ((assq (car form) relint--safe-alternatives)
    (relint--eval (cons (cdr (assq (car form) relint--safe-alternatives))
                        (cdr form))))

   ;; delete-dups: Work on a copy of the argument.
   ((eq (car form) 'delete-dups)
    (let ((arg (relint--eval (cadr form))))
      (delete-dups (copy-sequence arg))))

   ;; FIXME: more macros. Maybe ones from cl?
   ;; If they are useful but expand to impure code, we need to emulate them.
   ((memq (car form) '(when unless \` backquote-list* pcase pcase-let))
    (relint--eval (macroexpand form)))

   ;; apply: Call only if the function is safe and all args evaluated.
   ((eq (car form) 'apply)
    (let ((args (mapcar #'relint--eval (cdr form))))
      (let ((fun (relint--wrap-function (car args))))
        (condition-case err
            (apply #'apply (cons fun (cdr args)))
          (error (signal 'relint--eval-error (format "eval error: %S: %s"
                                                     form err)))))))

   ;; funcall: Call only if the function is safe and all args evaluated.
   ((eq (car form) 'funcall)
    (let ((args (mapcar #'relint--eval (cdr form))))
      (let ((fun (relint--wrap-function (car args))))
        (condition-case err
            (apply fun (cdr args))
          (error (signal 'relint--eval-error (format "eval error: %S: %s"
                                                     form err)))))))

   ;; mapcar, mapcan: Call only if the function is safe.
   ;; The sequence argument may be missing a few arguments that we cannot
   ;; evaluate.
   ((memq (car form) '(mapcar mapcan))
    (let* ((fun (relint--wrap-function (relint--eval (cadr form))))
           (arg (relint--eval-list (caddr form)))
           (seq (if (listp arg)
                    (remq nil arg)
                  arg)))
      (condition-case err
          (funcall (car form) fun seq)
        (error (signal 'relint--eval-error (format "eval error: %S: %s"
                                                   form err))))))

   ;; mapconcat: Call only if the function is safe and all arguments evaluated.
   ((eq (car form) 'mapconcat)
    (let ((fun (relint--wrap-function (relint--eval (cadr form))))
          (args (mapcar #'relint--eval (cddr form))))
      (condition-case err
          (apply (car form) fun args)
        (error (signal 'relint--eval-error (format "eval error: %S: %s"
                                                   form err))))))
          
   ;; sort: accept missing items in a list argument.
   ((eq (car form) 'sort)
    (let* ((arg (relint--eval-list (cadr form)))
           (seq (cond ((listp arg) (remq nil arg))
                      ((sequencep arg) (copy-sequence arg))
                      (arg)))
           (pred (relint--wrap-function (relint--eval (caddr form)))))
      (condition-case err
          (sort seq pred)
        (error (signal 'relint--eval-error (format "eval error: %S: %s"
                                                   form err))))))

   ;; rx, rx-to-string: check for (eval ...) constructs first, then apply.
   ((eq (car form) 'rx)
    (relint--eval-rx (list (cons 'seq (cdr form)) t)))

   ((eq (car form) 'rx-to-string)
    (let ((args (mapcar #'relint--eval (cdr form))))
      (relint--eval-rx args)))

   ;; setq: Ignore its side-effect and just pass on the value (dubious)
   ((eq (car form) 'setq)
    (relint--eval (caddr form)))

   ;; let and let*: do not permit multi-expression bodies, since they
   ;; will contain necessary side-effects that we don't handle.
   ((eq (car form) 'let)
    (unless (= (length form) 3)
      (throw 'relint-eval 'no-value))
    (let ((bindings
           (mapcar (lambda (binding)
                     (if (consp binding)
                         (cons (car binding)
                               (list 'quote (relint--eval (cadr binding))))
                       (cons binding nil)))
                   (cadr form))))
      (let ((relint--variables (append bindings relint--variables)))
        (relint--eval (car (last form))))))

   ;; let*: bind a single variable and recurse.
   ((eq (car form) 'let*)
    (unless (= (length form) 3)
      (throw 'relint-eval 'no-value))
    (let ((bindings (cadr form)))
      (if bindings
          (let* ((binding (car bindings))
                 (relint--variables
                  (cons
                   (if (consp binding)
                       (cons (car binding)
                             (list 'quote (relint--eval (cadr binding))))
                     (cons binding nil))
                   relint--variables)))
            (relint--eval `(let* ,(cdr bindings) ,@(cddr form))))
        (relint--eval (car (last form))))))

   ;; Loose comma: can occur if we unwittingly stumbled into a backquote
   ;; form. Just eval the arg and hope for the best.
   ((eq (car form) '\,)
    (relint--eval (cadr form)))

   ;; featurep: only handle features that we are reasonably sure about,
   ;; to avoid depending too much on the particular host Emacs.
   ((eq (car form) 'featurep)
    (let ((arg (relint--eval (cadr form))))
      (cond ((eq arg 'xemacs) nil)
            ((memq arg '(emacs mule)) t)
            (t (throw 'relint-eval 'no-value)))))

   (t
    ;;(relint--add-to-error-buffer (format "eval rule missing: %S\n" form))
    (throw 'relint-eval 'no-value))))

;; Evaluate FORM. Return nil if something prevents it from being evaluated.
(defun relint--eval-or-nil (form)
  (let ((val (catch 'relint-eval (relint--eval form))))
    (if (eq val 'no-value)
        nil
      val)))

;; Evaluate a form as far as possible, attempting to keep its list structure
;; even if all subexpressions cannot be evaluated. Parts that cannot be
;; evaluated are nil.
(defun relint--eval-list (form)
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

   ((assq (car form) relint--safe-alternatives)
    (relint--eval-list (cons (cdr (assq (car form) relint--safe-alternatives))
                             (cdr form))))

   ((eq (car form) 'delete-dups)
    (let ((arg (relint--eval-list (cadr form))))
      (delete-dups (copy-sequence arg))))

   ((memq (car form) '(purecopy copy-sequence copy-alist))
    (relint--eval-list (cadr form)))

   ((memq (car form) '(\` backquote-list*))
    (relint--eval-list (macroexpand form)))

   (t
    (relint--eval-or-nil form))))

;; Convert something to a list, or nil.
(defun relint--get-list (form file pos path)
  (condition-case err
      (let ((val (relint--eval-list form)))
        (and (consp val) val))
    (relint--eval-error (relint--report file pos path (cdr err))
                        nil)))
  

;; Convert something to a string, or nil.
(defun relint--get-string (form file pos path)
  (condition-case err
      (let ((val (relint--eval-or-nil form)))
        (and (stringp val) val))
    (relint--eval-error (relint--report file pos path (cdr err))
                        nil)))

(defun relint--check-re (form name file pos path)
  (let ((re (relint--get-string form file pos path)))
    (when re
      (relint--check-re-string re name file pos path))))

;; Check a list of regexps.
(defun relint--check-list (form name file pos path)
  ;; Don't use mapc -- mustn't crash on improper lists.
  (let ((l (relint--get-list form file pos path)))
    (while (consp l)
      (when (stringp (car l))
        (relint--check-re-string (car l) name file pos path))
      (setq l (cdr l)))))

;; Check a list of regexps or conses whose car is a regexp.
(defun relint--check-list-any (form name file pos path)
  (mapc (lambda (elem)
          (cond
           ((stringp elem)
            (relint--check-re-string elem name file pos path))
           ((and (consp elem)
                 (stringp (car elem)))
            (relint--check-re-string (car elem) name file pos path))))
        (relint--get-list form file pos path)))

(defun relint--check-font-lock-keywords (form name file pos path)
  (relint--check-list-any form name file pos path))

;; Check regexps in `compilation-error-regexp-alist-alist'
(defun relint--check-compilation-error-regexp-alist-alist
    (form name file pos path)
  (mapc (lambda (elem)
          (if (cadr elem)
              (relint--check-re-string
               (cadr elem)
               (format "%s (%s)" name (car elem))
               file pos path)))
        (relint--get-list form file pos path)))

;; Check a variable on `align-mode-rules-list' format
(defun relint--check-rules-list (form name file pos path)
  (mapc (lambda (rule)
          (when (and (consp rule)
                     (symbolp (car rule)))
            (let* ((rule-name (car rule))
                   (re-form (cdr (assq 'regexp (cdr rule))))
                   (re (relint--get-string re-form file pos path)))
              (when (stringp re)
                (relint--check-re-string 
                 re (format "%s (%s)" name rule-name) file pos path)))))
        (relint--get-list form file pos path)))

;; List of known regexp-generating functions used in EXPR.
;; EXPANDED is a list of expanded functions, to prevent recursion.
(defun relint--regexp-generators (expr expanded)
  (cond
   ((symbolp expr)
    (let ((def (assq expr relint--variables)))
      (and def (relint--regexp-generators (cdr def) expanded))))
   ((atom expr) nil)
   ((memq (car expr) '(regexp-quote regexp-opt regexp-opt-charset
                       rx rx-to-string wildcard-to-regexp))
    (list (car expr)))
   ((memq (car expr) '(looking-at re-search-forward re-search-backward
                       string-match string-match-p looking-back looking-at-p))
    nil)
   ((listp (cdr (last expr)))
    (let ((head (car expr)))
      (append (mapcan (lambda (x) (relint--regexp-generators x expanded))
                      (cdr expr))
              (let ((fun (assq head relint--function-defs)))
                (and fun (not (memq head expanded))
                     (relint--regexp-generators
                      (caddr fun) (cons head expanded)))))))))

(defun relint--check-skip-set-provenance (skip-function form file pos path)
  (let ((reg-gen (relint--regexp-generators form nil)))
    (when reg-gen
      (relint--report file pos path
                      (format "`%s' cannot be used for arguments to `%s'"
                              (car reg-gen) skip-function)))))

(defun relint--check-form-recursively-1 (form file pos path)
  (pcase form
    (`(,(or `defun `defmacro `defsubst)
       ,name ,args . ,body)

     ;; Save the function or macro for possible use.
     (while (or (stringp (car body))
                (and (consp (car body))
                     (memq (caar body) '(interactive declare))))
       (setq body (cdr body)))          ; Skip doc and declarations.
     ;; Only consider functions/macros with single-expression bodies.
     (when (= (length body) 1)
       (let ((entry (list name args (car body))))
         (if (eq (car form) 'defmacro)
             (push entry relint--macro-defs)
           (push entry relint--function-defs))
         ))

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
    (_
     (let ((index 0))
       (while (consp form)
         (when (consp (car form))
           (relint--check-form-recursively-1
            (car form) file pos (cons index path)))
         (setq form (cdr form))
         (setq index (1+ index)))))))

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
     ;; string-trim has another regexp argument (trim, arg 3)
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
    (`(,(or `defvar `defconst `defcustom)
       ,name ,re-arg . ,rest)
     (when (symbolp name)
       (cond
        ((string-match-p (rx (or "-regexp" "-re" "-regex" "-pattern") eos)
                         (symbol-name name))
         (relint--check-re re-arg name file pos (cons 2 path))
         (push name relint--checked-variables))
        ((string-match-p (rx (or (or "-regexps" "-regexes" "-patterns")
                                 (seq (or "-regexp" "-re" "-regex" "-pattern")
                                      "-list"))
                             eos)
                         (symbol-name name))
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
        ((string-match-p (rx (or "-regexp" "-re" "-regex" "-pattern")
                             "-alist" eos)
                         (symbol-name name))
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
         (push name relint--checked-variables))
        )
       (push (cons name re-arg) relint--variables)))
    (`(define-generic-mode ,name ,_ ,_ ,font-lock-list ,auto-mode-list . ,_)
     (let ((origin (format "define-generic-mode %s" name)))
       (relint--check-font-lock-keywords font-lock-list origin
                                         file pos (cons 4 path))
       (relint--check-list auto-mode-list origin file pos (cons 5 path))))
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

;; Read top-level forms from the current buffer.
;; Return a list of (FORM . STARTING-POSITION).
(defun relint--read-buffer (file)
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

(defun relint--single-file (file)
  (let ((errors-before relint--error-count))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert-file-contents file)
      (let ((forms (relint--read-buffer file))
            (case-fold-search nil)
            (relint--variables nil)
            (relint--checked-variables nil)
            (relint--regexp-functions nil)
            (relint--function-defs nil)
            (relint--macro-defs nil)
            )
        (dolist (form forms)
          (relint--check-form-recursively-1 (car form) file (cdr form) nil))
        (dolist (form forms)
          (relint--check-form-recursively-2 (car form) file (cdr form) nil))))
    (when (> relint--error-count errors-before)
      (relint--show-errors))))
        
(defun relint--init (dir)
  (if noninteractive
      (setq relint--error-count 0)
    (with-current-buffer (relint--error-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert ";; -*- compilation -*-\n"))
      (setq default-directory dir)
      (setq relint--error-count 0))))

(defun relint--finish ()
  (unless noninteractive
    (relint--add-to-error-buffer "Finished.\n")
    (let ((errors relint--error-count))
      (message "relint: %d error%s found." errors (if (= errors 1) "" "s")))))

(defun relint--scan-files (files cwd)
  (relint--init cwd)
  (dolist (file files)
    ;;(relint--add-to-error-buffer (format "Scanning %s\n" file))
    (relint--single-file file))
  (relint--finish))

(defun relint--tree-files (dir)
  (directory-files-recursively
   dir (rx bos (not (any ".")) (* anything) ".el" eos)))


;;;###autoload
(defun relint-file (file)
  "Scan FILE, an elisp file, for errors in regexp strings."
  (interactive "fRelint elisp file: ")
  (relint--scan-files (list file) (file-name-directory file)))
        

;;;###autoload
(defun relint-directory (dir)
  "Scan all *.el files in DIR for errors in regexp strings."
  (interactive "DRelint directory: ")
  (relint--scan-files (relint--tree-files dir) dir))


(defun relint-batch ()
  "Scan elisp source files for errors in regex strings.
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
                      default-directory)
  (setq command-line-args-left nil))

(provide 'relint)

;;; relint.el ends here
