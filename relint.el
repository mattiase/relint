;;; relint.el --- Scan elisp files for regexp errors -*- lexical-binding: t -*-

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

(defconst relint--error-buffer-name "*relint-catch*")

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

(defun relint--check-skip-set (skip-set-string name file pos path)
  (let ((complaints
         (condition-case err
             (mapcar (lambda (warning)
                       (let ((ofs (car warning)))
                         (format "In %s: %s (pos %d)\n  %s\n   %s"
                                 name (cdr warning) ofs
                                 (relint--quote-string skip-set-string)
                                 (relint--caret-string skip-set-string ofs))))
                     (xr-skip-set-lint skip-set-string))
           (error (list (format "In %s: Error: %s: %s"
                                name  (cadr err)
                                (relint--quote-string skip-set-string)))))))
    (mapc (lambda (msg) (relint--report file pos path msg))
          complaints)))

(defun relint--check-re-string (re name file pos path)
  (let ((complaints
         (condition-case err
             (mapcar (lambda (warning)
                       (let ((ofs (car warning)))
                         (format "In %s: %s (pos %d)\n  %s\n   %s"
                                 name (cdr warning) ofs
                                 (relint--quote-string re)
                                 (relint--caret-string re ofs))))
                     (xr-lint re))
           (error (list (format "In %s: Error: %s: %s"
                                name  (cadr err)
                                (relint--quote-string re)))))))
    (mapc (lambda (msg) (relint--report file pos path msg))
          complaints)))
  
;; Alist of variable definitions seen so far.
;; The variable names map to unevaluated forms.
(defvar relint--variables)

;; List of variables that have been checked, so that we can avoid
;; checking direct uses of it.
(defvar relint--checked-variables)

;; Alist of functions taking regexp argument(s).
;; The names map to a list of the regexp argument indices.
(defvar relint--regexp-functions)

;; Functions that are safe to call during evaluation.
;; With some exceptions (noted), these are pure.
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
    string-equal string= string< string-lessp char-equal string-match-p

    ; These alter the match state.
    string-match split-string replace-regexp-in-string

    combine-and-quote-strings split-string-and-unquote
    string-join string-trim-left string-trim-right string-trim
    string-prefix-p string-suffix-p
    string-blank-p string-remove-prefix string-remove-suffix
    vector aref elt vconcat
    char-to-string string-to-char
    number-to-string string-to-number int-to-string
    upcase downcase capitalize
    purecopy copy-sequence copy-alist copy-tree
    assoc-default member-ignore-case alist-get
    last butlast number-sequence
    plist-get plist-member
    consp atom stringp symbolp listp nlistp
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

;; Transform FORM into an expression that is safe to evaluate with the
;; bindings in relint--variables and parameters in PARAMS.
;; Return the transformed expression with known variables substituted away,
;; or 'no-value if safe evaluation could not be guaranteed.
(defun relint--safe-expr (form params)
  (cond
   ((symbolp form)
    (if (or (memq form '(t nil))
            (memq form params))
        form
      (let ((binding (assq form relint--variables)))
        (if binding
            (list 'quote (relint--eval (cdr binding)))
          'no-value))))
   ((atom form) form)                   ; Other atoms considered OK.
   ((eq (car form) 'quote) form)
   (t
    (let* ((fun (relint--safe-function (car form) params))
           (args (mapcar (lambda (x) (relint--safe-expr x params))
                         (cdr form))))
      (if (and fun (not (memq 'no-value args)))
          (cons fun args)
        'no-value)))))

;; Transform F into a function that is safe to pass as a higher-order function
;; in a call. Return the transformed function or nil if safe evaluation
;; could not be guaranteed.
;; PARAMS is a list of parameters that can be assumed to be in scope.
(defun relint--safe-function (f params)
  (cond
   ;; Functions (and some special forms/macros) considered safe.
   ((symbolp f)
    (cond ((or (memq f relint--safe-functions)
               (memq f '(if when unless and or)))
           f)
          ((cdr (assq f relint--safe-alternatives)))))
   ((atom f) nil)
   ((eq (car f) 'function)
    (relint--safe-function (cadr f) params))

   ;; Only permit one-argument one-expression lambdas (for purity),
   ;; where the body only refers to arguments and known variables,
   ;; and calls safe functions.
   ((eq (car f) 'lambda)
    (let ((vars (cadr f))
          (body (cddr f)))
      (and (= (length vars) 1)
           (= (length body) 1)
           (let ((expr (relint--safe-expr (car body) (cons (car vars) params))))
             (and (not (eq expr 'no-value))
                  `(lambda (,(car vars)) ,expr))))))))

;; Whether an `rx' form is safe to translate.
;; Will mutate (eval ...) subforms with their results when possible.
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
    'no-value))

;; Evaluate a form as far as possible. Substructures that cannot be evaluated
;; become `no-value'.
(defun relint--eval (form)
  (cond
   ((memq form '(nil t)) form)
   ((symbolp form)
    (and form
         (let ((binding (assq form relint--variables)))
           (if binding
               (relint--eval (cdr binding))
             'no-value))))
   ((atom form)
    form)
   ((not (symbolp (car form)))
    (relint--add-to-error-buffer (format "eval error: %S" form))
    'no-value)
   ((eq (car form) 'quote)
    (if (and (consp (cadr form))
             (eq (caadr form) '\,))     ; In case we are inside a backquote.
        'no-value
      (cadr form)))
   ((eq (car form) 'function)
    (cadr form))
   ((eq (car form) 'eval-when-compile)
    (relint--eval (car (last form))))
   ((eq (car form) 'lambda)
    form)

   ;; Reasonably pure functions: only call if all args can be fully evaluated.
   ((memq (car form) relint--safe-functions)
    (let ((args (mapcar #'relint--eval (cdr form))))
      (if (memq 'no-value args)
          'no-value
        ;; Catching all errors isn't wonderful, but sometimes a global
        ;; variable argument has an unsuitable default value which is supposed
        ;; to have been changed at the expression point.
        (condition-case nil
            (apply (car form) args)
          (error 'no-value)))))

   ;; replace-regexp-in-string: Only safe if no function given.
   ((eq (car form) 'replace-regexp-in-string)
    (let ((args (mapcar #'relint--eval (cdr form))))
      (if (and (not (memq 'no-value args))
               (stringp (cadr args)))
          (condition-case nil
              (apply (car form) args)
            (error 'no-value))
        'no-value)))

   ;; if, when, unless, and, or: Treat these as functions and eval all args.
   ((memq (car form) '(if when unless and or))
    (let ((args (mapcar #'relint--eval (cdr form))))
      (if (memq 'no-value args)
          'no-value
        ;; eval is safe here: all args are quoted constants.
        (eval (cons (car form)
                    (mapcar (lambda (x) (list 'quote x)) args))))))

   ((assq (car form) relint--safe-alternatives)
    (relint--eval (cons (cdr (assq (car form) relint--safe-alternatives))
                        (cdr form))))

   ;; delete-dups: Work on a copy of the argument.
   ((eq (car form) 'delete-dups)
    (let ((arg (relint--eval (cadr form))))
      (if (eq arg 'no-value)
          'no-value
        (delete-dups (copy-sequence arg)))))

   ((memq (car form) '(\` backquote-list*))
    (relint--eval (macroexpand form)))

   ;; apply: Call only if the function is safe and all args evaluated.
   ((eq (car form) 'apply)
    (let ((args (mapcar #'relint--eval (cdr form))))
      (if (memq 'no-value args)
          'no-value
        (let ((fun (relint--safe-function (car args) nil)))
          (if fun
              (condition-case err
                  (apply #'apply (cons fun (cdr args)))
                (error (signal 'relint--eval-error (format "eval error: %S: %s"
                                                           form err))))
            'no-value)))))

   ;; funcall: Call only if the function is safe and all args evaluated.
   ((eq (car form) 'funcall)
    (let ((args (mapcar #'relint--eval (cdr form))))
      (if (memq 'no-value args)
          'no-value
        (let ((fun (relint--safe-function (car args) nil)))
          (if fun
              (condition-case err
                  (apply fun (cdr args))
                (error (signal 'relint--eval-error (format "eval error: %S: %s"
                                                           form err))))
            'no-value)))))

   ;; mapcar, mapcan: Call only if the function is safe.
   ;; The sequence argument may be missing a few arguments that we cannot
   ;; evaluate.
   ((memq (car form) '(mapcar mapcan))
    (let ((fun (relint--safe-function (relint--eval (cadr form)) nil))
          (seq (remq nil (relint--eval-list (caddr form)))))
      (if fun
          (condition-case err
              (funcall (car form) fun seq)
            (error (signal 'relint--eval-error (format "eval error: %S: %s"
                                                       form err))))
        'no-value)))

   ;; mapconcat: Call only if the function is safe and all arguments evaluated.
   ((eq (car form) 'mapconcat)
    (let ((fun (relint--safe-function (relint--eval (cadr form)) nil))
          (args (mapcar #'relint--eval (cddr form))))
      (if fun
          (if (memq 'no-value args)
              'no-value
            (condition-case err
                (apply (car form) fun args)
              (error (signal 'relint--eval-error (format "eval error: %S: %s"
                                                         form err)))))
        'no-value)))
          
   ;; rx, rx-to-string: check for (eval ...) constructs first, then apply.
   ((eq (car form) 'rx)
    (relint--eval-rx (list (cons 'seq (cdr form)) t)))

   ((eq (car form) 'rx-to-string)
    (let ((args (mapcar #'relint--eval (cdr form))))
      (if (memq 'no-value args)
          'no-value
        (relint--eval-rx args))))

   ;; setq: Ignore its side-effect and just pass on the value.
   ((eq (car form) 'setq)
    (let ((val (relint--eval (caddr form))))
      (if (eq val 'no-value)
          'no-value
        val)))

   ;; let and let*: do not permit multi-expression bodies, since they
   ;; will contain necessary side-effects that we don't handle.
   ((and (eq (car form) 'let)
         (null (cdddr form)))
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
   ((and (eq (car form) 'let*)
         (null (cdddr form)))
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

   ((memq (car form) '(cond)) 'no-value)

   (t
    ;;(relint--add-to-error-buffer (format "eval rule missing: %S\n" form))
    'no-value)))

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
   ((not (symbolp (car form)))
    (relint--add-to-error-buffer (format "eval error: %S\n" form))
    nil)
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
    (let ((arg (relint--eval (cadr form))))
      (if (eq arg 'no-value)
          'no-value
        (delete-dups (copy-sequence arg)))))

   ((memq (car form) '(purecopy copy-sequence copy-alist))
    (relint--eval-list (cadr form)))

   ((memq (car form) '(\` backquote-list*))
    (relint--eval-list (macroexpand form)))

   (t
    (let ((val (relint--eval form)))
      (if (eq val 'no-value) nil val)))))

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
      (let ((val (relint--eval form)))
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

(defun relint--check-form-recursively-1 (form file pos path)
  (pcase form
    (`(,(or `defun `defmacro `defsubst)
       ,name ,args . ,_)
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
                                 file pos (cons 1 path)))))
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
        (relint--check-form-recursively-2 (car form) file pos (cons index path)))
      (setq form (cdr form))
      (setq index (1+ index)))))

(defun relint--show-errors ()
  (unless noninteractive
    (let ((pop-up-windows t))
      (display-buffer (relint--error-buffer))
      (sit-for 0))))

(defun relint--check-buffer (file forms function)
  (dolist (form forms)
    (funcall function (car form) file (cdr form) nil)))

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
            (relint--variables nil)
            (relint--checked-variables nil)
            (relint--regexp-functions nil))
        (relint--check-buffer file forms #'relint--check-form-recursively-1)
        (relint--check-buffer file forms #'relint--check-form-recursively-2)))
    (when (> relint--error-count errors-before)
      (relint--show-errors))))
        
(defun relint--tree (dir &optional excludes)
  (let ((excludes (or excludes "a\\`")))
    (dolist (file (directory-files-recursively
                 dir (rx bos (not (any ".")) (* anything) ".el" eos)))
      ;;(relint--add-to-error-buffer (format "Scanning %s\n" file))
      (unless (string-match excludes file)
	(relint--single-file file)))))

(defun relint--init (file-or-dir dir)
  (unless noninteractive
    (with-current-buffer (relint--error-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format ";; relint %s  -*- compilation -*-\n" file-or-dir)))
      (setq relint--error-count 0)
      (cd dir))))

(defun relint--finish ()
  (relint--add-to-error-buffer "Finished.\n")
  (let ((errors relint--error-count))
    (message "relint: %d error%s found." errors (if (= errors 1) "" "s"))))


;;;###autoload
(defun relint-file (file)
  "Scan FILE, an elisp file, for errors in regexp strings."
  (interactive "fRelint elisp file: ")
  (relint--init file (file-name-directory file))
  (relint--single-file file)
  (relint--finish))
        

;;;###autoload
(defun relint-directory (dir &optional excludes)
  "Scan all *.el files in DIR for errors in regexp strings."
  (interactive "DRelint directory: ")
  (relint--init dir dir)
  (relint--tree dir excludes)
  (relint--finish))


(defun relint-batch ()
  "Scan elisp source files for errors in regex strings.
Call this function in batch mode with files and directories as
command-line arguments.  Files are scanned; directories are
searched recursively for *.el files to scan."
  (unless noninteractive
    (error "`relint-batch' is to be used only with -batch"))
  (setq relint--error-count 0)
  (while command-line-args-left
    (let ((arg (pop command-line-args-left)))
      (if (file-directory-p arg)
          (relint--tree arg)
        (relint--single-file arg)))))

(provide 'relint)

;;; relint.el ends here
