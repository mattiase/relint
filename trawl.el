;;; trawl.el --- Scan elisp files for regexp errors -*- lexical-binding: t -*-

;; Author: Mattias Engdegård <mattiase@acm.org>
;; Version: 1.1
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
;; It can also be used from batch mode by calling `trawl-batch' with
;; files and/or directories as command-line arguments, errors going
;; to stderr:
;;
;;  emacs -batch -l trawl.el -f trawl-batch FILES-AND-DIRS...
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

(defun trawl--output-error (string)
  (if noninteractive
      (message "%s" string)
    (trawl--add-to-error-buffer (concat string "\n"))))

(defun trawl--report (file pos path message)
  (let ((line-col (trawl--line-col-from-pos-path pos path)))
    (trawl--output-error
     (format "%s:%d:%d: %s" file (car line-col) (cdr line-col) message)))
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

(defun trawl--caret-string (string pos)
  (let ((quoted-pos
         (- (length (trawl--quote-string (substring string 0 pos)))
            2)))                        ; Lop off quotes
    (concat (make-string quoted-pos ?.) "^")))

(defun trawl--check-re-string (re name file pos path)
  (let ((complaints
         (condition-case err
             (mapcar (lambda (warning)
                       (let ((pos (car warning)))
                         (format "In %s: %s (pos %d)\n  %s\n   %s"
                                 name (cdr warning) pos
                                 (trawl--quote-string re)
                                 (trawl--caret-string re pos))))
                     (xr-lint re))
           (error (list (format "In %s: Error: %s: %s"
                                name  (cadr err)
                                (trawl--quote-string re)))))))
    (mapc (lambda (msg) (trawl--report file pos path msg))
          complaints)))
  
;; Alist of variable definitions seen so far.
;; The variable names map to unevaluated forms.
(defvar trawl--variables)

;; List of variables that have been checked, so that we can avoid
;; checking direct uses of it.
(defvar trawl--checked-variables)

;; Alist of functions taking regexp argument(s).
;; The names map to a list of the regexp argument indices.
(defvar trawl--regexp-functions)

;; Transform FORM into an expression that is safe to evaluate with the
;; bindings in trawl--variables and parameters in PARAMS.
;; Return the transformed expression with known variables substituted away,
;; or 'no-value if safe evaluation could not be guaranteed.
(defun trawl--safe-expr (form params)
  (cond
   ((symbolp form)
    (if (or (memq form '(t nil))
            (memq form params))
        form
      (let ((binding (assq form trawl--variables)))
        (if binding
            (list 'quote (trawl--eval (cdr binding)))
          'no-value))))
   ((atom form) form)                   ; Other atoms considered OK.
   ((eq (car form) 'quote) form)
   (t
    (let* ((fun (trawl--safe-function (car form) params))
           (args (mapcar (lambda (x) (trawl--safe-expr x params))
                         (cdr form))))
      (if (and fun (not (memq 'no-value args)))
          (cons fun args)
        'no-value)))))

;; Transform F into a function that is safe to pass as a higher-order function
;; in a call. Return the transformed function or nil if safe evaluation
;; could not be guaranteed.
;; PARAMS is a list of parameters that can be assumed to be in scope.
(defun trawl--safe-function (f params)
  (cond
   ;; Functions (and some special forms/macros) considered safe.
   ((symbolp f)
    (and (or (and (get f 'side-effect-free)
                  (not (eq f 'symbol-value)))
             (memq f '(caar cadr cdar cddr purecopy remove remq
                       if unless when and or
                       regexp-opt regexp-opt-charset)))
         f))
   ((atom f) nil)
   ((eq (car f) 'function)
    (trawl--safe-function (cadr f) params))

   ;; Only permit one-argument one-expression lambdas (for purity),
   ;; where the body only refers to arguments and known variables,
   ;; and calls safe functions.
   ((eq (car f) 'lambda)
    (let ((vars (cadr f))
          (body (cddr f)))
      (and (= (length vars) 1)
           (= (length body) 1)
           (let ((expr (trawl--safe-expr (car body) (cons (car vars) params))))
             (and (not (eq expr 'no-value))
                  `(lambda (,(car vars)) ,expr))))))))

;; Whether an `rx' form is safe to translate.
;; Will mutate (eval ...) subforms with their results when possible.
(defun trawl--rx-safe (form)
  (cond
   ((atom form) t)
   ((eq (car form) 'eval)
    (let ((arg (trawl--eval (cadr form))))
      (and (stringp arg)
           (setcar (cdr form) arg))))    ; Avoid double work.
   ;; Avoid traversing impure lists like (?A . ?Z).
   ((memq (car form) '(any in char not-char)) t)
   (t (not (memq nil (mapcar #'trawl--rx-safe (cdr form)))))))

(define-error 'trawl--eval-error "trawl expression evaluation error")

;; Evaluate an `rx-to-string' expression if safe.
(defun trawl--eval-rx (args)
  (if (trawl--rx-safe (car args))
      (condition-case err
          (apply #'rx-to-string args)
        (error (signal 'trawl--eval-error (format "rx error: %s" (cadr err)))))
    'no-value))

;; Evaluate a form as far as possible. Substructures that cannot be evaluated
;; become `no-value'.
(defun trawl--eval (form)
  (cond
   ((symbolp form)
    (and form
         (let ((binding (assq form trawl--variables)))
           (if binding
               (trawl--eval (cdr binding))
             'no-value))))
   ((atom form)
    form)
   ((not (symbolp (car form)))
    (trawl--add-to-error-buffer (format "eval error: %S" form))
    'no-value)
   ((eq (car form) 'quote)
    (if (and (consp (cadr form))
             (eq (caadr form) '\,))     ; In case we are inside a backquote.
        'no-value
      (cadr form)))
   ((eq (car form) 'function)
    (cadr form))
   ((eq (car form) 'eval-when-compile)
    (trawl--eval (car (last form))))
   ((eq (car form) 'lambda)
    form)

   ;; Reasonably pure functions: only call if all args can be fully evaluated.
   ((or (and (get (car form) 'side-effect-free)
             ;; Exceptions: there should probably be more.
             ;; Maybe we should just list the ones we believe are safe,
             ;; and not use side-effect-free?
             (not (eq (car form) 'symbol-value)))
        ;; Common functions that aren't marked as side-effect-free.
        (memq (car form) '(caar cadr cdar cddr
                           regexp-opt regexp-opt-charset
                           ;; alters last-coding-system-used
                           decode-coding-string
                           format-message format-spec
                           purecopy remove remq
                           ;; alters match state
                           string-match string-match-p)))
    (let ((args (mapcar #'trawl--eval (cdr form))))
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
    (let ((args (mapcar #'trawl--eval (cdr form))))
      (if (and (not (memq 'no-value args))
               (stringp (cadr args)))
          (condition-case nil
              (apply (car form) args)
            (error 'no-value))
        'no-value)))

   ;; if, when, unless, and, or: Treat these as functions and eval all args.
   ((memq (car form) '(if when unless and or))
    (let ((args (mapcar #'trawl--eval (cdr form))))
      (if (memq 'no-value args)
          'no-value
        ;; eval is safe here: all args are quoted constants.
        (eval (cons (car form)
                    (mapcar (lambda (x) (list 'quote x)) args))))))

   ((memq (car form) '(\` backquote-list*))
    (trawl--eval (macroexpand form)))

   ;; apply: Call only if the function is safe and all args evaluated.
   ((eq (car form) 'apply)
    (let ((args (mapcar #'trawl--eval (cdr form))))
      (if (memq 'no-value args)
          'no-value
        (let ((fun (trawl--safe-function (car args) nil)))
          (if fun
              (condition-case err
                  (apply #'apply (cons fun (cdr args)))
                (error (signal 'trawl--eval-error (format "eval error: %S: %s"
                                                          form err))))
            'no-value)))))

   ;; funcall: Call only if the function is safe and all args evaluated.
   ((eq (car form) 'funcall)
    (let ((args (mapcar #'trawl--eval (cdr form))))
      (if (memq 'no-value args)
          'no-value
        (let ((fun (trawl--safe-function (car args) nil)))
          (if fun
              (condition-case err
                  (apply fun (cdr args))
                (error (signal 'trawl--eval-error (format "eval error: %S: %s"
                                                          form err))))
            'no-value)))))

   ;; mapcar, mapcan: Call only if the function is safe.
   ;; The sequence argument may be missing a few arguments that we cannot
   ;; evaluate.
   ((memq (car form) '(mapcar mapcan))
    (let ((fun (trawl--safe-function (trawl--eval (cadr form)) nil))
          (seq (delq nil (trawl--eval-list (caddr form)))))
      (if fun
          (condition-case err
              (funcall (car form) fun seq)
            (error (signal 'trawl--eval-error (format "eval error: %S: %s"
                                                      form err))))
        'no-value)))

   ;; mapconcat: Call only if the function is safe and all arguments evaluated.
   ((eq (car form) 'mapconcat)
    (let ((fun (trawl--safe-function (trawl--eval (cadr form)) nil))
          (args (mapcar #'trawl--eval (cddr form))))
      (if fun
          (if (memq 'no-value args)
              'no-value
            (condition-case err
                (apply (car form) fun args)
              (error (signal 'trawl--eval-error (format "eval error: %S: %s"
                                                        form err)))))
        'no-value)))
          
   ;; rx, rx-to-string: check for (eval ...) constructs first, then apply.
   ((eq (car form) 'rx)
    (trawl--eval-rx (list (cons 'seq (cdr form)) t)))

   ((eq (car form) 'rx-to-string)
    (let ((args (mapcar #'trawl--eval (cdr form))))
      (if (memq 'no-value args)
          'no-value
        (trawl--eval-rx args))))

   ;; setq: Ignore its side-effect and just pass on the value.
   ((eq (car form) 'setq)
    (let ((val (trawl--eval (caddr form))))
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
                               (list 'quote (trawl--eval (cadr binding))))
                       (cons binding nil)))
                   (cadr form))))
      (let ((trawl--variables (append bindings trawl--variables)))
        (trawl--eval (car (last form))))))

   ;; let*: bind a single variable and recurse.
   ((and (eq (car form) 'let*)
         (null (cdddr form)))
    (let ((bindings (cadr form)))
      (if bindings
          (let* ((binding (car bindings))
                 (trawl--variables
                  (cons
                   (if (consp binding)
                       (cons (car binding)
                             (list 'quote (trawl--eval (cadr binding))))
                     (cons binding nil))
                   trawl--variables)))
            (trawl--eval `(let* ,(cdr bindings) ,@(cddr form))))
        (trawl--eval (car (last form))))))

   ;; Loose comma: can occur if we unwittingly stumbled into a backquote
   ;; form. Just eval the arg and hope for the best.
   ((eq (car form) '\,)
    (trawl--eval (cadr form)))

   ((memq (car form) '(cond)) 'no-value)

   (t
    ;;(trawl--add-to-error-buffer (format "eval rule missing: %S\n" form))
    'no-value)))

;; Evaluate a form as far as possible, attempting to keep its list structure
;; even if all subexpressions cannot be evaluated. Parts that cannot be
;; evaluated are nil.
(defun trawl--eval-list (form)
  (cond
   ((symbolp form)
    (and form
         (let ((val (cdr (assq form trawl--variables))))
           (and val (trawl--eval-list val)))))
   ((atom form)
    form)
   ((not (symbolp (car form)))
    (trawl--add-to-error-buffer (format "eval error: %S\n" form))
    nil)
   ((eq (car form) 'eval-when-compile)
    (trawl--eval-list (car (last form))))

   ;; Pure structure-generating functions: Apply even if we cannot evaluate
   ;; all arguments (they will be nil), because we want a reasonable
   ;; approximation of the structure.
   ((memq (car form) '(list append cons))
    (apply (car form) (mapcar #'trawl--eval-list (cdr form))))

   ((eq (car form) 'purecopy)
    (trawl--eval-list (cadr form)))

   ((memq (car form) '(\` backquote-list*))
    (trawl--eval-list (macroexpand form)))

   (t
    (let ((val (trawl--eval form)))
      (if (eq val 'no-value) nil val)))))

;; Convert something to a list, or nil.
(defun trawl--get-list (form file pos path)
  (condition-case err
      (let ((val (trawl--eval-list form)))
        (and (consp val) val))
    (trawl--eval-error (trawl--report file pos path (cdr err))
                       nil)))
  

;; Convert something to a string, or nil.
(defun trawl--get-string (form file pos path)
  (condition-case err
      (let ((val (trawl--eval form)))
        (and (stringp val) val))
    (trawl--eval-error (trawl--report file pos path (cdr err))
                       nil)))

(defun trawl--check-re (form name file pos path)
  (let ((re (trawl--get-string form file pos path)))
    (when re
      (trawl--check-re-string re name file pos path))))

;; Check a list of regexps.
(defun trawl--check-list (form name file pos path)
  ;; Don't use mapc -- mustn't crash on improper lists.
  (let ((l (trawl--get-list form file pos path)))
    (while (consp l)
      (when (stringp (car l))
        (trawl--check-re-string (car l) name file pos path))
      (setq l (cdr l)))))

;; Check a list of regexps or conses whose car is a regexp.
(defun trawl--check-list-any (form name file pos path)
  (mapc (lambda (elem)
          (cond
           ((stringp elem)
            (trawl--check-re-string elem name file pos path))
           ((and (consp elem)
                 (stringp (car elem)))
            (trawl--check-re-string (car elem) name file pos path))))
        (trawl--get-list form file pos path)))

(defun trawl--check-font-lock-keywords (form name file pos path)
  (trawl--check-list-any form name file pos path))

;; Check regexps in `compilation-error-regexp-alist-alist'
(defun trawl--check-compilation-error-regexp-alist-alist
    (form name file pos path)
  (mapc (lambda (elem)
          (if (cadr elem)
              (trawl--check-re-string
               (cadr elem)
               (format "%s (%s)" name (car elem))
               file pos path)))
        (trawl--get-list form file pos path)))

;; Check a variable on `align-mode-rules-list' format
(defun trawl--check-rules-list (form name file pos path)
  (mapc (lambda (rule)
          (when (and (consp rule)
                     (symbolp (car rule)))
            (let* ((rule-name (car rule))
                   (re-form (cdr (assq 'regexp (cdr rule))))
                   (re (trawl--get-string re-form file pos path)))
              (when (stringp re)
                (trawl--check-re-string 
                 re (format "%s (%s)" name rule-name) file pos path)))))
        (trawl--get-list form file pos path)))

(defun trawl--check-form-recursively (form file pos path)
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
                  (memq re-arg trawl--checked-variables))
       (trawl--check-re re-arg (format "call to %s" (car form))
                        file pos (cons 1 path))))
    (`(,(or `split-string `split-string-and-unquote
            `string-trim-left `string-trim-right
            `directory-files-recursively)
       ,_ ,re-arg . ,rest)
     (unless (and (symbolp re-arg)
                  (memq re-arg trawl--checked-variables))
       (trawl--check-re re-arg (format "call to %s" (car form))
                        file pos (cons 2 path)))
     ;; split-string has another regexp argument (trim, arg 4)
     (when (and (eq (car form) 'split-string)
                (cadr rest))
       (let ((trim (cadr rest)))
         (unless (and (symbolp trim)
                      (memq trim trawl--checked-variables))
           (trawl--check-re trim (format "call to %s" (car form))
                            file pos (cons 4 path))))))
    (`(,(or `defvar `defconst `defcustom)
       ,name ,re-arg . ,rest)
     (when (symbolp name)
       (cond
        ((string-match-p (rx (or "-regexp" "-re" "-regex" "-pattern") eos)
                         (symbol-name name))
         (trawl--check-re re-arg name file pos (cons 2 path))
         (push name trawl--checked-variables))
        ((string-match-p (rx (or (or "-regexps" "-regexes" "-patterns")
                                 (seq (or "-regexp" "-re" "-regex" "-pattern")
                                      "-list"))
                             eos)
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
         (trawl--check-list-any re-arg name file pos (cons 2 path))
         (push name trawl--checked-variables))
        ((string-match-p (rx "-mode-alist" eos)
                         (symbol-name name))
         (trawl--check-list-any re-arg name file pos (cons 2 path))
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
                ((eq arg '&optional))
                ((eq arg '&rest)
                 (setq args nil))
                (t
                 (when (or (string-suffix-p "regexp" (symbol-name arg))
                           (string-suffix-p "regex" (symbol-name arg))
                           (eq arg 're)
                           (string-suffix-p "-re" (symbol-name arg)))
                   (push index indices))
                 (setq index (1+ index)))))
             (setq args (cdr args))))
         (when indices
           (push (cons name (reverse indices)) trawl--regexp-functions)))))
    )

  ;; Check calls to remembered functions with regexp arguments.
  (when (consp form)
    (let ((indices (cdr (assq (car form) trawl--regexp-functions))))
      (when indices
        (let ((index 0)
              (args (cdr form)))
          (while (and indices (consp args))
            (when (= index (car indices))
              (unless (and (symbolp (car args))
                           (memq (car args) trawl--checked-variables))
                (trawl--check-re (car args) (format "call to %s" (car form))
                                 file pos (cons (1+ index) path)))
              (setq indices (cdr indices)))
            (setq args (cdr args))
            (setq index (1+ index)))))))

  (let ((index 0))
    (while (consp form)
      (when (consp (car form))
        (trawl--check-form-recursively (car form) file pos (cons index path)))
      (setq form (cdr form))
      (setq index (1+ index)))))

(defun trawl--check-toplevel-form (form file pos)
  (when (consp form)
    (trawl--check-form-recursively form file pos nil)))
                      
(defun trawl--show-errors ()
  (unless noninteractive
    (let ((pop-up-windows t))
      (display-buffer (trawl--error-buffer))
      (sit-for 0))))

(defun trawl--single-file (file)
  (let ((errors-before trawl--error-count))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((pos nil)
            (keep-going t)
            (read-circle nil)
            (trawl--variables nil)
            (trawl--checked-variables nil)
            (trawl--regexp-functions nil))
        (while keep-going
          (setq pos (point))
          ;;(trawl--report file (point) nil "reading")
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
                 (trawl--report file (point) nil
                                (prin1-to-string err))
                 (setq keep-going nil))))
              (error
               (trawl--report file (point) nil
                              (prin1-to-string err))
               (setq keep-going nil)))
            (when form
              (trawl--check-toplevel-form form file pos))))))
    (when (> trawl--error-count errors-before)
      (trawl--show-errors))))
        
(defun trawl--tree (dir)
  (dolist (file (directory-files-recursively
                 dir (rx bos (not (any ".")) (* anything) ".el" eos)))
    ;;(trawl--add-to-error-buffer (format "trawling %s\n" file))
    (trawl--single-file file)))

(defun trawl--init (file-or-dir dir)
  (unless noninteractive
    (with-current-buffer (trawl--error-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format ";; Trawling %s  -*- compilation -*-\n" file-or-dir)))
      (setq trawl--error-count 0)
      (cd dir))))

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
  (trawl--tree dir)
  (trawl--finish))


(defun trawl-batch ()
  "Scan elisp source files for errors in regex strings.
Call this function in batch mode with files and directories as
command-line arguments.  Files are scanned; directories are
searched recursively for *.el files to scan."
  (unless noninteractive
    (error "`trawl--batch' is to be used only with -batch"))
  (setq trawl--error-count 0)
  (while command-line-args-left
    (let ((arg (pop command-line-args-left)))
      (if (file-directory-p arg)
          (trawl--tree arg)
        (trawl--single-file arg)))))

(provide 'trawl)

;;; trawl.el ends here
