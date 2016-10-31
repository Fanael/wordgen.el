;;; wordgen.el --- Random word generator -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/wordgen.el
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2015-2016, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; Commentary:

;;

;;; Code:
(eval-when-compile (require 'pcase))
(eval-when-compile (require 'cl-lib))


;;; Public interface

;;;###autoload
(cl-defun wordgen (ruleset &key (word-count 1) (starting-rule nil) (seed nil))
  ""
  (let ((compiled-ruleset (wordgen-compile-ruleset ruleset))
        (rng (wordgen--prng-create-from-bytes (or seed (wordgen--get-default-seed))))
        (result '()))
    (dotimes (_ word-count)
      (push (wordgen-evaluate-ruleset compiled-ruleset rng starting-rule) result))
    result))

(defun wordgen-compile-ruleset (ruleset)
  "Compile RULESET to executable form.
RULESET should be a rule set of the same form as in `wordgen'."
  (let ((rules (make-hash-table :test #'eq)))
    (dolist (rule ruleset)
      (pcase rule
        (`(,rule-name ,rule-expr)
         (when (gethash rule-name rules)
           (error "Redefinition of rule %S" rule-name))
         (puthash rule-name (wordgen--compile-expression-to-lambda rule-expr) rules))
        (_
         (error "Invalid rule %S" rule))))
    rules))

(defvar wordgen--output-strings '())

(defmacro wordgen-with-output-to-string (&rest body)
  "Execute BODY, returning the strings passed to `wordgen-print-string'.
The strings are concantenated in the order of the `wordgen-print-string' calls."
  (declare (debug t) (indent defun))
  `(let ((wordgen--output-strings '()))
     (progn ,@body)
     (apply #'concat (nreverse wordgen--output-strings))))

;; NB: `cl-defsubst', not `defsubst', as the former generates slightly better
;; bytecode. This comes at the cost of the arguments being immutable, but
;; `wordgen-print-string' doesn't need to change its argument anyway.
(cl-defsubst wordgen-print-string (string)
  "Print a STRING.
The string is printed to the innermost enclosing
`wordgen-with-output-to-string'."
  (push string wordgen--output-strings)
  "")

(defun wordgen-evaluate-ruleset (ruleset rng &optional starting-rule)
  "Evaluate the RULESET using pseudo-random generator RNG.
Evaluation starts from STARTING-RULE; if that's nil, it starts from `result'."
  (wordgen-with-output-to-string
    (wordgen-call-rule-by-name ruleset rng (or starting-rule 'result))))

(defun wordgen-call-rule-by-name (ruleset rng rule-name)
  "Call a rule using its name.
RULESET is the rule set we're using.
RNG is the pseudo-random number generator.
RULE-NAME is the name of the rule to call."
  (let ((rule-desc (gethash rule-name ruleset)))
    (unless rule-desc
      (error "Rule named %S not found" rule-name))
    (funcall rule-desc ruleset rng)))


;;; Intermediate representation

;; There's no `:named', because we're doing our own type tagging.
(cl-defstruct
    (wordgen--expr
     (:constructor nil)
     (:copier nil)
     (:predicate nil)
     (:type vector))
  "Base struct for intermediate representation expression objects."
  (subclass-type-info :read-only t)
  (type)
  (original-form :read-only t))

(defmacro wordgen--define-derived-expr-type (name-type ctor &rest slots)
  "Define a `cl-defstruct' for a derived IR expression type.

NAME-TYPE is a list (NAME TYPE), where NAME is a symbol used to generate the
type name by prepending \"wordgen--expr-\"; TYPE is the value to initialize
`wordgen--expr''s type slot with.
CTOR is passed directly as `:constructor' to `cl-defstruct'.
SLOTS are passed directly to `cl-defstruct'."
  (declare (indent 2) (doc-string 3))
  (let* ((name (nth 0 name-type))
         (type (nth 1 name-type))
         (struct-name (intern (concat "wordgen--expr-" (symbol-name name))))
         (type-info-sym (intern (concat (symbol-name struct-name) "-type-info")))
         (simply-func-sym (intern (concat (symbol-name struct-name) "-simplify"))))
    `(progn
       ;; We could build the alist at macro expansion time instead, but leaving
       ;; it to runtime means we can sharp-quote the function names, so the byte
       ;; compiler will warn if a function is not defined.
       (defconst ,type-info-sym
         (list
          (cons 'type ',name)
          (cons 'simplify-func #',simply-func-sym)))
       (cl-defstruct
           (,struct-name
            (:include wordgen--expr
                      (subclass-type-info ,type-info-sym)
                      (type ,type))
            (:copier nil)
            (:constructor nil)
            (:constructor ,@ctor)
            (:type vector))
         ,@slots))))

(defun wordgen--expr-subclass-type (expr)
  "Get the type tag of EXPR."
  (cdr (assq 'type (wordgen--expr-subclass-type-info expr))))

(wordgen--define-derived-expr-type (string 'string)
    (wordgen--expr-string-make (value original-form))
  (value :read-only t))

(wordgen--define-derived-expr-type (integer 'integer)
    (wordgen--expr-integer-make (value original-form))
  (value :read-only t))

(wordgen--define-derived-expr-type (choice nil)
    (wordgen--expr-choice-make (children-count total-weight children original-form))
  "CHILDREN is a list of lists (EXPR WEIGHT RUNNING-WEIGHT).
CHILDREN is sorted according to RUNNING-WEIGHT, ascending."
  ;; The children count is trivial to get from the original vector, so store it
  ;; to avoid calling `length' on lists later.
  (children-count :read-only t)
  (total-weight :read-only t)
  (children :read-only t))

(wordgen--define-derived-expr-type (rule-call nil)
    (wordgen--expr-rule-call-make (rule-name original-form))
  (rule-name :read-only t))

(wordgen--define-derived-expr-type (concat 'string)
    (wordgen--expr-concat-make (children original-form))
  (children :read-only t))

(wordgen--define-derived-expr-type (replicate 'string)
    (wordgen--expr-replicate-make (reps subexpr original-form))
  (reps :read-only t)
  (subexpr :read-only t))

(wordgen--define-derived-expr-type (concat-reeval 'string)
    (wordgen--expr-concat-reeval-make (reps subexpr original-form))
  (reps :read-only t)
  (subexpr :read-only t))

(wordgen--define-derived-expr-type (lisp-call nil)
    (wordgen--expr-lisp-call-make (func original-form))
  (func :read-only t))

(defun wordgen--parse-expression (expression)
  "Compile wordgen EXPRESSION to intermediate representation."
  (pcase expression
    ((pred stringp) (wordgen--parse-string expression))
    ((pred integerp) (wordgen--parse-integer expression))
    ((pred vectorp) (wordgen--parse-choice-expr expression))
    ((pred symbolp) (wordgen--parse-rule-call expression))
    (`(++ . ,_) (wordgen--parse-concat expression))
    (`(replicate . ,_) (wordgen--parse-replicate expression))
    (`(eval-multiple-times . ,_) (wordgen--parse-concat-reeval expression))
    (`(lisp . ,_) (wordgen--parse-lisp-call expression))
    (_ (error "Invalid expression %S" expression))))

(defun wordgen--parse-string (string)
  "Compile a STRING literal to intermediate representation."
  (wordgen--expr-string-make string string))

(defun wordgen--parse-integer (integer)
  "Compile an INTEGER literal to intermediate representation."
  (wordgen--expr-integer-make integer integer))

(defun wordgen--parse-choice-expr (vec)
  "Compile a choice expression VEC to intermediate representation."
  (when (= 0 (length vec))
    (error "Empty choice expression"))
  (let ((children-count (length vec))
        (children '())
        (running-total-weight 0))
    (dotimes (i children-count)
      (let ((child (aref vec i)))
        (push (pcase child
                ((and `(,weight ,child-expr)
                      (guard (integerp weight)))
                 (when (<= weight 0)
                   (error "Weight %d is not positive" weight))
                 `(,child-expr ,weight ,(cl-incf running-total-weight weight)))
                (_
                 `(,child 1 ,(cl-incf running-total-weight))))
              children)))
    ;; Actually compile the children now.
    (dolist (child children)
      (cl-callf wordgen--parse-expression (car child)))
    (wordgen--expr-choice-make
     children-count running-total-weight (nreverse children) vec)))

(defun wordgen--parse-rule-call (rule)
  "Compile a RULE call to intermediate representation."
  (wordgen--expr-rule-call-make rule rule))

(defun wordgen--parse-concat (expression)
  "Compile a concat expression to intermediate representation.
EXPRESSION is the whole (++ ...) list."
  (wordgen--expr-concat-make (mapcar #'wordgen--parse-expression (cdr expression))
                             expression))

(defun wordgen--parse-replicate (expression)
  "Compile a replicate expression to intermediate representation.
EXPRESSION is the whole (replicate ...) list."
  (pcase (cdr expression)
    (`(,reps ,child)
     (wordgen--expr-replicate-make
      (wordgen--parse-expression reps)
      (wordgen--parse-expression child)
      expression))
    (_
     (error "Invalid replicate expression %S: expects 2 arguments, %d given"
            expression (length (cdr expression))))))

(defun wordgen--parse-concat-reeval (expression)
  "Compile a concat-reeval expression to intermediate representation.
EXPRESSION is the whole (eval-multiple-times ...) list."
  (pcase (cdr expression)
    (`(,reps ,child)
     (wordgen--expr-concat-reeval-make
      (wordgen--parse-expression reps)
      (wordgen--parse-expression child)
      expression))
    (_
     (error "Invalid concat-reeval expression %S: expects 2 arguments, %d given"
            expression (length (cdr expression))))))

(defun wordgen--parse-lisp-call (expression)
  "Compile a list function call expression to intermediate representation.
EXPRESSION is the whole (lisp ...) list."
  (pcase (cdr expression)
    (`(,func)
     (wordgen--expr-lisp-call-make func expression))
    (_
     (error "Invalid lisp expression %S: expects 1 argument, %d given"
            expression (length (cdr expression))))))


;;; IR-based simplification

;; The simplification is rather, uhm, simple: it mostly handles trivial stuff
;; like transforming (++ "foo" "bar") into "foobar".

(defun wordgen--simplify-expr (expr)
  "Simplify EXPR, returning the simplified expression.
The result is not necessarily `eq' to EXPR."
  (funcall (cdr (assq 'simplify-func (wordgen--expr-subclass-type-info expr)))
           expr))

(defalias 'wordgen--expr-integer-simplify #'identity)
(defalias 'wordgen--expr-string-simplify #'identity)
(defalias 'wordgen--expr-rule-call-simplify #'identity)
(defalias 'wordgen--expr-lisp-call-simplify #'identity)
(defalias 'wordgen--expr-replicate-simplify #'identity)
(defalias 'wordgen--expr-concat-reeval-simplify #'identity)

(defun wordgen--expr-choice-simplify (choice)
  "Simplify a CHOICE expression."
  (pcase (wordgen--expr-choice-children-count choice)
    ;; [anything] is equivalent to anything.
    (1 (wordgen--simplify-expr (caar (wordgen--expr-choice-children choice))))
    (_
     (dolist (child (wordgen--expr-choice-children choice))
       (cl-callf wordgen--simplify-expr (car child)))
     choice)))

(defun wordgen--expr-concat-simplify (concat)
  "Simplify a CONCAT expression."
  (pcase (wordgen--expr-concat-children concat)
    ;; (++) is equivalent to "".
    ((pred null)
     (wordgen--expr-string-make "" (wordgen--expr-original-form concat)))
    ;; (++ anything) is equivalent to anything.
    ((and children (guard (null (cdr children))))
     (wordgen--simplify-expr (car children)))
    (old-children
     ;; Iterate over children, simplifying them and concatenating any adjacent
     ;; strings.
     (let ((new-children '())
           (pending-strings '()))
       (cl-flet
           ((flush-pending-strings
             ()
             (when pending-strings
               (let ((strings (nreverse pending-strings)))
                 (push (wordgen--expr-string-make
                        (apply #'concat
                               (mapcar #'wordgen--expr-string-value strings))
                        ;; Make up an original form.
                        `(++ (...)
                             ,@(mapcar #'wordgen--expr-original-form strings)
                             (...)))
                       new-children)))))
         (dolist (child old-children)
           (let ((simplified (wordgen--simplify-expr child)))
             (if (eq 'string (wordgen--expr-subclass-type simplified))
                 (push simplified pending-strings)
               (flush-pending-strings)
               (push simplified new-children))))
         (flush-pending-strings)
         (setf (wordgen--expr-concat-children concat) (nreverse new-children))))
     concat)))


;;; Expression compiler

;; Wordgen code is made executable by converting its expressions to Emacs Lisp
;; forms, which then are joined together (which is trivial, as Emacs Lisp is a
;; Lisp), put into a lambda, and finally compiled by Emacs's own byte compiler.
;; It's done this way because it's probably the easiest way in Lisp and it's
;; reasonably fast.

(defvar wordgen--compile-to-bytecode t
  "If non-nil, compile the generated lambdas to Emacs bytecode.
Should be t at all times, except when debugging.")

(defun wordgen--compile-expression-to-lambda (expression)
  "Compile rule body EXPRESSION to an Emacs Lisp function."
  (let ((func `(lambda (rules rng)
                 ,(wordgen--compile-expression expression))))
    (if wordgen--compile-to-bytecode
        ;; We have to silence `byte-compile-log-warning' as it can log some
        ;; warnings to *Compile-Log* even though we set `byte-compile-warnings'
        ;; to nil.
        (cl-letf (((symbol-function #'byte-compile-log-warning) #'ignore)
                  (lexical-binding t)
                  (byte-compile-warnings nil)
                  (byte-compile-verbose nil)
                  (byte-optimize t)
                  (byte-compile-generate-call-tree nil))
          (byte-compile func))
      ;; Not compiling, just return the lambda.
      func)))

(defun wordgen--compile-expression (expression)
  "Compile EXPRESSION to an Emacs Lisp form."
  (pcase expression
    ((pred stringp) (wordgen--compile-string expression))
    ((pred integerp) (wordgen--compile-integer expression))
    ((pred vectorp) (wordgen--compile-choice-expr expression))
    ((pred symbolp) (wordgen--compile-rule-call expression))
    ((pred listp) (wordgen--compile-function-call expression))
    (_ (error "Invalid expression %S" expression))))

(defun wordgen--compile-string (string)
  "Compile STRING to an Emacs Lisp form."
  `(wordgen-print-string ,string))

(defun wordgen--compile-integer (integer)
  "Compile INTEGER to an Emacs Lisp form."
  integer)

(defun wordgen--compile-choice-expr (vec)
  "Compile choice expression VEC to an Emacs Lisp form.

It's done by either building a vector of compiled forms and choosing a random
index (when the elements' weights are dense enough), by building a sorted vector
of ranges and binary-searching for a random number (when the weights are
sparse), or by turning it into cond, comparing the rolled number against
consecutive running total weights (when there are very few elements)."
  (when (= 0 (length vec))
    (error "Empty choice expression"))
  (pcase-let ((`(,total-weight . ,subexprs)
               (wordgen--normalize-choice-subexpressions vec)))
    ;; We have three strategies available to us.
    ;; * lookup table: used when the weights are relatively dense.
    ;; * binary search: used when the weights are sparse.
    ;; * cond-based linear search: used when the are few subexpressions.
    ;; The conditions used here are mere heuristics that work good enough.
    (cond
     ((and (nthcdr 2 subexprs)
           (> 10 (/ total-weight (length vec))))
      (wordgen--compile-choice-dense subexprs total-weight))
     ((nthcdr 5 subexprs)
      (wordgen--compile-choice-sparse subexprs total-weight))
     (t
      (wordgen--compile-choice-tiny subexprs total-weight)))))

(defun wordgen--normalize-choice-subexpressions (vec)
  "Transform the choice expression VEC into more usable form.

The returned value is (TOTAL-WEIGHT . SUBEXPRS).
SUBEXPRS is a list of lists (EXPR WEIGHT RUNNING-WEIGHT).
SUBEXPRS is sorted according to RUNNING-WEIGHT, ascending."
  (let ((subexprs '())
        (running-total-weight 0))
    (dotimes (i (length vec))
      (let ((subexpr (aref vec i)))
        (push (pcase subexpr
                ((and `(,weight ,subexpr-expr)
                      (guard (integerp weight)))
                 `(,subexpr-expr ,weight ,(cl-incf running-total-weight weight)))
                (_
                 `(,subexpr 1 ,(cl-incf running-total-weight))))
              subexprs)))
    (cons running-total-weight (nreverse subexprs))))

(defun wordgen--compile-choice-dense (subexprs total-weight)
  "Compile a choice expression into a dense table lookup.

SUBEXPRS and TOTAL-WEIGHT are the results of
`wordgen--normalize-choice-subexpressions', which see."
  (let ((vec (wordgen--build-vector
              total-weight subexprs #'wordgen--build-choice-subexpression)))
    ;; Note: this let is actually useful, as it lets the compiler macro on
    ;; `wordgen--eval-choice-subexpression' generate optimal bytecode.
    `(let ((x (aref ,vec (wordgen-prng-next-int ,(1- total-weight) rng))))
       (wordgen--eval-choice-subexpression x rules rng))))

(defun wordgen--compile-choice-tiny (subexprs total-weight)
  "Compile a choice expression into a series of conditionals.

SUBEXPRS and TOTAL-WEIGHT are the results of
`wordgen--normalize-choice-subexpressions', which see."
  `(let ((number (wordgen-prng-next-int ,(1- total-weight) rng)))
     (cond
      ,@(mapcar
         (pcase-lambda (`(,expr _ ,limit))
           `((< number ,limit)
             ,(wordgen--compile-expression expr)))
         subexprs))))

(defun wordgen--compile-choice-sparse (subexprs total-weight)
  "Compile a choice expression into binary search.

SUBEXPRS and TOTAL-WEIGHT are the results of
`wordgen--normalize-choice-subexpressions', which see."
  (let ((vec
         (apply
          #'vector
          (mapcar
           (pcase-lambda (`(,expr ,weight ,running-weight))
             (list (- running-weight weight)
                   running-weight
                   (wordgen--build-choice-subexpression expr)))
           subexprs))))
    `(wordgen--choice-binary-search
      ,vec (wordgen-prng-next-int ,(1- total-weight) rng) rules rng)))

(defun wordgen--build-vector (length subexprs fn)
  "Build a vector of LENGTH elements using SUBEXPRS.

For each (EXPR WEIGHT . _) element of SUBEXPRS, the result of (funcall FN EXPR)
appears WEIGHT times in the returned vector."
  (let ((result (make-vector length nil))
        (i 0))
    (pcase-dolist (`(,expr ,weight . ,_) subexprs)
      (let ((fn-expr (funcall fn expr)))
        (dotimes (_ weight)
          (aset result i fn-expr)
          (cl-incf i))))
    result))

(defun wordgen--build-choice-subexpression (subexpr)
  "Generate a vector element for a expression SUBEXPR.

Strings are returned unchanged, other forms are wrapped in a lambda and
compiled."
  (if (stringp subexpr)
      subexpr
    (wordgen--compile-expression-to-lambda subexpr)))

(cl-defsubst wordgen--eval-choice-subexpression (subexpr rules rng)
  "Eval a SUBEXPR compiled by `wordgen--build-choice-subexpression'.

RULES and RNG are passed unchanged to the compiled form."
  (if (stringp subexpr)
      (wordgen-print-string subexpr)
    (funcall subexpr rules rng)))

(defun wordgen--choice-binary-search (vec number rules rng)
  "Find the range in VEC in which NUMBER is, using binary search.

VEC is a sorted vector of (BEGIN END FORM), where [BEGIN..END) is a numeric
range and FORM is its corresponding compiled form returned from
`wordgen--build-choice-subexpression'.

RULES and RNG are passed unchanged to the compiled forms."
  (catch 'return
    (let ((low 0)
          (high (length vec)))
      (while t
        (let* ((half (+ low (/ (- high low) 2)))
               (guess (aref vec half))
               (guess-low (nth 0 guess)))
          (cond
           ((and (<= guess-low number)
                 (< number (nth 1 guess)))
            (let ((form (nth 2 guess)))
              (throw 'return (wordgen--eval-choice-subexpression form rules rng))))
           ((> guess-low number)
            (setq high half))
           (t
            (setq low (1+ half)))))))))

(defun wordgen--compile-function-call (function-call)
  "Compile a builtin FUNCTION-CALL to an Emacs Lisp form."
  (pcase function-call
    (`(++ . ,rest)
     (wordgen--compile-concat rest))
    (`(replicate ,times ,expr)
     (wordgen--compile-replicate times expr))
    (`(eval-multiple-times ,times ,expr)
     (wordgen--compile-eval-multiple-times times expr))
    (`(lisp ,function)
     (wordgen--compile-call-lisp function))
    (_
     (error "Invalid function call expression %S" function-call))))

(defun wordgen--compile-concat (expressions)
  "Compile concatenation to an Emacs Lisp form.
EXPRESSIONS is a list of subexpressions.

As all EXPRESSIONS must evaluate to a string, and we concat strings by adding
them to a list, this is just a `progn'."
  `(progn
     ,@(mapcar (lambda (expr)
                 `(let ((result ,(wordgen--compile-expression expr)))
                    (unless (stringp result)
                      (error "Arguments of ++ must be strings"))
                    result))
               expressions)))

(defun wordgen--compile-replicate (times expr)
  "Compile a replicate expression to an Emacs Lisp form.

TIMES is the uncompiled replication count subexpression.
EXPR is the uncompiled subexpression evaluating to the string to replicate.
If TIMES is <= 0, the whole replicate evaluates to an empty string."
  (let ((times-compiled (wordgen--compile-expression times))
        (expr-compiled (wordgen--compile-expression expr)))
    `(let ((times ,times-compiled))
       (unless (integerp times)
         (error "Replication count %S is not an integer" times))
       (when (> times 0)
         (let ((string
                (wordgen-with-output-to-string
                  (unless (stringp ,expr-compiled)
                    (error "Second argument of replicate must be a string")))))
           (dotimes (_ times)
             (push string wordgen--output-strings))))
       "")))

(defun wordgen--compile-eval-multiple-times (times expr)
  "Compile an eval-multiple-times expression to an Emacs Lisp form.

TIMES is the uncompiled replication count subexpression.
EXPR is the uncompiled subexpression to evaluate multiple times.
Only the result of the last evaluation is returned.
If TIMES is <= 0, the whole eval-multiple-times evaluates to an empty string."
  (let ((times-compiled (wordgen--compile-expression times))
        (expr-compiled (wordgen--compile-expression expr)))
    `(let ((times ,times-compiled))
       (unless (integerp times)
         (error "Evaluation count %S is not an integer" times))
       (if (<= times 0)
           ""
         ;; Run the loop N-1 times, so the result of the if will be the same as
         ;; that of last expr evaluation.
         (dotimes (_ (1- times))
           ,expr-compiled)
         ,expr-compiled))))

(defun wordgen--compile-call-lisp (lisp-function)
  "Compile a call to a LISP-FUNCTION."
  `(funcall ,lisp-function rules rng))

(defun wordgen--compile-rule-call (rule-name)
  "Compile a call to rule RULE-NAME."
  `(wordgen-call-rule-by-name rules rng ',rule-name))


;;; PRNG helpers

(when (< most-positive-fixnum #xFFFFFFFF)
  (error "This package requires Lisp integers to be at least 32-bit"))

(eval-and-compile
  (defconst wordgen--prng-array-size 128)
  (defconst wordgen--prng-optimal-bytes-size (* 4 wordgen--prng-array-size)))

(defun wordgen--get-default-seed ()
  "Get the PRNG default seed.
When available, uses /dev/urandom."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (pcase (ignore-errors
             (call-process "dd" nil '(t nil) nil
                           "if=/dev/urandom"
                           (eval-when-compile (concat "bs=" (number-to-string wordgen--prng-optimal-bytes-size)))
                           "count=1"))
      (0 (buffer-string))
      (_ (current-time-string)))))


;;; PRNG engine

(defun wordgen--prng-create (passed-array)
  "Create a PRNG seeded using PASSED-ARRAY.
PASSED-ARRAY must be a vector of 128 32-bit integers."
  (let ((index 0)
        (array (copy-sequence passed-array)))
    (lambda ()
      (cl-macrolet
          ((as-index
            (index)
            `(logand ,index ,(- (1- wordgen--prng-array-size))))
           (xorshift
            (shift value)
            (let ((value-symbol (make-symbol "value")))
              `(let ((,value-symbol ,value))
                 (logxor ,value-symbol
                         ,(if (> shift 0)
                              ;; We're shifting left, so the result may be bigger than 32
                              ;; bits; truncate it.
                              `(logand (lsh ,value-symbol ,shift) #xFFFFFFFF)
                            `(lsh ,value-symbol ,shift)))))))
        (let* ((result
                (logxor (xorshift -12 (xorshift 17 (aref array index)))
                        (xorshift -15 (xorshift 13 (aref array (as-index (+ index 33))))))))
          (aset array index result)
          (setq index (as-index (1+ index)))
          result)))))

(defun wordgen--prng-create-from-bytes (bytes)
  "Create a prng seeded from BYTES.
BYTES should be a sequence of integers."
  (let* ((array (make-vector wordgen--prng-array-size 0))
         (bytes-word-len (/ (length bytes) 4))
         (i 0)
         (max-words (min wordgen--prng-array-size bytes-word-len)))
    (while (< i max-words)
      (let ((word 0))
        (dotimes (k 4)
          (setq word (lsh word 8))
          (setq word (+ word (logand #xFF (elt bytes (+ k (* i 4)))))))
        (aset array i word))
      (cl-incf i))
    ;; No more words in bytes, fill the state using a linear congruential
    ;; generator.
    (let ((seed (if (= i 0) 1 (aref array (1- i)))))
      (while (< i wordgen--prng-array-size)
        (setq seed (logand #xFFFFFFFF (+ 1013904223 (* 1664525 seed))))
        (aset array i seed)
        (cl-incf i)))
    (wordgen--prng-create array)))

(defun wordgen--prng-next-int-small (limit rng)
  "Return a 32-bit pseudo-random integer in interval [0, LIMIT] using RNG.
LIMIT must be a non-negative integer smaller than 2^32-1."
  (let* ((range (1+ limit))
         (scaling-factor (/ #xFFFFFFFF range))
         (actual-limit (* range scaling-factor))
         (result nil))
    (while (< actual-limit (setq result (funcall rng))))
    (/ result scaling-factor)))

(defun wordgen-prng-next-int (limit rng)
  "Return a pseudo-random integer in interval [0, LIMIT] using RNG.
LIMIT must be a non-negative integer."
  (cond
   ((< limit #xFFFFFFFF) (wordgen--prng-next-int-small limit rng))
   ((= limit #xFFFFFFFF) (funcall rng))
   (t (let ((result nil))
        (while (> (setq result (+ (lsh (wordgen-prng-next-int (lsh limit -32) rng) 32)
                                  (funcall rng)))
                  limit))
        result))))

(provide 'wordgen)
;;; wordgen.el ends here
