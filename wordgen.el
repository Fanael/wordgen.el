;;; wordgen-prng.el --- Pseudo-random number generator -*- lexical-binding: t -*-

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
         (puthash rule-name (wordgen--compile-rule-body rule-expr) rules))
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

(defun wordgen-print-string (string)
  "Print a STRING.
The string is printed to the innermost enclosing
`wordgen-with-output-to-string'."
  (push string wordgen--output-strings)
  'string)

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
    (wordgen--call-rule-by-desc ruleset rng rule-desc)))

(defun wordgen--call-rule-by-desc (ruleset rng rule-desc)
  "Call a rule using its descriptor.
RULESET is the rule set we're using.
RNG is the pseudo-random number generator.
RULE-DESC is the rule descriptor to call."
  (funcall rule-desc ruleset rng))


;;; Expression compiler

;; Wordgen code is made executable by converting its expressions to Emacs Lisp
;; forms, which then are joined together (which is trivial, as Emacs Lisp is a
;; Lisp), put into a lambda, and finally compiled by Emacs's own byte compiler.
;; It's done this way because it's probably the easiest way in Lisp and it's
;; reasonably fast.

(defvar wordgen--compile-to-bytecode t
  "If non-nil, compile the generated lambdas to Emacs bytecode.
Should be t at all times, except when debugging.")

(defun wordgen--compile-rule-body (expression)
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

Currently it's done by turning it into cond, comparing the rolled number against
consecutive running total weights."
  ;; TODO: consider compiling to jump tables where appropriate.
  (let* ((total-weight-so-far 0)
         (cond-clauses
          (mapcar (lambda (subexpr)
                    (let ((weight 1)
                          (expr subexpr))
                      (pcase subexpr
                        ((and `(,subexpr-weight ,subexpr-expr)
                              (guard (integerp subexpr-weight)))
                         (setq weight subexpr-weight)
                         (setq expr subexpr-expr)))
                      (setq total-weight-so-far (+ total-weight-so-far weight))
                      `((< rolled-number ,total-weight-so-far)
                        ,(wordgen--compile-expression expr))))
                  vec)))
    `(let ((rolled-number (wordgen-prng-next-int ,(1- total-weight-so-far) rng)))
       (cond
        ,@cond-clauses))))

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
                    (unless (eq 'string result)
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
                  (unless (eq 'string ,expr-compiled)
                    (error "Second argument of replicate must be a string")))))
           (dotimes (_ times)
             (push string wordgen--output-strings))))
       'string)))

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
           'string
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
      (setq i (1+ i)))
    ;; No more words in bytes, fill the state using a linear congruential
    ;; generator.
    (let ((seed (if (= i 0) 1 (aref array (1- i)))))
      (while (< i wordgen--prng-array-size)
        (setq seed (logand #xFFFFFFFF (+ 1013904223 (* 1664525 seed))))
        (aset array i seed)
        (setq i (1+ i))))
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
