;;; wordgen-prng.el --- Pseudo-random number generator -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/wordgen.el
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2015, Fanael Linithien
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
(require 'wordgen-prng)

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
  "Compile RULESET to executable form."
  (let ((rules (make-hash-table :test #'eq)))
    (dolist (rule ruleset)
      (pcase rule
        (`(,rule-name ,rule-expr)
         (when (gethash rule-name rules)
           (error "Redefinition of rule %S" rule-name))
         (puthash rule-name (wordgen--compile-expression rule-expr) rules))
        (_
         (error "Invalid rule %S" rule))))
    rules))

(defvar wordgen--output-strings '())

(defmacro wordgen--with-output-to-string (&rest body)
  ""
  (declare (debug t) (indent defun))
  `(let ((wordgen--output-strings '()))
     (progn ,@body)
     (apply #'concat (nreverse wordgen--output-strings))))

(defun wordgen-evaluate-ruleset (ruleset rng &optional starting-rule)
  "Evaluate the RULESET using pseudo-random generator RNG.
Evaluation starts from STARTING-RULE; if that's nil, it starts from `result'."
  (wordgen--with-output-to-string
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

(defun wordgen--compile-expression (expression)
  "Compile EXPRESSION to executable form."
  (pcase expression
    ((pred stringp) (wordgen--compile-string expression))
    ((pred integerp) (wordgen--compile-integer expression))
    ((pred vectorp) (wordgen--compile-choice-expr expression))
    ((pred symbolp) (wordgen--compile-rule-call expression))
    ((pred listp) (wordgen--compile-function-call expression))
    (_ (error "Invalid expression %S" expression))))

(defun wordgen--compile-string (string)
  "Compile STRING to executable form."
  (lambda (_rules _rng)
    (push string wordgen--output-strings)
    'string))

(defun wordgen--compile-integer (integer)
  "Compile INTEGER to executable form."
  (lambda (_rules _rng)
    integer))

(defun wordgen--compile-choice-expr (vec)
  "Compile choice expression VEC to executable form."
  (let* ((len (length vec))
         (compiled-subexprs (make-vector len nil))
         (total-weight-so-far 0))
    (dotimes (i len)
      (pcase-let ((`(,weight . ,expr)
                   (pcase (aref vec i)
                     (`(,weight ,expr) (cons weight expr))
                     (expr (cons 1 expr)))))
        (setq total-weight-so-far (+ total-weight-so-far weight))
        (aset compiled-subexprs i (cons total-weight-so-far (wordgen--compile-expression expr)))))
    (let ((max-rolled-number total-weight-so-far))
      (lambda (rules rng)
        (let ((rolled-number (wordgen--prng-next-int max-rolled-number rng)))
          (catch 'break
            (let ((i 0))
              ;; We could use binary search here, but the typical number of
              ;; choices is too small for it to be worthwhile.
              (while t
                (let ((element (aref compiled-subexprs i)))
                  (setq i (1+ i))
                  (when (<= rolled-number (car element))
                    (throw 'break (funcall (cdr element) rules rng))))))))))))

(defun wordgen--compile-function-call (function-call)
  "Compile a builtin FUNCTION-CALL to executable form."
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
  "Compile concatenation to executable form.
EXPRESSIONS is a list of subexpressions."
  (let* ((len (length expressions))
         (compiled-subexprs (make-vector len nil))
         (i 0))
    (dolist (expr expressions)
      (aset compiled-subexprs i (wordgen--compile-expression expr))
      (setq i (1+ i)))
    (lambda (rules rng)
      (dotimes (i len)
        (unless (eq 'string (funcall (aref compiled-subexprs i) rules rng))
          (error "Arguments of ++ must be strings")))
      'string)))

(defun wordgen--compile-replicate (times expr)
  ""
  (let ((times-compiled (wordgen--compile-expression times))
        (expr-compiled (wordgen--compile-expression expr)))
    (lambda (rules rng)
      (let ((actual-times (funcall times-compiled rules rng)))
        (unless (integerp actual-times)
          (error "Replication count %S is not an integer" actual-times))
        (when (> actual-times 0)
          (let ((string
                 (wordgen--with-output-to-string
                   (unless (eq 'string (funcall expr-compiled rules rng))
                     (error "Second argument of replicate must be a string")))))
            (dotimes (_ actual-times)
              (push string wordgen--output-strings)))))
      'string)))

(defun wordgen--compile-eval-multiple-times (times expr)
  ""
  (let ((times-compiled (wordgen--compile-expression times))
        (expr-compiled (wordgen--compile-expression expr)))
    (lambda (rules rng)
      (let ((actual-times (funcall times-compiled rules rng)))
        (unless (integerp actual-times)
          (error "Replication count %S is not an integer" actual-times))
        (if (<= actual-times 0)
            'string
          (dotimes (_ (1- actual-times))
            (funcall expr-compiled rules rng))
          (funcall expr-compiled rules rng))))))

(defun wordgen--compile-call-lisp (lisp-function)
  ""
  lisp-function)

(defun wordgen--compile-rule-call (rule-name)
  "Compile a call to rule RULE-NAME."
  (lambda (rules rng)
    (wordgen-call-rule-by-name rules rng rule-name)))

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

(provide 'wordgen)
;;; wordgen.el ends here
