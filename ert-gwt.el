;;; ert-gwt.el --- GWT-style BDD testing for ERT -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl

;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/ert-gwt
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.4"))
;; Keywords: lisp tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ert-gwt adds Given-When-Then structure to ERT:
;;
;;     (ert-gwt-deftest
;;       (:given ((user (make-user "alice" "secret"))))
;;       (:when  (login user))
;;       (:then  (login-redirects-to-home-p user)))
;;
;; `:given' takes `let'-style bindings followed by setup forms and may
;; repeat; later givens see earlier bindings.  `:when' is the action
;; under test and is required.  `:then' forms are wrapped in `should'
;; and may repeat.  `:cleanup' forms run after the test, even on failure.
;;
;; Each test is named after the defining file and a hash of its clauses:
;; <file-prefix>-<hash8>, with -2, -3, ... appended when a file contains
;; identical clauses more than once.  The name is ERT's symbol; it is not
;; written into the source, so the defining form is recorded on the test
;; symbol for `find-function'.
;;
;; Setup stubs belong in `:given' as place bindings:
;;
;;     (:given (((symbol-function 'yes-or-no-p)
;;                (lambda (_prompt) t))))
;;
;; A `cl-letf' written as a setup form is not a stub: the rebinding is
;; undone as soon as that form finishes.
;;
;; After the user `:cleanup' forms, buffers created during the test are
;; killed and files made with `ert-gwt--temp-file' are deleted.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'find-func)

(defvar ert-gwt--seen (make-hash-table :test 'equal)
  "Per-file test name counters.
Keys are file truenames; values are hash tables mapping name bases to
use counts.")

(defun ert-gwt--seen-clear-for-load (file)
  "Discard the name counters recorded for FILE.
Runs on `after-load-functions' so reloading produces the same names."
  (remhash (or (ert-gwt--source-file file) (file-truename file))
            ert-gwt--seen))

(defun ert-gwt--source-file (&optional file)
  "Return the source file for FILE, or for the current load context.
FILE defaults to `load-file-name', then the variable
`buffer-file-name'.  A byte-compiled .elc file maps to its .el source
when that exists."
  (let ((file (or file
                 load-file-name
                 (and (fboundp 'macroexp-file-name)
                      (macroexp-file-name))
                 buffer-file-name)))
    (when file
      (setq file (file-truename file))
      (if (and (string-match "\\.elc\\'" file)
               (file-exists-p (concat (file-name-sans-extension file) ".el")))
          (concat (file-name-sans-extension file) ".el")
        file))))

(defun ert-gwt--print-form (form)
  "Return a stable printed representation of FORM.
Bind the print variables that can truncate output, so a test name
is derived from the whole form."
  (let ((print-length nil)
        (print-level nil)
        (print-circle t)
        (print-gensym t)
        (print-escape-newlines t)
        (print-escape-nonascii t))
    (prin1-to-string form)))

(defun ert-gwt--find-definition (symbol)
  "Return (BUFFER . POSITION) for SYMBOL's recorded GWT definition.
Return nil when the definition cannot be located."
  (let* ((definition (get symbol 'ert-gwt--definition))
         (file (plist-get definition :file))
         (clauses (plist-get definition :clauses))
         (occurrence (or (plist-get definition :occurrence) 1)))
    (when (and file (file-readable-p file) clauses)
      (condition-case nil
          (with-current-buffer (find-file-noselect file)
            (save-excursion
              (save-restriction
                (widen)
                (with-syntax-table emacs-lisp-mode-syntax-table
                  (goto-char (point-min))
                  (let ((seen 0)
                        found)
                    (condition-case nil
                        (while (not (eobp))
                          (forward-comment (point-max))
                          (if (looking-at "(ert-gwt-deftest\\_>")
                              (let ((start (point))
                                    (form (let ((read-eval nil))
                                            (ignore read-eval)
                                            (read (current-buffer)))))
                                (when (and (consp form)
                                           (equal (cdr form) clauses))
                                  (setq seen (1+ seen))
                                  (when (= seen occurrence)
                                    (setq found start))))
                            (forward-sexp 1)))
                      (end-of-file nil)
                      (error nil))
                    (when found
                      (cons (current-buffer) found)))))))
        (error nil)))))

(defun ert-gwt--advice-find-function-search (orig symbol type library)
  "Call ORIG unless SYMBOL has a recorded GWT definition.
TYPE and LIBRARY are passed to ORIG."
  (or (and (symbolp symbol)
           (get symbol 'ert-gwt--definition)
           (ert-gwt--find-definition symbol))
      (funcall orig symbol type library)))

(defun ert-gwt--advice-find-definition-noselect (orig symbol type &optional file)
  "Call ORIG unless SYMBOL has a recorded GWT definition.
TYPE and FILE are passed to ORIG."
  (or (and (symbolp symbol)
           (get symbol 'ert-gwt--definition)
           (ert-gwt--find-definition symbol))
      (funcall orig symbol type file)))

(defun ert-gwt-unload-function ()
  "Undo the setup done when ert-gwt was loaded."
  (remove-hook 'after-load-functions #'ert-gwt--seen-clear-for-load)
  (ert-gwt-defining-file-jump-mode -1)
  nil)

;;;###autoload
(define-minor-mode ert-gwt-defining-file-jump-mode
  "Jump to `ert-gwt-deftest' definitions from ERT results.
When enabled, `find-function' uses the defining form recorded when
the test was defined."
  :global t
  :group 'ert-gwt
  (if ert-gwt-defining-file-jump-mode
      (progn
        (advice-add 'find-function-search-for-symbol :around
                    #'ert-gwt--advice-find-function-search)
        (advice-add 'find-definition-noselect :around
                    #'ert-gwt--advice-find-definition-noselect)
        (add-hook 'after-load-functions #'ert-gwt--seen-clear-for-load))
    (remove-hook 'after-load-functions #'ert-gwt--seen-clear-for-load)
    (advice-remove 'find-function-search-for-symbol
                   #'ert-gwt--advice-find-function-search)
    (advice-remove 'find-definition-noselect
                   #'ert-gwt--advice-find-definition-noselect)))

(ert-gwt-defining-file-jump-mode 1)

(when (and (boundp 'load-file-name) load-file-name)
  (let ((dir (file-name-directory load-file-name)))
    (add-to-list 'load-path dir)
    (let ((lisp-dir (expand-file-name "lisp" dir)))
      (when (file-directory-p lisp-dir)
        (add-to-list 'load-path lisp-dir)))))

(defun ert-gwt--parse-clauses (clauses)
  "Parse CLAUSES into a plist with :givens, :when, :thens and :cleanups.
Each :given is recorded as (BINDINGS . SETUP-FORMS)."
  (let (givens when when-p thens cleanups)
    (dolist (clause clauses)
      (pcase clause
        (`(:given . ,rest)
         (let ((first (car rest)))
           (if (and (consp first) (listp (car first)))
               (setq givens (append givens (list (cons first (cdr rest))) nil))
             (setq givens (append givens (list (cons nil rest)) nil)))))
        (`(:when ,form)
         (when when-p
           (error "ert-gwt-deftest: Only one :when clause allowed"))
         (setq when form
               when-p t))
        (`(:then . ,rest)
         (dolist (form rest)
           (push form thens)))
        (`(:cleanup . ,rest)
         (setq cleanups (append cleanups rest nil)))
        (_
         (error "ert-gwt-deftest: Unrecognized clause %S" clause))))
    (unless when-p
      (error "ert-gwt-deftest: Missing :when clause"))
    (unless thens
      (error "ert-gwt-deftest: Missing :then clause"))
    (list :givens givens
          :when when
          :thens (nreverse thens)
          :cleanups cleanups)))

(defgroup ert-gwt nil
  "Given-When-Then extensions for ERT."
  :prefix "ert-gwt-"
  :group 'ert)

(defun ert-gwt--expand-givens (GIVENS WHEN-FORM THENS CLEANUPS)
  "Expand GIVENS, WHEN-FORM, THENS and CLEANUPS into a test body.
All bindings go into one `cl-letf*', so each binding sees the earlier
ones and a place stub covers the whole scenario.  A stubbed function
must already be defined, since its old value is read at setup time.
WHEN and each THEN are reported separately; a failure is re-signalled
after every clause has run, and CLEANUPS always run."
  (let* ((given-lines
         (mapcar (lambda (segment)
                   (ert-gwt--clause-text "GIVEN" segment))
                 GIVENS))
        (when-text (ert-gwt--clause-text "WHEN" WHEN-FORM))
        ;; The steps alone are the body when there is nothing to clean up.
        ;; `unwind-protect' with no unwind forms is legal but pointless, and
        ;; Emacs 30 warns about it on every test that has no `:cleanup'.
        (steps
         `((ert-info (,when-text)
              (condition-case ert-gwt--err
                  (progn ,WHEN-FORM)
                (error
                 (setq ert-gwt--aborted t)
                 (setq ert-gwt--first-error ert-gwt--err))))
           ,@(mapcar #'ert-gwt--expand-then THENS)
           (when ert-gwt--first-error
             (ert-info (,when-text)
               (ert-info ((mapconcat #'identity
                                     (nreverse ert-gwt--then-notes)
                                     "\n"))
                 (signal (car ert-gwt--first-error)
                         (cdr ert-gwt--first-error))))))))
    `(ert-info (,(mapconcat #'identity given-lines "\n"))
       (cl-letf* (,@(apply #'append (mapcar #'car GIVENS)))
         ,@(apply #'append (mapcar #'cdr GIVENS))
         (let ((ert-gwt--then-notes nil)
               (ert-gwt--aborted nil)
               (ert-gwt--first-error nil))
           ,(if CLEANUPS
                `(unwind-protect (progn ,@steps) ,@CLEANUPS)
              `(progn ,@steps)))))))

(defvar ert-gwt--tracked-files nil
  "Temp files created by `ert-gwt--temp-file', deleted after each test.")

(defvar ert-gwt--then-notes nil
  "Status notes for the THEN clauses of the current test.")

(defvar ert-gwt--aborted nil
  "Non-nil when a WHEN or THEN step has failed.")

(defvar ert-gwt--first-error nil
  "First error caught during the current test.")

(defun ert-gwt--temp-file (&optional prefix)
  "Create a temporary file named with PREFIX and delete it after the test.
PREFIX defaults to \"ert-gwt-\"."
  (let ((file (make-temp-file (or prefix "ert-gwt-"))))
    (push file ert-gwt--tracked-files)
    file))

(defun ert-gwt--cleanup (pre-buffers)
  "Kill buffers not in PRE-BUFFERS and delete tracked temp files."
  (dolist (buf (buffer-list))
    (unless (memq buf pre-buffers)
      (when (buffer-live-p buf)
        (kill-buffer buf))))
  (dolist (file ert-gwt--tracked-files)
    (when (file-exists-p file)
      (if (file-directory-p file)
          (delete-directory file t)
        (delete-file file))))
  (setq ert-gwt--tracked-files nil))

(defun ert-gwt--clause-text (label form)
  "Return an `ert-info' message for LABEL and FORM.
FORM is printed and % escaped so it can be spliced safely."
  (format "%s: %s"
          label
          (replace-regexp-in-string "%" "%%"
                                    (prin1-to-string form))))

(defun ert-gwt--expand-then (THEN)
  "Expand one THEN form, recording a status note.
A form that already starts with `should', `should-not', `should-error'
or `ert-info' is used as-is; anything else is wrapped in `should'.
If an earlier step failed, record a skipped note instead.  Errors are
stored for `ert-gwt--expand-givens' to re-signal."
  (let* ((assertion (if (and (consp THEN)
                             (memq (car THEN) '(should should-not should-error ert-info)))
                        THEN
                      `(should ,THEN)))
         (info-text (ert-gwt--clause-text "THEN" THEN))
         (ok-note (ert-gwt--clause-text "✓ THEN" THEN))
         (fail-note (ert-gwt--clause-text "✗ THEN" THEN))
         (skip-note (ert-gwt--clause-text "− THEN" THEN)))
    `(cond
      (ert-gwt--aborted
       (setq ert-gwt--then-notes
             (cons ,skip-note ert-gwt--then-notes)))
      (t
       (condition-case ert-gwt--err
           (progn
             (ert-info (,info-text)
               ,assertion)
             (setq ert-gwt--then-notes
                   (cons ,ok-note ert-gwt--then-notes)))
         (error
          (setq ert-gwt--aborted t)
          (unless ert-gwt--first-error
            (setq ert-gwt--first-error ert-gwt--err))
          (setq ert-gwt--then-notes
                (cons ,fail-note ert-gwt--then-notes))))))))

(defun ert-gwt--name-and-occurrence (clauses)
  "Return (NAME . OCCURRENCE) for CLAUSES.
NAME is interned in `ert-gwt--seen'; OCCURRENCE is the one-based
position among identical clause blocks in the defining file."
  (let* ((file (ert-gwt--source-file))
         (key (or file "test-gwt"))
         (prefix (if file (file-name-base file) "test-gwt"))
         (subtable (or (gethash key ert-gwt--seen)
                       (puthash key
                                (make-hash-table :test 'equal)
                                ert-gwt--seen))))
    (let ((result (ert-gwt--compute-name prefix clauses subtable)))
      (cons (intern (car result)) (cdr result)))))

(defun ert-gwt--name (clauses)
  "Return an interned test name for CLAUSES.
The prefix is the defining file's base name, or \"test-gwt\" when no
source file is known; counters are kept per file in `ert-gwt--seen'."
  (car (ert-gwt--name-and-occurrence clauses)))

(defun ert-gwt--compute-name (prefix clauses taken)
  "Return (NAME . OCCURRENCE) for CLAUSES under PREFIX.
NAME is PREFIX-HASH8, with -2, -3, ... appended on collisions in
TAKEN; OCCURRENCE is the one-based count for that name."
  (let* ((hash (substring (secure-hash 'sha1 (format "%S" clauses)) 0 8))
         (base (format "%s-%s" prefix hash))
         (count (gethash base taken 0)))
    (if (zerop count)
        (progn
          (puthash base 1 taken)
          (cons base 1))
      (puthash base (1+ count) taken)
      (cons (format "%s-%d" base (1+ count)) (1+ count)))))

(defun ert-gwt-name-from-clauses (prefix clauses taken)
  "Return a test name for CLAUSES under PREFIX.
The name is PREFIX-HASH8, with -2, -3, ... appended on collisions
within TAKEN.  TAKEN maps name bases to use counts and is updated in
place."
  (car (ert-gwt--compute-name prefix clauses taken)))

(defmacro ert-gwt-deftest (&rest clauses)
  "Define a GWT-style ERT test from CLAUSES.

CLAUSES may contain:

  (:given BINDINGS FORM...)  `let'-style bindings and setup forms; may
                             repeat, and later givens see earlier ones
  (:when FORM)               action under test, required, once
  (:then FORM)               expectation, one or more; wrapped in
                             `should' unless it already asserts
  (:cleanup FORM...)         cleanup forms, run even on failure

The place bindings in each :given wrap the rest of the test, so a stub
written there covers `:when' and `:then'.  Setup forms run in order but
do not wrap the later clauses.  WHEN and each THEN get their own status
line in the ERT report.  After the `:cleanup' forms, buffers and temp
files created by the test are removed automatically.

The test name comes from the defining file and a hash of CLAUSES.  For
the example below it is an ERT symbol such as `ert-gwt-3f2a9c1d'; the
exact hash depends on the file and the clauses:

  (ert-gwt-deftest
    (:given ((file (ert-gwt--temp-file \"data.txt\"))
             (old \"old contents\")
             (new \"new contents\")
             ((symbol-function \\='yes-or-no-p)
              (lambda (_prompt) t)))
            (with-temp-file file (insert old)))
    (:when (when (yes-or-no-p \"Replace contents?\")
             (with-temp-file file (insert new))))
    (:then (should (equal \"new contents\"
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string)))))
    (:cleanup (delete-file file)))"
  (declare (indent 0))
  (let* ((parsed (ert-gwt--parse-clauses clauses))
         (name-info (ert-gwt--name-and-occurrence clauses))
         (name (car name-info))
         (occurrence (cdr name-info))
         (file (ert-gwt--source-file))
         (givens (plist-get parsed :givens))
         (when-form (plist-get parsed :when))
         (thens (plist-get parsed :thens))
         (cleanups (plist-get parsed :cleanups)))
    `(progn
       (put ',name 'ert-gwt--definition
            ',(list :file file :clauses clauses :occurrence occurrence))
       (ert-deftest ,name ()
         "Anonymous GWT-style test defined by `ert-gwt-deftest'."
         (let ((ert-gwt--pre-buffers (buffer-list)))
           (unwind-protect
               ,(ert-gwt--expand-givens givens when-form thens cleanups)
             (ert-gwt--cleanup ert-gwt--pre-buffers)))))))

(provide 'ert-gwt)

;;; ert-gwt.el ends here
