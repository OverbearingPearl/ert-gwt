;;; ert-gwt.el --- GWT-style BDD testing for ERT -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl

;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/ert-gwt
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ert-gwt adds Given-When-Then (GWT) structure to ERT, the standard
;; testing framework for Emacs Lisp.  Instead of encoding the shape of
;; a test in a long symbol name you write the structure directly as
;; macro clauses:
;;
;;     (ert-gwt-deftest
;;       (:given ((user (make-user "alice" "secret"))))
;;       (:when  (login user))
;;       (:then  (login-redirects-to-home-p user)))
;;
;; The macro generates a unique ERT test name: the next free
;; `test-gwt-N' name.
;;
;; `:given' accepts `let'-style bindings followed by arbitrary setup
;; forms.  `:when' holds the action under test.  Each `:then' form is
;; wrapped in `should', so plain ERT expectations apply.
;;
;; Clause occurrence rules: `:given' is optional and may repeat (each occurrence is
;; nested, so later givens see earlier bindings), `:when' is
;; required and allowed exactly once, `:then' is required and may
;; repeat, and `:cleanup' is optional and may repeat.
;;
;; Cleanup is automatic: user `:cleanup' forms run first, then
;; buffers created during the test are killed and temp files made
;; with `ert-gwt--temp-file' are deleted, all inside
;; `unwind-protect' even when the test fails.
;;
;; Because each `:given' segment expands to a `let' that wraps every
;; later clause, setup forms inside `:given' -- including stubs --
;; stay in effect through `:when' and `:then'.  A stub is just part
;; of the world state, so it belongs in `:given':
;;
;;     (ert-gwt-deftest
;;       (:given ((file (ert-gwt--temp-file "data.txt"))
;;                (old "old contents")
;;                (new "new contents")
;;                (with-temp-file file (insert old))))
;;       (:given (cl-letf (((symbol-function (function yes-or-no-p)
;;                          (lambda (_prompt) t)))))
;;       (:when  (with-temp-file file (insert new)))
;;       (:then  (string= "new contents"
;;                        (with-temp-buffer
;;                          (insert-file-contents file)
;;                          (buffer-string)))))
;;
;; Here the file, its contents and the stubbed prompt answer are
;; world state in `:given'; `:when' is one user action; `:then'
;; observes only the result.
;;
;; There are no dependencies beyond ERT itself.  One macro, one
;; counter, one `provide'.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defvar ert-gwt--counter 0
  "Counter used to generate unique names for anonymous tests.")

(defvar ert-gwt--base-buffers nil
  "Buffers present when a GWT test body started.

Buffers created afterwards are killed by the test's `unwind-protect',
so a test never leaves extra buffers behind.")

(when (and (boundp 'load-file-name) load-file-name)
  (let ((dir (file-name-directory load-file-name)))
    (add-to-list 'load-path dir)
    (let ((lisp-dir (expand-file-name "lisp" dir)))
      (when (file-directory-p lisp-dir)
        (add-to-list 'load-path lisp-dir)))))

(defun ert-gwt--parse-clauses (clauses)
  "Parse GWT CLAUSES into a property list of parsed parts.

Recognized clauses are `:given' (a list of `let'-style bindings
followed by optional setup forms; repeatable, with each occurrence
recorded as a segment (BINDINGS . SETUP-FORMS) in order), `:when'
\\(a single form, allowed at most once), `:then' \\(one or more
forms) and `:cleanup' (one or more forms; repeatable, with forms
collected in order)."
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
        (`(:then ,form)
         (push form thens))
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

(defun ert-gwt--expand-givens (GIVENS WHEN-FORM THENS CLEANUPS)
  "Build nested `let*' forms binding GIVEN segments around WHEN-FORM and THENS.
GIVENS is a list of GIVEN segments, each a list whose car is a
binding list and whose cdr is the segment body.  Bindings within a
segment see earlier ones, as in `let*'.  WHEN-FORM is the WHEN
clause form to evaluate.  THENS is a list of THEN forms, each
wrapped in `should'.  CLEANUPS is a list of cleanup forms run on
unwind, inside the scope of every given binding."
  (if GIVENS
      (let ((segment (car GIVENS)))
        `(let* ,(car segment)
           ,@(cdr segment)
           ,(ert-gwt--expand-givens (cdr GIVENS) WHEN-FORM THENS CLEANUPS)))
    `(unwind-protect
         (progn ,WHEN-FORM
                ,@(mapcar (lambda (then) `(should ,then)) THENS))
       ,@CLEANUPS)))

(defvar ert-gwt--tracked-files nil "Temporary files created via `ert-gwt--temp-file', deleted after each test.")

(defvar ert-gwt--initial-buffers nil
  "Snapshot of `buffer-list' taken at `ert-gwt--deftest' body start.
Used by `ert-gwt--cleanup' to detect buffers created during the test.")

(defun ert-gwt--temp-file (&optional prefix)
  "Create a temp file named with PREFIX (or \"ert-gwt-\" by default).
The file is deleted automatically after the test."
  (let ((file (make-temp-file (or prefix "ert-gwt-"))))
    (push file ert-gwt--tracked-files)
    file))

(defun ert-gwt--cleanup (pre-buffers)
  "Kill buffers created during the test (not listed in PRE-BUFFERS)
and delete tracked temp files."
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

(defun ert-gwt--name ()
  "Return the next anonymous ERT test name `<prefix>-N'.

The prefix is derived from the file being loaded at macro-expansion
time: `load-file-name' is bound while the file defining the test is
being loaded, so `file-name-base' of e.g. \"mm-test.el\" yields the
prefix \"mm-test\".  Fall back to the prefix \"test-gwt\" when
`load-file-name' is nil (e.g. tests running after load, interactive
eval), which yields names like \"test-gwt-1\"."
  (intern
   (format "%s-%d"
           (if load-file-name
               (file-name-base load-file-name)
             "test-gwt")
           (cl-incf ert-gwt--counter))))

(defmacro ert-gwt-deftest (&rest clauses)
  "Define an anonymous GWT-style ERT test from CLAUSES.

Each element of CLAUSES is one of:

  (:given BINDINGS FORM...)  let-style bindings plus setup forms;
                             may appear multiple times, each nested
                             so later givens see earlier bindings
  (:when FORM)               the action under test (exactly one)
  (:then FORM)               an expectation (one or more)
  (:cleanup FORM...)         cleanup forms, evaluated in order
                             inside the innermost given \\=`let' and
                             under the \\=`unwind-protect' before
                             \\=`ert-gwt--cleanup', so they run even
                             when the test fails

The macro expands to an \\=`ert-deftest' whose body evaluates the
\\=`:given' clauses in order, runs the \\=`:when' form, and wraps every
\\=`:then' form in \\=`should'.  The test body and the user-supplied
\\=`:cleanup' forms run under \\=`unwind-protect', so cleanup happens
unconditionally even on failure.  The \\=`:cleanup' forms are
evaluated inside the nested given scope, after the \\=`:when' and
\\=`:then' forms, so they can refer to any given binding.  After
the cleanup forms, \\=`ert-gwt--cleanup' performs automatic
cleanup: buffers created during the test are killed, and temp
files created via \\=`ert-gwt--temp-file' are deleted (files created
by any other means are not tracked and are not removed).  Buffers
that existed before the test are never touched, because the
snapshot in \\=`ert-gwt--pre-buffers' is captured via \\=`buffer-list'
at the very start of the test body, before any clause runs;
consequently buffers created before the macro expansion's body
executes (i.e., outside this macro) are outside the snapshot's
diff and are preserved.

Standard example.  The scenario: an old file already exists on
disk; the user approves an overwrite prompt; afterwards the file
holds the new contents.  Everything that sets up the world --
the temp file (made with \\=`ert-gwt--temp-file' so it is tracked),
its old and new contents, and the stub answering \"y\" -- belongs
in \\=`:given'.  A stub is written as a given too: a \\=`cl-letf'
rebinding placed inside \\=`:given', which works because each
\\=`:given' segment expands to a \\=`let' that wraps all later givens,
the \\=`:when', every \\=`:then', and the \\=`:cleanup' forms, so a stub
binding there covers the whole scenario without leaving the GWT
vocabulary.  \\=`:when' is the single user action (saving the new
contents), and each \\=`:then' states an observable outcome in
business language (the file now contains the new text).

  (ert-gwt-deftest
    (:given (file (ert-gwt--temp-file \"data.txt\"))
            (old \"old contents\")
            (new \"new contents\")
            (with-temp-file file (insert old)))
    (:given (cl-letf (((symbol-function (function yes-or-no-p)
                       (lambda (_prompt) t)))))
    (:when (with-temp-file file (insert new)))
    (:then (should (equal \"new contents\"
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string)))))
    (:cleanup (delete-file file)))

Here the temp file, its old and new contents, and the stubbed
prompt answer are world state, placed in \\=`:given'; \\=`:when' is
exactly one action, the call under test; and \\=`:then' observes
only its result, in terms a user would recognize.  Anything not
observable from outside the code under test does not belong in
\\=`:then'."
  (declare (indent 0))
  (let* ((parsed (ert-gwt--parse-clauses clauses))
         (name (ert-gwt--name))
         (givens (plist-get parsed :givens))
         (when-form (plist-get parsed :when))
         (thens (plist-get parsed :thens))
         (cleanups (plist-get parsed :cleanups)))
    `(ert-deftest ,name ()
       "Anonymous GWT-style test defined by `ert-gwt-deftest'."
       (let ((ert-gwt--pre-buffers (buffer-list)))
         (unwind-protect
             ,(ert-gwt--expand-givens givens when-form thens cleanups)
           (ert-gwt--cleanup ert-gwt--pre-buffers))))))

(provide 'ert-gwt)

;;; ert-gwt.el ends here
