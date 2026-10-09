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
;;                (new "new contents"))
;;               (with-temp-file file (insert old)))
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

(defvar ert-gwt--seen (make-hash-table :test 'equal)
  "Hash of content-hash names already used in this session.
Keys are content hashes, values are how many times each has been
used, so two test blocks with identical content in the same file
get distinct names.")

(defvar ert-gwt--base-buffers nil
  "Buffers present when a GWT test body started.

Buffers created afterwards are killed by the test's `unwind-protect',
so a test never leaves extra buffers behind.")

(defun ert-gwt--advice-find-test-other-window (orig-fn test-name-string)
  "Advice around `ert-find-test-other-window'.

Call ORIG-FN; TEST-NAME-STRING is the name of the test being
jumped to.  When the default literal search failed and left point
at `point-min' (generated test names do not appear in the source
text), look up the defining file recorded in the
`ert-gwt--defining-file' symbol property of the test name and jump
to it.  The property is written at macro expansion time."
  (let ((result (funcall orig-fn test-name-string)))
    (when (= (point) (point-min))
      (let ((file (get (intern-soft test-name-string)
                       'ert-gwt--defining-file)))
        (when (and file (file-exists-p file))
          (find-file-other-window file)
          (goto-char (point-min)))))
    result))

(advice-add 'ert-find-test-other-window :around #'ert-gwt--advice-find-test-other-window)

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

(defun ert-gwt--expand-givens (GIVENS WHEN-FORM THENS CLEANUPS)
  "Expand the GIVEN segments in GIVENS into a single binding scope.
Build a single `let*' binding the bindings of all GIVEN segments,
wrapped in one `ert-info' whose message joins one \"GIVEN: ...\" line
per segment.  Bindings within a segment see earlier ones, as in
`let*'.  WHEN-FORM is the WHEN clause form to evaluate, wrapped in its
own `ert-info' whose message is computed at expansion time by
`ert-gwt--clause-text'; the body of that `ert-info' is exactly the
`condition-case' around WHEN-FORM, so WHEN appears as its own unmarked
Info line on both success and failure.  Expand each of the THENS, a
list of THEN forms, with `ert-gwt--expand-then', so a failure report
shows one marked line (✓/✗/−) per clause in a single summary
`ert-info'.  CLEANUPS is a list of cleanup forms run on unwind, inside
the scope of every given binding.

The WHEN `ert-info', the THEN expansions, and the first-error summary
all live in the same `progn' under `unwind-protect', so CLEANUPS run
on unwind regardless of outcome.  If the WHEN form fails, the error is
remembered and the first error is re-signalled at the end inside a
nested pair of `ert-info's: the outer one carries the unmarked WHEN
line as its message, and the inner one carries the joined,
chronological summary of one ✓/✗/− line per THEN.  The notes list
starts empty and all notes are collected with inline `setq' in the
expansion; `ert-gwt--clause-text' is reused to render every line."
  (let ((given-lines
         (mapcar (lambda (segment)
                   (ert-gwt--clause-text "GIVEN" segment))
                 GIVENS)))
    `(ert-info (,(mapconcat #'identity given-lines "\n"))
       (let* (,@(apply #'append (mapcar #'car GIVENS)))
         ,@(apply #'append (mapcar #'cdr GIVENS))
         (let ((ert-gwt--then-notes nil)
               (ert-gwt--aborted nil)
               (ert-gwt--first-error nil))
           (unwind-protect
               (progn
                 (ert-info (,(ert-gwt--clause-text "WHEN" ',WHEN-FORM))
                   (condition-case ert-gwt--err
                       (progn ,WHEN-FORM)
                     (error
                      (setq ert-gwt--aborted t)
                      (setq ert-gwt--first-error ert-gwt--err))))
                 ,@(mapcar #'ert-gwt--expand-then THENS)
                 (when ert-gwt--first-error
                   (ert-info ((ert-gwt--clause-text "WHEN" ',WHEN-FORM))
                     (ert-info ((mapconcat #'identity
                                           (nreverse ert-gwt--then-notes)
                                           "\n"))
                       (signal (car ert-gwt--first-error)
                               (cdr ert-gwt--first-error))))))
             ,@CLEANUPS))))))

(defvar ert-gwt--tracked-files nil "Temporary files created via `ert-gwt--temp-file', deleted after each test.")

(defvar ert-gwt--initial-buffers nil
  "Snapshot of `buffer-list' taken at `ert-gwt--deftest' body start.
Used by `ert-gwt--cleanup' to detect buffers created during the test.")

(defvar ert-gwt--then-notes nil
  "List of collected THEN status note strings.
Dynamically bound inside the generated test body by `ert-gwt'.")

(defvar ert-gwt--aborted nil
  "Non-nil once a WHEN or a THEN step failed.
Later THEN steps are marked as not executed.  Dynamically bound
inside the generated test body by `ert-gwt'.")

(defvar ert-gwt--first-error nil
  "The first error condition captured, re-signalled in the summary.
Dynamically bound inside the generated test body by `ert-gwt'.")

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

(defun ert-gwt--clause-text (label form)
  "Return the % escaped `ert-info' message string for LABEL and FORM.
`ert-info' splices its MESSAGE argument into the expansion
unquoted, so the form spliced there must evaluate to a plain
string.  A string evaluates to itself, and the callers' wrapping
`(ert-info ,(ert-gwt--clause-text ...))' already supplies the
necessary parens; returning a quoted list would instead make
`ert-info' parse the string as the first element and the rest as
keyword arguments, signaling `Keyword argument ... not one of
\(:prefix)'.
The printed form of FORM is % escaped before splicing, because
any subsequent `format' on the message would otherwise treat %
as a format specifier.
Used to echo GWT clause source into failure reports."
  (format "%s: %s"
          label
          (replace-regexp-in-string "%" "%%"
                                    (prin1-to-string form))))

(defun ert-gwt--expand-then (THEN)
  "Expand one THEN form, recording a status note for every THEN.
An unmarked `ert-info' line labeled plainly THEN wraps the
assertion.  On success a ✓ THEN note is recorded.  On failure a
✗ THEN note is recorded, the dynamically scoped flag
`ert-gwt--aborted' is set so later THENs are marked − THEN (not
executed), and the first error is stored in
`ert-gwt--first-error' for the caller to re-signal inside a
summary `ert-info' after all THENs are accounted for.  Nothing is
re-signalled here.  The status marks appear only in the notes,
not in the ert-info line.  All note strings are computed at
expansion time; nothing is evaluated during expansion.  Notes
read e.g. \"✓ THEN: ...\"."
  (let ((info-text (ert-gwt--clause-text "THEN" THEN))
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
               (should ,THEN))
             (setq ert-gwt--then-notes
                   (cons ,ok-note ert-gwt--then-notes)))
         (error
          (setq ert-gwt--aborted t)
          (unless ert-gwt--first-error
            (setq ert-gwt--first-error ert-gwt--err))
          (setq ert-gwt--then-notes
                (cons ,fail-note ert-gwt--then-notes))))))))

(defun ert-gwt--name (clauses)
  "Return an interned ERT test name for CLAUSES.

The prefix comes from `load-file-name' (bound while the file
defining the test is being loaded): `file-name-base' of e.g.
\"mm-test.el\" yields \"mm-test\".  Fall back to \"test-gwt\"
when `load-file-name' is nil (interactive eval, tests already
loaded).

Hashing and duplicate-name bookkeeping are delegated to
`ert-gwt-name-from-clauses', which uses `ert-gwt--seen' as the
hash table holding per-name usage counts; this function only
interns the returned string."
  (intern (ert-gwt-name-from-clauses
           (if load-file-name
               (file-name-base load-file-name)
             "test-gwt")
           clauses
           ert-gwt--seen)))

(defun ert-gwt-name-from-clauses (prefix clauses taken)
  "Return a unique GWT test name for CLAUSES under PREFIX.

This is the single source of truth for the GWT naming rule, shared by
the macro and external tools (e.g. Scalpel locators).

The base name is \"PREFIX-HASH8\", where HASH8 is the first 8 hex
characters of (secure-hash \\='sha1 (format \"%S\" clauses)).  If the
base name has not been used yet (absent from TAKEN), return it and
record a count of 1.  On subsequent calls return \"PREFIX-HASH8-N\"
with N starting at 2, incrementing the stored count.

TAKEN is a hash table keyed by base-name strings with integer usage
counts; it is read and updated here.  No other global state (in
particular `ert-gwt--seen') is read or modified."
  (let* ((hash (substring (secure-hash 'sha1 (format "%S" clauses)) 0 8))
         (base (format "%s-%s" prefix hash))
         (count (gethash base taken 0)))
    (if (zerop count)
        (progn
          (puthash base 1 taken)
          base)
      (puthash base (1+ count) taken)
      (format "%s-%d" base (1+ count)))))

(defmacro ert-gwt-deftest (&rest clauses)
  "Define an anonymous GWT-style ERT test from CLAUSES.

The macro generates a unique anonymous test name using the
content-hash naming rule: the name is of the form
\\=`<file-prefix>-<hash8>\\=', where <hash8> is a content hash of the
clause list.  When identical clause bodies repeat in the same
file, a numeric suffix \\=`-2\\=', \\=`-3\\=', ... is appended to keep the
names distinct.  Nothing else from the clauses appears in the
name.  The function \\=`ert-gwt-name-from-clauses\\=' is the single
source of truth for this naming rule and is reusable by external
tools such as Scalpel locators.

Each element of CLAUSES is one of:

  (:given BINDINGS FORM...)  let-style bindings plus setup forms;
                             may appear multiple times, each nested
                             so later givens see earlier bindings
  (:when FORM)               the action under test (exactly one)
  (:then FORM)               an expectation (one or more)
  (:cleanup FORM...)         cleanup forms, evaluated in order
                             inside the innermost given \\=`let\\=' and
                             under the \\=`unwind-protect\\=' before
                             \\=`ert-gwt--cleanup\\=', so they run even
                             when the test fails

The macro expands to an \\=`ert-deftest\\=' whose body evaluates the
\\=`:given\\=' clauses in order and runs the \\=`:when\\=' form.  WHEN is
wrapped in \\=`condition-case\\=' and all THENs are reported with
Unicode status marks -- ✓ when the THEN succeeded, ✗ when it
failed, and − when it was not executed because the WHEN or an
earlier THEN failed.  Each clause's source is echoed through
\\=`ert-info\\=', and if any clause failed, the first error is
re-signalled after all THENs are accounted for, inside an
\\=`ert-info\\=' summary that lists one status line per WHEN and THEN
clause, so the whole scenario's outcome is readable directly from
the *ert* failure report, without relying on message logs.  The
test body and the user-supplied \\=`:cleanup\\=' forms run under
\\=`unwind-protect\\=', so cleanup happens unconditionally even on
failure.  The \\=`:cleanup\\=' forms are evaluated inside the nested
given scope, after the \\=`:when\\=' and \\=`:then\\=' forms, so they can
refer to any given binding.  After the cleanup forms,
\\=`ert-gwt--cleanup\\=' performs automatic cleanup: buffers created
during the test are killed, and temp files created via
\\=`ert-gwt--temp-file\\=' are deleted (files created by any other
means are not tracked and are not removed).  Buffers that existed
before the test are never touched, because the snapshot in
\\=`ert-gwt--initial-buffers\\=' is captured via \\=`buffer-list\\=' at the
very start of the test body, before any clause runs; consequently
buffers created before the macro expansion's body executes (i.e.,
outside this macro) are outside the snapshot's diff and are
preserved.

The macro also records the defining file at expansion time as a
symbol property \\=`ert-gwt--defining-file\\=' on the generated test name
symbol; the advice in \\=`ert-gwt--advice-find-test-other-window\\='
reads that property to fix up \\=`ert-find-test-other-window\\='
jumps for generated test names, which the default literal search
cannot find in the source.

Standard example.  The scenario: an old file already exists on
disk; the user approves an overwrite prompt; afterwards the file
holds the new contents.  Everything that sets up the world --
the temp file (made with \\=`ert-gwt--temp-file\\=' so it is tracked),
its old and new contents, and the stub answering \"y\" -- belongs
in \\=`:given\\='.  A stub is written as a given too: a \\=`cl-letf\\='
rebinding placed inside \\=`:given\\=', which works because each
\\=`:given\\=' segment expands to a \\=`let\\=' that wraps all later givens,
the \\=`:when\\=', every \\=`:then\\=', and the
\\=`:cleanup\\=' forms, so a stub binding there covers the whole
scenario without leaving the GWT vocabulary.  \\=`:when\\=' is the
single user action (saving the new contents), and each \\=`:then\\='
states an observable outcome in business language (the file now
contains the new text).

  (ert-gwt-deftest
    (:given ((file (ert-gwt--temp-file \"data.txt\"))
             (old \"old contents\")
             (new \"new contents\"))
            (with-temp-file file (insert old)))
    (:given (cl-letf (((symbol-function \\='yes-or-no-p)
                       (lambda (_prompt) t)))))
    (:when (with-temp-file file (insert new)))
    (:then (should (equal \"new contents\"
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string)))))
    (:cleanup (delete-file file)))

Here the temp file, its old and new contents, and the stubbed
prompt answer are world state, placed in \\=`:given\\='; \\=`:when\\=' is
exactly one action, the call under test; and \\=`:then\\=' observes
only its result, in terms a user would recognize.  Anything not
observable from outside the code under test does not belong in
\\=`:then\\='."
  (declare (indent 0))
  (let* ((parsed (ert-gwt--parse-clauses clauses))
         (name (ert-gwt--name clauses))
         (givens (plist-get parsed :givens))
         (when-form (plist-get parsed :when))
         (thens (plist-get parsed :thens))
         (cleanups (plist-get parsed :cleanups))
         (defining-file (and load-file-name
                             (file-truename load-file-name))))
    (when defining-file
      (put name 'ert-gwt--defining-file defining-file))
    `(ert-deftest ,name ()
       "Anonymous GWT-style test defined by `ert-gwt-deftest'."
       (let ((ert-gwt--pre-buffers (buffer-list)))
         (unwind-protect
             ,(ert-gwt--expand-givens givens when-form thens cleanups)
           (ert-gwt--cleanup ert-gwt--pre-buffers))))))

(provide 'ert-gwt)

;;; ert-gwt.el ends here
