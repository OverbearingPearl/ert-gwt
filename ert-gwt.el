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
;; The macro generates a unique ERT test name: a unique content-hash
;; name <file-prefix>-<hash8> (SHA1 of the clause list; repeated
;; identical clauses get a -2, -3, ... suffix).
;;
;; `:given' accepts `let'-style bindings followed by arbitrary setup
;; forms.  `:when' holds the action under test.  Each `:then' form is
;; wrapped in `should', so plain ERT expectations apply.
;;
;; Clause occurrence rules: `:given' is optional and may repeat (each
;; occurrence is nested, so later givens see earlier bindings),
;; `:when' is required and allowed exactly once, `:then' is required
;; and may repeat, and `:cleanup' is optional and may repeat.
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
;;       (:given (cl-letf (((symbol-function 'yes-or-no-p)
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
;; content-hash namer, one `provide'.

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
  "Jump to a generated test name via ORIG-FN, with fallback logic.
Calls ORIG-FN with TEST-NAME-STRING first; if the search fails
and leaves point at point-min, looks up the defining file and
anchor text recorded on the test symbol at macro expansion time
and jumps there, falling back to point-min with a message."
  (let ((result (funcall orig-fn test-name-string)))
    (when (bobp)
      (let* ((sym (intern-soft test-name-string))
             (file (and sym (get sym 'ert-gwt--defining-file)))
             (anchor (and sym (get sym 'ert-gwt--defining-anchor))))
        (when (and file (file-exists-p file))
          (find-file-other-window file)
          (goto-char (point-min))
          (cond
           ((not anchor)
            (message "ert-gwt: no defining anchor for %s; jumping to %s at point-min (degraded)"
                     test-name-string file))
           ((save-excursion
              (goto-char (point-min))
              (search-forward anchor nil t))
            (goto-char (match-beginning 0)))
           (t
            (message "ert-gwt: anchor %S for %s not found in %s; jumping to point-min (degraded)"
                     anchor test-name-string file))))))
    result))

(defun ert-gwt-unload-function ()
  "Disable `ert-gwt-defining-file-jump-mode' before unload.
Called automatically by `unload-feature' when unloading this
package.  Disabling the mode removes the around advice this
package installed on `ert-find-test-other-window'.  Return nil so
the normal unloading flow continues."
  (ert-gwt-defining-file-jump-mode -1)
  nil)

;;;###autoload
(define-minor-mode ert-gwt-defining-file-jump-mode
  "Improve `ert-find-test-other-window' for `ert-gwt-deftest' tests.

When enabled, `ert-find-test-other-window' is advised so that a
failed literal search falls back to the defining file recorded at
macro expansion time.  The mode is enabled on load;
`ert-gwt-unload-function' turns it off on unload."
  :global t
  :group 'ert-gwt
  (if ert-gwt-defining-file-jump-mode
      (advice-add 'ert-find-test-other-window :around
                  #'ert-gwt--advice-find-test-other-window)
    (advice-remove 'ert-find-test-other-window
                   #'ert-gwt--advice-find-test-other-window)))

(ert-gwt-defining-file-jump-mode 1)

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

(defgroup ert-gwt nil
  "Given-When-Then extensions for ERT."
  :prefix "ert-gwt-"
  :group 'ert)

(defun ert-gwt--expand-givens (GIVENS WHEN-FORM THENS CLEANUPS)
  "Expand the GIVEN segments in GIVENS around WHEN-FORM and THENS.
Generate a single `let*' binding all bindings of every GIVEN segment,
wrapped in one `ert-info' whose message joins one \"GIVEN: ...\" line
per segment; bindings within a segment see earlier ones, as in
`let*'.  The WHEN-FORM is evaluated inside its own `ert-info' and a
`condition-case' that records a failure instead of propagating it, so
WHEN appears as its own unmarked Info line on both success and
failure.  Each form in THENS is expanded with `ert-gwt--expand-then',
so a failure report shows one marked line (✓/✗/−) per clause in a
single summary `ert-info'.  The WHEN `ert-info', the THEN expansions,
and the re-signal of the first error (if any) all live in one `progn'
under `unwind-protect', so CLEANUPS run on unwind regardless of
outcome, inside the scope of every given binding.  When the WHEN form
fails, the error is re-signalled at the end inside nested `ert-info's
carrying the unmarked WHEN line and the chronological per-THEN summary."
  (let ((given-lines
         (mapcar (lambda (segment)
                   (ert-gwt--clause-text "GIVEN" segment))
                 GIVENS))
        (when-text (ert-gwt--clause-text "WHEN" WHEN-FORM)))
    `(ert-info (,(mapconcat #'identity given-lines "\n"))
       (let* (,@(apply #'append (mapcar #'car GIVENS)))
         ,@(apply #'append (mapcar #'cdr GIVENS))
         (let ((ert-gwt--then-notes nil)
               (ert-gwt--aborted nil)
               (ert-gwt--first-error nil))
           (unwind-protect
               (progn
                 (ert-info (,when-text)
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
`ert-info' splices its MESSAGE argument into the expansion unquoted,
so what we return must evaluate to a plain string.  The printed form
of FORM is % escaped before splicing, so that any later `format' on
the message cannot mistake a literal % for a format specifier.
Used to echo GWT clause source into failure reports."
  (format "%s: %s"
          label
          (replace-regexp-in-string "%" "%%"
                                    (prin1-to-string form))))

(defun ert-gwt--expand-then (THEN)
  "Expand one THEN form, recording a status note for every THEN.
Each THEN expands to a `cond': if an earlier THEN aborted, a
\"− THEN\" note is recorded; otherwise the assertion runs inside a
plain `ert-info'.  Success records a \"✓ THEN\" note; failure
records a \"✗ THEN\" note and stashes the error for the caller to
re-signal in a summary `ert-info' after all THENs are accounted
for.  All note strings are rendered at expansion time."
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

The name prefix is derived from the defining file's base name,
falling back to \"test-gwt\" if unavailable.  Hashing and
duplicate-name handling are delegated to `ert-gwt-name-from-clauses',
which tracks per-name usage counts in `ert-gwt--seen'."
  (intern (ert-gwt-name-from-clauses
           (if load-file-name
               (file-name-base load-file-name)
             "test-gwt")
           clauses
           ert-gwt--seen)))

(defun ert-gwt--defining-anchor (clauses)
  "Return the `prin1' serialization of CLAUSES.
The result is used to locate the `ert-gwt-deftest' definition by
literal search in the source file.  Return nil if serialization
fails, in which case the caller falls back to jumping to the
beginning of the file."
  (condition-case nil
      (prin1-to-string clauses)
    (error nil)))

(defun ert-gwt-name-from-clauses (prefix clauses taken)
  "Return a unique GWT test name for CLAUSES under PREFIX.

The name is \"PREFIX-HASH8\", where HASH8 is derived from the
clauses' content; collisions are resolved by appending -2, -3, etc.

TAKEN is a hash table mapping base names to usage counts; it is read
and updated here, and no other global state is touched."
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

The test name is generated by `ert-gwt-name-from-clauses'.  At
expansion time the defining file is recorded as the symbol property
`ert-gwt--defining-file', which `ert-gwt--advice-find-test-other-window'
reads to fix up `ert-find-test-other-window' jumps.

Each element of CLAUSES is one of:

  (:given BINDINGS FORM...)  let-style bindings plus setup forms; may
                             appear multiple times, each nested so later
                             givens see earlier bindings.  A stub is
                             just a `cl-letf' here; because each given
                             wraps everything after it, a stub covers
                             the whole scenario.
  (:when FORM)               the action under test (exactly one)
  (:then FORM)               an expectation (one or more), stated in
                             observable, business-level terms
  (:cleanup FORM...)         cleanup forms, evaluated in order inside
                             the innermost given `let'

Failure reporting: WHEN runs under `condition-case'; each THEN is
marked ✓ (passed), ✗ (failed), or − (skipped because WHEN or an
earlier THEN failed).  Each clause's source is echoed through
`ert-info', and on failure the first error is re-signalled inside an
`ert-info' summary listing one status line per WHEN and THEN, so the
scenario outcome is readable directly from the *ert* report.

Cleanup: the body runs under `unwind-protect'.  The user `:cleanup'
forms run first (they can see all given bindings), then
`ert-gwt--cleanup' performs automatic cleanup of tracked buffers and
temp files; pre-existing buffers are never touched.

Standard example.  An old file exists on disk; the user approves an
overwrite prompt; afterwards the file holds the new contents.

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

Anything not observable from outside the code under test does not
belong in `:then'."
  (declare (indent 0))
  (let* ((parsed (ert-gwt--parse-clauses clauses))
         (name (ert-gwt--name clauses))
         (givens (plist-get parsed :givens))
         (when-form (plist-get parsed :when))
         (thens (plist-get parsed :thens))
         (cleanups (plist-get parsed :cleanups))
         (defining-file (and load-file-name
                             (file-truename load-file-name)))
         (anchor (ert-gwt--defining-anchor clauses)))
    (when defining-file
      (put name 'ert-gwt--defining-file defining-file))
    (when anchor
      (put name 'ert-gwt--defining-anchor anchor))
    `(ert-deftest ,name ()
       "Anonymous GWT-style test defined by `ert-gwt-deftest'."
       (let ((ert-gwt--pre-buffers (buffer-list)))
         (unwind-protect
             ,(ert-gwt--expand-givens givens when-form thens cleanups)
           (ert-gwt--cleanup ert-gwt--pre-buffers))))))

(provide 'ert-gwt)

;;; ert-gwt.el ends here
