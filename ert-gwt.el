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
;; Each test is named after the defining file and a hash of its clauses
;; and file identity: <file-prefix>-<hash8>, with -2, -3, ... appended
;; when a file contains identical clauses more than once.  The name is
;; ERT's symbol; it is not written into the source, so the defining form
;; is recorded on the test symbol for `find-function'.
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
(require 'subr-x)

;;; Test registry and project identity

(defvar ert-gwt--seen (make-hash-table :test 'equal)
  "Per-file test name records.
Keys are file truenames; values are hash tables mapping name bases to
records used by `ert-gwt--compute-name'.  Older callers may still
supply an integer count instead of a record.")

(defvar ert-gwt--generation 0
  "Generation number for the most recent definition pass.")

(defvar ert-gwt--reload-generation nil
  "Generation bound while a file is being loaded or evaluated.")

(defun ert-gwt--seen-clear-for-load (file)
  "Discard the name counters recorded for FILE.
Runs on `after-load-functions' so reloading produces the same names."
  (remhash (or (ert-gwt--source-file file) (file-truename file))
            ert-gwt--seen))

(defun ert-gwt--source-file (&optional file)
  "Return the source file for FILE, or for the current load context.
FILE defaults to the variable `buffer-file-name', then
`macroexp-file-name', then `load-file-name'.  A byte-compiled .elc
file maps to its .el source when that exists."
  (let ((file (or file
                 buffer-file-name
                 (and (fboundp 'macroexp-file-name)
                      (macroexp-file-name))
                 load-file-name)))
    (when file
      (setq file (file-truename file))
      (if (and (string-match "\\.elc\\'" file)
               (file-exists-p (concat (file-name-sans-extension file) ".el")))
          (concat (file-name-sans-extension file) ".el")
        file))))

(defun ert-gwt--project-root (file)
  "Return the project root directory containing FILE, or nil.
A project root is the nearest directory with a .git, .hg, Cask or
Eask marker."
  (let ((dir (file-name-directory (file-truename file))))
    (or (locate-dominating-file dir ".git")
        (locate-dominating-file dir ".hg")
        (locate-dominating-file dir "Cask")
        (locate-dominating-file dir "Eask"))))

(defun ert-gwt--git-config-file (root)
  "Return the git config file for ROOT, or nil.
ROOT may be a normal checkout or a worktree whose .git is a file."
  (let ((dotgit (expand-file-name ".git" root)))
    (cond
     ((file-directory-p dotgit)
      (let ((config (expand-file-name "config" dotgit)))
        (and (file-readable-p config) config)))
     ((file-readable-p dotgit)
      (with-temp-buffer
        (insert-file-contents dotgit)
        (goto-char (point-min))
        (when (looking-at "gitdir:[ \t]*\\(.+\\)")
          (let* ((dir (expand-file-name (string-trim (match-string 1)) root))
                 (common (expand-file-name "commondir" dir))
                 (dir (if (file-readable-p common)
                          (with-temp-buffer
                            (insert-file-contents common)
                            (expand-file-name (string-trim (buffer-string)) dir))
                        dir))
                 (config (expand-file-name "config" dir)))
            (and (file-readable-p config) config))))))))

(defun ert-gwt--git-remote (root)
  "Return the first remote URL in ROOT's git config, or nil."
  (let ((config (ert-gwt--git-config-file root)))
    (when config
      (with-temp-buffer
        (insert-file-contents config)
        (goto-char (point-min))
        (when (re-search-forward "^\\[remote \"[^\"]+\"\\]" nil t)
          (let ((end (save-excursion
                       (if (re-search-forward "^\\[" nil t)
                           (match-beginning 0)
                         (point-max)))))
            (when (re-search-forward
                   "^[ \t]*url[ \t]*=[ \t]*\\(.+\\)$" end t)
              (string-trim (match-string 1)))))))))

(defun ert-gwt--hg-remote (root)
  "Return the default path in ROOT's hgrc, or nil."
  (let ((hgrc (expand-file-name ".hg/hgrc" root)))
    (when (file-readable-p hgrc)
      (with-temp-buffer
        (insert-file-contents hgrc)
        (goto-char (point-min))
        (when (re-search-forward "^\\[paths\\]" nil t)
          (let ((end (save-excursion
                       (if (re-search-forward "^\\[" nil t)
                           (match-beginning 0)
                         (point-max)))))
            (when (re-search-forward
                   "^[ \t]*default[ \t]*=[ \t]*\\(.+\\)$" end t)
              (string-trim (match-string 1)))))))))

(defun ert-gwt--project-identity (root)
  "Return a stable identity for the project at ROOT.
A VCS remote separates unrelated projects that happen to share a
directory name.  Without one, fall back to that directory name."
  (let ((basename (file-name-nondirectory (directory-file-name root)))
        (remote (or (ert-gwt--git-remote root)
                    (ert-gwt--hg-remote root))))
    (if remote
        (format "%s@%s" basename
                (substring (secure-hash 'sha1 remote) 0 8))
      basename)))

(defun ert-gwt-name-context (file)
  "Return the hash context for test definitions in FILE.
The context is FILE's path relative to its project root, prefixed
with an identity derived from that root's VCS remote.  This keeps
names stable when the project is checked out at a different absolute
path, while still distinguishing same-named projects with different
remotes.  When no project root is found, use the parent directory
name and FILE's base name.  Projects without a VCS remote fall back
to the root's directory name."
  (if (null file)
      ""
    (let* ((true (file-truename file))
           (root (ert-gwt--project-root true)))
      (if root
          (let ((root (file-truename root)))
            (concat (ert-gwt--project-identity root)
                    "/"
                    (file-relative-name true root)))
        (concat (file-name-nondirectory
                 (directory-file-name (file-name-directory true)))
                "/"
                (file-name-nondirectory true))))))

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

;;; Finding a definition

(defconst ert-gwt--definition-wrappers
  (list 'progn (intern "eval-and-compile") 'eval-when-compile
        'cl-eval-when
        'when 'unless 'if 'cond 'and 'or 'let 'let* 'letrec 'while
        'dolist 'dotimes 'save-excursion 'save-restriction
        'unwind-protect 'condition-case)
  "Forms whose bodies run while a file is being loaded.
The definition finder and the source counter only descend into these,
so an `ert-gwt-deftest' inside a function body is not mistaken for a
top-level test.")

(defun ert-gwt--read-form ()
  "Read one form at point, reporting bad source as a `user-error'."
  (condition-case err
      (let ((read-eval nil))
        (ignore read-eval)
        (read (current-buffer)))
    (error
     (user-error "Cannot read a form in %s at %d: %s"
                 (or buffer-file-name (buffer-name))
                 (point)
                 (error-message-string err)))))

(defun ert-gwt--count-definitions (form clauses)
  "Count `ert-gwt-deftest' forms in FORM whose clauses equal CLAUSES.
Only descend into wrappers that run at load time."
  (cond
   ((not (consp form)) 0)
   ((memq (car form) '(quote function)) 0)
   ((and (eq (car form) 'ert-gwt-deftest)
         (equal (cdr form) clauses))
    1)
   ((memq (car form) ert-gwt--definition-wrappers)
    (let ((count 0))
      (dolist (part (cdr form))
        (setq count (+ count (ert-gwt--count-definitions part clauses))))
      count))
   (t 0)))

(defun ert-gwt--source-occurrence (clauses)
  "Return the one-based occurrence of CLAUSES through the form at point.
Count definitions in the current Emacs Lisp source buffer from the
beginning through the form being expanded, including a form whose
opening parenthesis is at point.  Return nil when the current buffer
is not a source file, as during `load'.  Signal a `user-error' if the
source up to that form cannot be read."
  (when (and buffer-file-name
             (derived-mode-p 'emacs-lisp-mode))
    ;; `eval-defun' expands the form with point on its opening
    ;; parenthesis, so include that form in the count.  During `load'
    ;; point is already past the form and needs no adjustment.
    (let ((limit (if (eq (char-after) ?\() (1+ (point)) (point)))
          (count 0))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) limit)
          (forward-comment (point-max))
          (when (< (point) limit)
            (setq count (+ count
                           (ert-gwt--count-definitions
                            (ert-gwt--read-form) clauses))))))
      (when (> count 0) count))))

(defun ert-gwt--scan-definitions (clauses end)
  "Return positions of CLAUSES forms before END.
Point starts at a form boundary.  Descend only into wrappers that run
at load time, so definitions inside `progn', `when', `if' and
`eval-and-compile' are found while function bodies are skipped.
Leading whitespace does not matter."
  (let (positions)
    (while (and (< (point) end)
                (progn
                  (forward-comment (point-max))
                  (and (< (point) end)
                       (not (eq (char-after) ?\))))))
      (let* ((start (point))
             (form (ert-gwt--read-form)))
        (when (and (consp form)
                   (eq (car form) 'ert-gwt-deftest)
                   (equal (cdr form) clauses))
          (push start positions))
        (when (and (consp form)
                   (memq (car form) ert-gwt--definition-wrappers))
          (let ((form-end (point)))
            (save-excursion
              (goto-char (1+ start))
              (setq positions
                    (nconc (nreverse
                            (ert-gwt--scan-definitions clauses form-end))
                           positions)))))))
    (nreverse positions)))

(defun ert-gwt--definition-positions (file clauses)
  "Return the positions of CLAUSES in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (save-restriction
        (widen)
        (with-syntax-table emacs-lisp-mode-syntax-table
          (goto-char (point-min))
          (ert-gwt--scan-definitions clauses (point-max)))))))

(defun ert-gwt--find-definition (symbol)
  "Return (BUFFER . POSITION) for SYMBOL's recorded GWT definition.
Signal a `user-error' when the source no longer contains the form."
  (let* ((definition (get symbol 'ert-gwt--definition))
         (file (plist-get definition :file))
         (clauses (plist-get definition :clauses))
         (occurrence (or (plist-get definition :occurrence) 1)))
    (unless (and file clauses)
      (user-error "No recorded source definition for %s" symbol))
    (unless (and (file-readable-p file)
                 (not (string-match "\\.elc\\'" file)))
      (let ((load-file (plist-get definition :load-file)))
        (if (and load-file (file-readable-p load-file))
            (user-error
             "The source file %s is missing; %s was loaded from %s"
             file symbol load-file)
          (user-error "The source file %s is missing" file))))
    (let* ((positions (ert-gwt--definition-positions file clauses))
           (position (nth (1- occurrence) positions))
           (count (length positions)))
      (unless position
        (user-error
         "The definition of %s is stale; %s has %d matching form%s"
         symbol file count (if (= count 1) "" "s")))
      (cons (find-file-noselect file) position))))

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

;;; Reloading and forgetting

(defun ert-gwt--forget-definition (name)
  "Remove NAME's ERT test and recorded GWT definition."
  (when (ert-test-boundp name)
    (ert-make-test-unbound name))
  (put name 'ert-gwt--definition nil))

(defun ert-gwt--delete-stale-tests (file generation)
  "Delete from FILE any test definition older than GENERATION."
  (let ((file (file-truename file)))
    (dolist (test (ert-select-tests t t))
      (let* ((name (ert-test-name test))
             (definition (get name 'ert-gwt--definition)))
        (when (and definition
                   (or (equal (plist-get definition :file) file)
                       (equal (plist-get definition :load-file) file))
                   (not (equal (plist-get definition :generation) generation)))
          (ert-gwt--forget-definition name))))))

(defun ert-gwt--advice-load (orig file &rest args)
  "Load FILE, dropping definitions that are no longer present.
ORIG and ARGS are the original `load' call.  The generation is
incremented so old tests from the same file can be recognized even
when their clauses changed the name."
  (let* ((source (and file (ert-gwt--source-file file)))
         (generation (1+ ert-gwt--generation))
         (ert-gwt--reload-generation generation)
         (result (apply orig file args)))
    (setq ert-gwt--generation generation)
    (when source
      (ert-gwt--delete-stale-tests source generation))
    result))

(defun ert-gwt--advice-eval-buffer
    (orig &optional buffer printflag filename unibyte do-allow-print)
  "Evaluate BUFFER, dropping definitions that are no longer present.
ORIG, PRINTFLAG, FILENAME, UNIBYTE and DO-ALLOW-PRINT are passed
through.  A full buffer evaluation is treated like a load, so changed
clauses do not leave old tests."
  (let* ((target (or (and buffer (get-buffer buffer)) (current-buffer)))
         (file (or filename (buffer-file-name target)))
         (source (and file (ert-gwt--source-file file)))
         (generation (1+ ert-gwt--generation))
         (ert-gwt--reload-generation generation)
         (result (funcall orig buffer printflag filename unibyte do-allow-print)))
    (setq ert-gwt--generation generation)
    (when source
      (ert-gwt--delete-stale-tests source generation))
    result))

;;; ERT output

(defconst ert-gwt--xref-name-regexp
  "[[:alnum:]_@.+-]*[[:alnum:]]"
  "Regexp matching a generated test name in ERT condition text.
The final alphanumeric character keeps a sentence period or comma out
of the match, while the earlier characters allow names derived from
files such as `foo.bar-tests.el'.")

(defun ert-gwt--advice-make-xrefs-region (orig begin end)
  "Call ORIG, then link GWT test names between BEGIN and END.
ERT only links names that have a `defun' definition; generated tests
have only an `ert--test' definition, so they are skipped otherwise."
  (funcall orig begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward ert-gwt--xref-name-regexp end t)
      (let* ((start (match-beginning 0))
             (symbol (intern-soft (match-string-no-properties 0)))
             (definition (and symbol (get symbol 'ert-gwt--definition)))
             (file (plist-get definition :file)))
        (when (and file (not (button-at start)))
          (goto-char start)
          (help-xref-button 0 'help-function-def symbol file))))))

(defun ert-gwt--advice-test-file-name (orig test)
  "Call ORIG for TEST, preferring a readable source file.
A test loaded from a byte-compiled file reports its source when that
exists, and the loaded file when it does not."
  (let ((file (funcall orig test)))
    (if (not (ert-test-p test))
        file
      (let* ((name (ert-test-name test))
             (definition (get name 'ert-gwt--definition))
             (source (plist-get definition :file))
             (loaded (plist-get definition :load-file)))
        (cond ((and source (file-readable-p source)) source)
              ((and loaded (file-readable-p loaded)) loaded)
              (t (or source file)))))))

;;; Modes

(defun ert-gwt-unload-function ()
  "Undo the setup done when ert-gwt was loaded."
  (ert-gwt-auto-reload-mode -1)
  (ert-gwt-defining-file-jump-mode -1)
  nil)

;;;###autoload
(define-minor-mode ert-gwt-defining-file-jump-mode
  "Jump to `ert-gwt-deftest' definitions from ERT results.
When enabled, `find-function' uses the defining form recorded when
the test was defined, `ert-test-file-name' reports its source file,
and generated names in ERT condition text become buttons.  Reload
handling is a separate mode, `ert-gwt-auto-reload-mode'."
  :global t
  :group 'ert-gwt
  (if ert-gwt-defining-file-jump-mode
      (progn
        (advice-add 'find-function-search-for-symbol :around
                    #'ert-gwt--advice-find-function-search)
        (advice-add 'find-definition-noselect :around
                    #'ert-gwt--advice-find-definition-noselect)
        (advice-add 'ert-test-file-name :around
                    #'ert-gwt--advice-test-file-name)
        (advice-add 'ert--make-xrefs-region :around
                    #'ert-gwt--advice-make-xrefs-region))
    (advice-remove 'find-function-search-for-symbol
                   #'ert-gwt--advice-find-function-search)
    (advice-remove 'find-definition-noselect
                   #'ert-gwt--advice-find-definition-noselect)
    (advice-remove 'ert-test-file-name
                   #'ert-gwt--advice-test-file-name)
    (advice-remove 'ert--make-xrefs-region
                   #'ert-gwt--advice-make-xrefs-region)))

;;;###autoload
(define-minor-mode ert-gwt-auto-reload-mode
  "Replace GWT tests when their defining file is evaluated again.
When enabled, loading a file or evaluating a buffer unbinds tests
that no longer exist and keeps the names of tests that do, instead of
leaving a `-2' duplicate behind.  This mode does not affect
`find-function'; `ert-gwt-defining-file-jump-mode' does that."
  :global t
  :group 'ert-gwt
  (if ert-gwt-auto-reload-mode
      (progn
        (advice-add 'load :around #'ert-gwt--advice-load)
        (advice-add 'eval-buffer :around #'ert-gwt--advice-eval-buffer)
        (add-hook 'after-load-functions #'ert-gwt--seen-clear-for-load))
    (remove-hook 'after-load-functions #'ert-gwt--seen-clear-for-load)
    (advice-remove 'load #'ert-gwt--advice-load)
    (advice-remove 'eval-buffer #'ert-gwt--advice-eval-buffer)))

(defcustom ert-gwt-auto-enable-jump-mode t
  "Whether loading ert-gwt enables `ert-gwt-defining-file-jump-mode'.
Set this to nil before loading ert-gwt to keep the finder advices off."
  :type 'boolean
  :group 'ert-gwt)

(defcustom ert-gwt-auto-enable-reload-mode t
  "Whether loading ert-gwt enables `ert-gwt-auto-reload-mode'.
Set this to nil before loading ert-gwt to keep the load and
`eval-buffer' advices off."
  :type 'boolean
  :group 'ert-gwt)

(when (and (boundp 'ert-gwt-auto-enable-jump-mode)
           (symbol-value 'ert-gwt-auto-enable-jump-mode))
  (ert-gwt-defining-file-jump-mode 1))

(when (and (boundp 'ert-gwt-auto-enable-reload-mode)
           (symbol-value 'ert-gwt-auto-enable-reload-mode))
  (ert-gwt-auto-reload-mode 1))

(when (and (boundp 'load-file-name) load-file-name)
  (let ((dir (file-name-directory load-file-name)))
    (add-to-list 'load-path dir)
    (let ((lisp-dir (expand-file-name "lisp" dir)))
      (when (file-directory-p lisp-dir)
        (add-to-list 'load-path lisp-dir)))))

;;; Expansion

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

(defcustom ert-gwt-name-context-function #'ert-gwt-name-context
  "Function used to derive a test name's file context.
It is called with the defining file's truename, or nil when no file
is known, and should return a string.  The default uses a path
relative to the project root so test names survive a change of
checkout directory."
  :type 'function
  :group 'ert-gwt)

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
        ;; The WHEN value is only echoed in the ERT status line.  It is
        ;; kept rather than dropped because a discarded pure call looks
        ;; like dead code to Emacs 30's byte-compiler; `ignore' and `prog1'
        ;; both still let the warning reach the call inside.
        (steps
         `((ert-info (,when-text)
              (condition-case ert-gwt--err
                  (setq ert-gwt--when-value ,WHEN-FORM)
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

(defvar ert-gwt--when-value nil
  "The value of the most recent WHEN form.")

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

(defun ert-gwt--test-docstring (givens when-form thens)
  "Return documentation listing GIVENS, WHEN-FORM and THENS.
The clauses are already parsed and are printed for a person reading
`ert-describe-test', not escaped for an `ert-info' format string."
  (concat "Anonymous GWT test defined by `ert-gwt-deftest'.\n\n"
          (mapconcat
           (lambda (given)
             (let ((bindings (car given))
                   (forms (cdr given)))
               (concat "GIVEN: "
                       (if bindings (ert-gwt--print-form bindings) "")
                       (mapconcat
                        (lambda (form)
                          (concat "\n       " (ert-gwt--print-form form)))
                        forms ""))))
           givens "\n")
          "\nWHEN: " (ert-gwt--print-form when-form)
          "\n"
          (mapconcat (lambda (then)
                       (format "THEN: %s" (ert-gwt--print-form then)))
                     thens "\n")))

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

;;; Naming from clauses

(defun ert-gwt--name-and-occurrence (clauses &optional occurrence)
  "Return (NAME . OCCURRENCE) for CLAUSES.
OCCURRENCE, when non-nil, is the one-based position among identical
clause blocks in the defining file.  Without it, a per-file counter
supplies the position.  NAME is interned globally; the counter lives
in `ert-gwt--seen'."
  (let* ((file (ert-gwt--source-file))
         (key (or file "test-gwt"))
         (prefix (if file (file-name-base file) "test-gwt"))
         (subtable (or (gethash key ert-gwt--seen)
                       (puthash key
                                (make-hash-table :test 'equal)
                                ert-gwt--seen))))
    (let ((result (ert-gwt--compute-name
                   prefix clauses subtable
                   (funcall ert-gwt-name-context-function file)
                   occurrence)))
      (cons (intern (car result)) (cdr result)))))

(defun ert-gwt--name (clauses)
  "Return an interned test name for CLAUSES.
The prefix is the defining file's base name, or \"test-gwt\" when no
source file is known; counters are kept per file in `ert-gwt--seen'."
  (car (ert-gwt--name-and-occurrence clauses)))

(defun ert-gwt--taken-record (taken base)
  "Return BASE's record from TAKEN.
A record is a plist with :count (the largest suffix in use) and
:names (an alist of suffix strings to clauses).  An integer value is
the form older callers used and is treated as an unknown claim."
  (let ((value (gethash base taken)))
    (cond
     ((and (consp value) (eq (car value) :count)) value)
     ((integerp value) (list :count value :names nil))
     (t (list :count 0 :names nil)))))

(defun ert-gwt--suffix-number (suffix)
  "Return the numeric position encoded by SUFFIX."
  (if (string= suffix "") 1 (string-to-number (substring suffix 1))))

(defun ert-gwt--suffix-name (base number)
  "Return BASE with NUMBER appended as a name suffix."
  (if (= number 1) base (format "%s-%d" base number)))

(defun ert-gwt--claim-name (record clauses number)
  "Record CLAUSES as using NUMBER under RECORD.
Return the new name.  NUMBER is one-based, so 1 is the bare base."
  (let ((suffix (if (= number 1) "" (format "-%d" number))))
    (setf (plist-get record :count)
          (max (plist-get record :count) number))
    (push (cons suffix clauses) (plist-get record :names))
    suffix))

(defun ert-gwt--compute-name (prefix clauses taken &optional context occurrence)
  "Return (NAME . OCCURRENCE) for CLAUSES under PREFIX.
NAME is PREFIX-HASH8, with -2, -3, ... appended on collisions in
TAKEN; OCCURRENCE is the one-based count for that name.  CONTEXT is
mixed into the hash so equal clauses in different files differ.
When OCCURRENCE is non-nil, use it instead of the counter in TAKEN;
the counter is still advanced so a later counter-based call does not
reuse the same suffix.  Different clauses that happen to share the
same hash are kept apart too, instead of reusing the bare base."
  (let* ((hash
          (substring
           (secure-hash 'sha1
                        (concat (or context "")
                                "\0"
                                (ert-gwt--print-form clauses)))
           0 8))
         (base (format "%s-%s" prefix hash))
         (record (ert-gwt--taken-record taken base)))
    (if occurrence
        (let* ((number occurrence)
               (suffix (if (= number 1) "" (format "-%d" number)))
               (entry (assoc suffix (plist-get record :names)))
               (legacy (and (= number 1)
                            (> (plist-get record :count) 0)
                            (null (plist-get record :names)))))
          (cond
           ((and entry (equal (cdr entry) clauses))
            (puthash base record taken)
            (cons (concat base suffix) occurrence))
           ((or entry legacy)
            (let ((suffix (ert-gwt--claim-name
                           record clauses (1+ (plist-get record :count)))))
              (puthash base record taken)
              (cons (concat base suffix) occurrence)))
           (t
            (ert-gwt--claim-name record clauses number)
            (puthash base record taken)
            (cons (concat base suffix) occurrence))))
      (let ((number (1+ (plist-get record :count))))
        (let ((suffix (ert-gwt--claim-name record clauses number)))
          (puthash base record taken)
          (cons (concat base suffix) number))))))

(defun ert-gwt-name-from-clauses (prefix clauses taken &optional context occurrence)
  "Return a test name for CLAUSES under PREFIX.
The name is PREFIX-HASH8, with -2, -3, ... appended on collisions
within TAKEN.  TAKEN maps name bases to records used to keep names
unique and is updated in place.  CONTEXT, when non-nil, separates
equal clauses in different files; `ert-gwt-name-context' is the rule
the macro uses.  When OCCURRENCE is non-nil, use it as the suffix
position instead of the counter in TAKEN."
  (car (ert-gwt--compute-name prefix clauses taken context occurrence)))

;;; The macro

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

The test name is the defining file's base name, a hyphen, and eight
hex digits of a hash of the project-relative path and CLAUSES.  The
hash cannot be shown here because it includes the path; in a file named
`example.el' the symbol has the form `example-<hash8>'.

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
         (occurrence-hint (ert-gwt--source-occurrence clauses))
         (name-info (ert-gwt--name-and-occurrence clauses occurrence-hint))
         (name (car name-info))
         (occurrence (cdr name-info))
         (file (ert-gwt--source-file))
         (givens (plist-get parsed :givens))
         (when-form (plist-get parsed :when))
         (thens (plist-get parsed :thens))
         (cleanups (plist-get parsed :cleanups))
         (docstring (ert-gwt--test-docstring givens when-form thens)))
    `(progn
       (ert-gwt--forget-definition ',name)
       (put ',name 'ert-gwt--definition
            (list :file ,file :clauses ',clauses :occurrence ,occurrence
                  :load-file (or load-file-name buffer-file-name)
                  :generation (or ert-gwt--reload-generation
                                  ert-gwt--generation)))
       (ert-deftest ,name ()
         ,docstring
         (let ((ert-gwt--pre-buffers (buffer-list)))
           (unwind-protect
               ,(ert-gwt--expand-givens givens when-form thens cleanups)
             (ert-gwt--cleanup ert-gwt--pre-buffers))))
       (let ((test (ert-get-test ',name)))
         (when test
           (setf (ert-test-file-name test)
                 (or (and ,file (file-readable-p ,file) ,file)
                     (or load-file-name buffer-file-name ,file))))
         ',name))))

(provide 'ert-gwt)

;;; ert-gwt.el ends here
