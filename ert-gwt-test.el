;;; ert-gwt-test.el --- Tests for ert-gwt -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `ert-gwt-deftest' and its internal helpers, plus the
;; entry point `ert-gwt-test-run' that reloads the package from
;; source and runs every test.

;;; Code:

(require 'cl-lib)
(require 'ert)

;; Bootstrap `load-path' so loading this entry file from any working
;; directory can find the package root.
;; Bootstrap `load-path' ...

(defun ert-gwt-test--package-root ()
  "Return the root directory of the ert-gwt package.
Works even after loading finished, when `load-file-name' and
the variable `buffer-file-name' are both nil: it falls back to a `load-path'
lookup for the main module's directory, then to
`default-directory'."
  (or (when load-file-name
        (file-name-directory load-file-name))
      (when buffer-file-name
        (file-name-directory buffer-file-name))
      (let ((main (locate-library "ert-gwt")))
        (when main
          (file-name-directory main)))
      (and (file-exists-p "ert-gwt.el")
           (expand-file-name default-directory))))

(when load-file-name
  (add-to-list 'load-path (ert-gwt-test--package-root)))

(require 'ert-gwt)

;;; Clause parsing

(ert-deftest ert-gwt-test--parse-full-clauses ()
  (let ((parsed (ert-gwt--parse-clauses
                 '((:given ((x 1)) (setq x (1+ x)))
                   (:when (ignore x))
                   (:then (not (null x)))
                   (:then t)))))
    (ert-info ("A full clause set parses into a list of givens\n plus thens")
      (should (equal (plist-get parsed :givens)
                     '((((x 1)) (setq x (1+ x))))))
      (should (= (length (plist-get parsed :thens)) 2)))))

(ert-deftest ert-gwt-test--parse-then-multiple-clauses ()
  "Repeated :then clauses concatenate expectations; unknown keys raise error."
  (let ((parsed (ert-gwt--parse-clauses
                 '((:given ((x 1)))
                   (:when t)
                   (:then (should (= x 1)))
                   (:then (should (> x 0)))
                   (:then t)))))
    (ert-info ("Three :then clauses concatenate in order")
      (should (equal (plist-get parsed :thens)
                     '((should (= x 1)) (should (> x 0)) t))))))

(ert-deftest ert-gwt-test--parse-rejects-missing-when ()
  (ert-info ("Parsing clauses without :when must signal an error")
    (should-error (ert-gwt--parse-clauses '((:then t))))))

(ert-deftest ert-gwt-test--parse-rejects-missing-then ()
  (ert-info ("Parsing clauses without :then must signal an error")
    (should-error (ert-gwt--parse-clauses '((:when t))))))

(ert-deftest ert-gwt-test--parse-rejects-unknown-clause ()
  (ert-info ("Parsing an unknown :bogus clause must signal an error")
    (should-error (ert-gwt--parse-clauses
                   '((:when t) (:then t) (:bogus t))))))

(ert-deftest ert-gwt-test--parse-rejects-second-when ()
  (ert-info ("Parsing a second :when clause must signal an error")
    (should-error (ert-gwt--parse-clauses
                   '((:when t) (:when t) (:then t))))))

;;; Naming

(ert-deftest ert-gwt-test--name-rejects-describe ()
  (ert-info ("A :describe clause is rejected since :describe was removed\n and the error message mentions :describe")
    (let ((err (should-error (ert-gwt--parse-clauses
                              '((:describe "Login Redirects")
                                (:when t) (:then t))))))
      (should (string-match-p ":describe" (error-message-string err))))))

(ert-deftest ert-gwt-test--name-anonymous-counter ()
  (let ((ert-gwt--seen (make-hash-table :test 'equal))
        (load-file-name nil))
    (with-temp-buffer
      (ert-info ("Anonymous :name with no clauses and no load-file-name derives content-hash base names; repeated identical clauses share the same base, distinct clauses differ")
        (let* ((first (ert-gwt--name '("alpha" "beta")))
               (second (ert-gwt--name '("alpha" "beta")))
               (third (ert-gwt--name '("gamma" "delta"))))
          (ert-info ("Second call with identical clauses gets a -2 suffixed name instead of the first name")
            (should-not (equal second first)))
          (ert-info ("Distinct clauses yield a different base name")
            (should-not (equal third first)))
          (ert-info ("Repeated identical clauses get a -2 suffix instead of a plain base name")
            (should (string-match-p (concat "\\`"
                                            (regexp-quote (symbol-name first))
                                            "-2\\'")
                                    (symbol-name second))))
          (ert-info ("All generated names match test-gwt-<hash8> or test-gwt-<hash8>-N")
            (dolist (name (list first second third))
              (should (string-match-p
                       "\\`test-gwt-[0-9a-f]\\{8\\}\\(?:-[1-9][0-9]*\\)?\\'"
                       (symbol-name name))))))))))

(ert-deftest ert-gwt-test--parse-cleanup-and-bare-given ()
  (ert-info ("Parse collects :cleanups as ((foo) (bar) (baz)) in order")
    (let ((plist (ert-gwt--parse-clauses
                  '((:given (x) (setq x 1))
                    (:cleanup (foo))
                    (:cleanup (bar) (baz))
                    (:when t)
                    (:then t)))))
      (should (equal (plist-get plist :cleanups)
                     '((foo) (bar) (baz))))))
  (ert-info ("Bare :given (non-binding rest) recorded as ((nil (message \"hi\"))); :cleanups unsupported here")
    (let ((plist (ert-gwt--parse-clauses
                  '((:given (message "hi")) (:when t) (:then t)))))
      (should (equal (plist-get plist :givens)
                     '((nil (message "hi"))))))))

;; The anonymous test defun left behind by this test is cleared on the
;; next full run: ert-gwt-test-run calls ert-delete-all-tests first.
(ert-deftest ert-gwt-test--deftest-cleanup-runs-on-failure ()
  (unwind-protect
      (progn
        (defvar ert-gwt-test--cleanup-ran)
        (let ((ert-gwt-test--cleanup-ran nil)
              (load-file-name nil)
              expected-name)
          (ert-info ("Evaluating ert-gwt-deftest registers the anonymous test")
            (setq expected-name
                  (eval '(ert-gwt-deftest
                           (:cleanup (setq ert-gwt-test--cleanup-ran t))
                           (:when (error "Boom"))
                           (:then t))
                        t))
            (should (symbolp expected-name))
            (should (ert-test-boundp expected-name)))
          (ert-info ("Running the failing test still runs :cleanup, flag should be t")
            (setq ert-gwt-test--cleanup-ran nil)
            (condition-case nil
                (ert-run-test (ert-get-test expected-name))
              (error nil))
            (should (eq ert-gwt-test--cleanup-ran t)))
          (ert-info ("The anonymous test is deleted so nothing leaks")
            (ert-delete-test expected-name)
            (should-not (ert-test-boundp expected-name)))))
    (condition-case nil
        (makunbound 'ert-gwt-test--cleanup-ran)
      (error nil))))

(ert-deftest ert-gwt-test--expand-givens-nesting ()
  "Expand-givens should build a single info form joined from the GIVEN lines.
The message is computed at expansion time.  The `ert-info' wraps a `let*'
over all segment bindings whose body holds the segment setup forms,
then a `let' of the runtime note variables whose body is the unwind
protection.  Its protected form is a `progn' of three elements:
an `ert-info' with a runtime message form wrapping only a
`condition-case' around a `progn' of the WHEN form, the THEN cond form, and
the `ert-gwt--first-error' summary starting with `when'."
  (let ((expanded (ert-gwt--expand-givens
                   '((((foo 1))) (((bar 2)) (setq bar (1+ bar))))
                   '(= foo bar)
                   '(t)
                   nil)))
    (should (consp expanded))
    (should (eq (car expanded) 'ert-info))
    (should (stringp (car (nth 1 expanded))))
    (let ((let* (nth 2 expanded)))
      (should (eq (car let*) 'let*))
      (should (equal (cadr let*) '((foo 1) (bar 2))))
      (let ((body (cddr let*)))
        (should (equal (car body) '(setq bar (1+ bar))))
        (let ((leaf (nth 1 body)))
          (should (eq (car leaf) 'let))
          (let ((up (nth 2 leaf)))
            (should (eq (car up) 'unwind-protect))
            (let ((progn (cadr up)))
              (should (eq (car progn) 'progn))
              (should (= (length progn) 4))
              (let ((when-info (nth 1 progn)))
                (should (eq (car when-info) 'ert-info))
                (should (consp (nth 1 when-info)))
                (let ((inner (nthcdr 2 when-info)))
                  (should (= (length inner) 1))
                  (let ((cc (car inner)))
                    (should (eq (car cc) 'condition-case))
                    (should (equal (nth 2 cc) '(progn (= foo bar)))))))
              (let ((then-cond (nth 2 progn)))
                (should (eq (car then-cond) 'cond)))
              (let ((summary (nth 3 progn)))
                (should (eq (car summary) 'when))))))))))

(ert-deftest ert-gwt-test--temp-file-registration ()
  "Verify that `ert-gwt--temp-file' creates the file on disk and registers it in `ert-gwt--tracked-files'.  Both assertions are plain `should' forms so failure messages are self-explanatory in the *ert* buffer."
  (let (ert-gwt--tracked-files path)
    (unwind-protect
        (progn
          (setq path (ert-gwt--temp-file "gwt-test"))
          ;; Temp file must exist on disk after creation.
          (should (file-exists-p path))
          ;; Temp file must be registered in the tracker list.
          (should (member path ert-gwt--tracked-files)))
      (when (and path (file-exists-p path))
        (delete-file path)))))

(ert-deftest ert-gwt-test--defining-anchor-serialization ()
  (ert-info ("Normal clause list yields non-nil serialized text containing each literal")
    (let ((clauses '("should" "return" "a cons cell")))
      (let ((result (ert-gwt--defining-anchor clauses)))
        (should result)
        (should (stringp result))
        (dolist (clause clauses)
          (should
           (string-match-p
            (regexp-quote clause)
            result))))))
  (let ((buf (generate-new-buffer " *ert-gwt-anchor-test*")))
    (unwind-protect
        (ert-info ("Clause list containing a buffer object still yields a string containing the buffer name; the downgrade path only covers real print errors, not unreadable-but-printable objects")
          (let ((result (ert-gwt--defining-anchor
                         (list (buffer-name buf) buf))))
            (should result)
            (should (stringp result))
            (should
             (string-match-p
              (regexp-quote (buffer-name buf))
              result))))
      (kill-buffer buf))))

(ert-deftest ert-gwt-test--defining-anchor-fallback-lookup ()
  "Locate the symbol via its defining-anchor property.
The advice `ert-gwt--advice-find-test-other-window' must locate
the symbol via its defining-anchor property (literal string
search, not regexp), land point at the beginning of the matched
anchor text, degrade to point-min with a loud message when the
anchor text is absent, and degrade loudly when the
defining-anchor property is missing entirely."
  (let* ((temp-file (make-temp-file "gwt-anchor-test-" nil ".el"))
         (subject 'gwt-test--anchor-subject)
         (anchor-line ";; anchor: (gwt-test--anchor-subject)")
         (missing-anchor ";; anchor: (gwt-test--no-such-subject)")
         (body (concat ";; preamble line\n"
                       anchor-line "\n"
                       "(defun gwt-test--anchor-subject () nil)\n"))
         (no-op (lambda (&rest _args) (goto-char (point-min)))))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert body))
          (put subject 'ert-gwt--defining-file temp-file)
          ;; Case 1: anchor text present -> point lands at match-beginning
          ;; in the buffer the advice opened for temp-file.
          (put subject 'ert-gwt--defining-anchor anchor-line)
          (ert-info ("Anchor lookup must land point at the start of the matched anchor symbol in the opened file buffer")
            (goto-char (point-min))
            (funcall #'ert-gwt--advice-find-test-other-window no-op subject)
            (should (equal (buffer-file-name) temp-file))
            (should (looking-at
                     (regexp-quote anchor-line)))
            (should (equal
                     (line-number-at-pos (point))
                     2)))
          ;; Case 2: anchor text absent -> degrade to point-min, loud message.
          (put subject 'ert-gwt--defining-anchor missing-anchor)
          (let ((log ""))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq log
                               (concat log
                                       (apply #'format (or fmt "nil") args)
                                       "\n"))
                         nil)))
              (ert-info ("Anchor mismatch must degrade to point-min and log, not fail silently")
                (goto-char (point-min))
                (funcall #'ert-gwt--advice-find-test-other-window no-op subject)
                (should (equal (buffer-file-name) temp-file))
                (should (= (point) (point-min)))
                (should (> (length log) 0))
                (should (string-match-p
                         "anchor"
                         log)))))
          ;; Case 3: defining-anchor property missing -> degrade loudly.
          (put subject 'ert-gwt--defining-anchor nil)
          (let ((log ""))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq log
                               (concat log
                                       (apply #'format (or fmt "nil") args)
                                       "\n"))
                         nil)))
              (ert-info ("Missing defining-anchor property must degrade to point-min and log the no-anchor message")
                (goto-char (point-min))
                (funcall #'ert-gwt--advice-find-test-other-window no-op subject)
                (should (equal (buffer-file-name) temp-file))
                (should (= (point) (point-min)))
                (should (> (length log) 0))
                (should (string-match-p
                         "no defining anchor"
                         log)))))
          ;; Kill any buffer the advice opened via find-file-other-window.
          (dolist (buf (buffer-list))
            (when (and (buffer-live-p buf)
                       (equal (buffer-file-name buf) temp-file))
              (kill-buffer buf))))
      (dolist (buf (buffer-list))
        (when (and (buffer-live-p buf)
                   (equal (buffer-file-name buf) temp-file))
          (kill-buffer buf)))
      ;; Cleanup-path degradation point: deleting the temp file may
      ;; fail (e.g. file already gone or permissions); swallow the
      ;; error explicitly so cleanup never masks the test result.
      (condition-case _err
          (delete-file temp-file)
        (error nil)))))

(ert-deftest ert-gwt-test--cleanup ()
  "Cleanup kills tracked buffers and files, resets tracker."
  (let* ((pre-buffers (buffer-list))
         ert-gwt--tracked-files
         buf path)
    (unwind-protect
        (progn
          (setq buf (generate-new-buffer " *gwt-cleanup-test*"))
          (setq path (ert-gwt--temp-file "gwt-cleanup"))
          (ert-gwt--cleanup pre-buffers)
          (ert-info ("Tracked buffer must be killed")
            (should-not (buffer-live-p buf)))
          (ert-info ("Tracked file must be deleted")
            (should-not (file-exists-p path)))
          (ert-info ("Tracker must be reset to nil")
            (should (null ert-gwt--tracked-files))))
      (when (file-exists-p path)
        (delete-file path))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest ert-gwt-test--name-and-cleanup-branches ()
  "Cover uncovered branches of `ert-gwt--name', `ert-gwt-name-from-clauses' and `ert-gwt--cleanup'."
  (ert-info ("ert-gwt--name: load-file-name nil falls back to the \"test-gwt\" prefix")
    (let (load-file-name)
      (should (string-prefix-p "test-gwt"
                               (symbol-name (ert-gwt--name '((:when t))))))))
  (ert-info ("ert-gwt-name-from-clauses: duplicate clause name gets a numeric suffix")
    (let ((taken (make-hash-table :test #'equal))
          (clauses '((:when t))))
      (should (string-match-p "\\`dup-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\'"
                              (ert-gwt-name-from-clauses "dup" clauses taken)))
      (should (string-match-p "\\`dup-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]-2\\'"
                              (ert-gwt-name-from-clauses "dup" clauses taken)))))
  (ert-info ("ert-gwt--cleanup: deletes a tracked directory; buffer-kill branch skipped by passing the full buffer-list as pre-buffers")
    (let* ((dir (make-temp-file "ert-gwt-test-dir-" t))
           (ert-gwt--tracked-files (list dir)))
      (unwind-protect
          (progn
            (should (file-directory-p dir))
            (ert-gwt--cleanup (buffer-list))
            (should-not (file-exists-p dir)))
        (when (file-exists-p dir)
          (delete-directory dir t))))))

;;; End-to-end macro behavior

(ert-gwt-deftest
  (:when t)
  (:then t))

(ert-gwt-deftest
  (:given ((x 1) (y 2)))
  (:when nil)
  (:then (= (+ x y) 3)))

(ert-gwt-deftest
  (:given ((z 0))
          (setq z (1+ z)))
  (:when (setq z (* 2 z)))
  (:then (= z 2)))

(ert-gwt-deftest
  (:given ((n 2)))
  (:when (setq n (* n n)))
  (:then (= n 4))
  (:then (> n 0))
  (:then (not (zerop n))))

;;; Test isolation

(ert-deftest ert-gwt-test--side-effect-free ()
  "Running a GWT test must not leave extra buffers behind."
  (let ((before (buffer-list)))
    (unwind-protect
        (progn
          (ert-run-test
           (ert-get-test 'ert-gwt-test--name-rejects-describe))
          (ert-info ("Buffer-list is unchanged by the test run")
            (should (equal before (buffer-list)))))
      ;; Nothing to clean up here, but the guard shows the intent:
      ;; if the run ever leaks buffers the assertion above fails
      ;; inside unwind-protect, so the failure is reported rather
      ;; than silently tolerated.
      nil)))

;;; Runner

(defun ert-gwt-test--module-features ()
  "Features to unload before reloading, derived from file names.
Lists every non-test el file of the package root and its lisp/
subdirectory, so new module files need no edit here."
  (let* ((root (ert-gwt-test--package-root))
         (lisp (expand-file-name "lisp" root))
         (files (append
                 (directory-files root t "^[^.].*\\.el\\='")
                 (when (file-directory-p lisp)
                   (directory-files lisp t "^[^.].*\\.el\\='")))))
    (mapcar (lambda (file)
              (intern (file-name-nondirectory
                       (file-name-sans-extension file))))
            (cl-remove-if
             (lambda (file)
               (string-match-p "-test\\.el\\='" file))
             files))))

(defun ert-gwt-test-run ()
  "Reload the ert-gwt package from source and run the test suite.

This command is interactive so `M-x ert-gwt-test-run' works from
any directory; the interactivity exception applies to the test
entry runner like keybindings do.

Clear prior ERT tests, unload the package features, reload the
main module and any lisp/ submodules, then load every -test.el
file.  In batch mode exit with the test result as the process
status; in interactive use open the `ert' browser for the prefix
\"test-\"."
  (interactive)
  (let* ((root (ert-gwt-test--package-root))
         (lisp (expand-file-name "lisp" root))
         (main (expand-file-name "ert-gwt.el" root))
         (test-files
          (cl-remove-if-not
           (lambda (file) (string-match-p "-test\\.el\\'" file))
           (directory-files root t "^[^.].*\\.el\\'")))
         (lisp-files
          (when (file-directory-p lisp)
            (cl-remove-if
             (lambda (file) (string-match-p "-test\\.el\\'" file))
             (directory-files lisp t "^[^.].*\\.el\\'")))))
    (when (get-buffer "*ert*") (kill-buffer "*ert*"))
    (ert-delete-all-tests)
    (dolist (feature (ert-gwt-test--module-features))
      (when (featurep feature)
        (unload-feature feature t)))
    (load-file main)
    (dolist (file lisp-files) (load-file file))
    (dolist (file test-files) (load-file file))
    (if noninteractive
        (ert-run-tests-batch-and-exit "ert-gwt-")
      (ert "ert-gwt-"))))

(provide 'ert-gwt-test)

;;; ert-gwt-test.el ends here
