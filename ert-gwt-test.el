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
;; Bootstrap `load-path' ... (前次编辑后函数定义紧贴注释块, 现已插入空行)

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
  (let ((ert-gwt--counter 0)
        (load-file-name nil))
    (with-temp-buffer
      (ert-info ("Anonymous :name with no load-file-name yields fallback prefix, expected test-gwt-1 then test-gwt-2")
        (let ((first (ert-gwt--name))
              (second (ert-gwt--name)))
          (should (eq first 'test-gwt-1))
          (should (eq second 'test-gwt-2)))))))

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
              (pre ert-gwt--counter))
          (eval '(ert-gwt-deftest
                   (:cleanup (setq ert-gwt-test--cleanup-ran t))
                   (:when (error "Boom"))
                   (:then t))
                t)
          (let ((expected-name (intern (format "test-gwt-%d" (1+ pre)))))
            (ert-info ("Evaluating ert-gwt-deftest registers the anonymous test")
              (should (ert-test-boundp expected-name)))
            (ert-info ("Running the failing test still runs :cleanup, flag should be t")
              (setq ert-gwt-test--cleanup-ran nil)
              (condition-case nil
                  (ert-run-test (ert-get-test expected-name))
                (error nil))
              (should (eq ert-gwt-test--cleanup-ran t)))
            (ert-info ("The anonymous test is deleted so nothing leaks")
              (ert-delete-test expected-name)
              (should-not (ert-test-boundp expected-name))))))
    (condition-case nil
        (makunbound 'ert-gwt-test--cleanup-ran)
      (error nil))))

(ert-deftest ert-gwt-test--expand-givens-nesting ()
  "Expand-givens should nest lets and wrap thens in a progn."
  (let* ((expanded (ert-gwt--expand-givens
                    '((((foo 1))) (((bar 2)) (setq bar (1+ bar))))
                    '(= foo bar)
                    '(t)))
         (outer expanded))
    (ert-info ("Outer form should begin with a let")
      (should (consp outer))
      (should (memq (car outer) '(let let*))))
    (ert-info ("First given's bindings form the outer let bindings")
      (should (equal (cadr outer) '((foo 1)))))
    (ert-info ("Inner nested form should also begin with a let")
      (let ((inner (nth 2 outer)))
        (should (consp inner))
        (should (memq (car inner) '(let let*)))
        (ert-info ("Base of inner let should be a progn wrapping when and thens")
          (let ((base (nth 3 inner)))
            (should (consp base))
            (should (eq (car base) 'progn))
            (ert-info ("Progn cadr should be the when form")
              (should (equal (cadr base) '(= foo bar))))
            (ert-info ("Progn should contain the then body")
              (should (member '(should t) (nthcdr 2 base))))))))))

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

Clear prior ERT tests, unload the package features, reset the
module variables that must be re-defined, reload the main module
and any lisp/ submodules, then load every -test.el file.  In
batch mode exit with the test result as the process status; in
interactive use open the `ert' browser for the prefix
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
    ;; Minimal reset: only variables the reload re-defvars and that
    ;; must be reset (here the name counter).  User configuration is
    ;; not touched.
    (when (boundp 'ert-gwt--counter)
      (makunbound 'ert-gwt--counter))
    (load-file main)
    (dolist (file lisp-files) (load-file file))
    (dolist (file test-files) (load-file file))
    (if noninteractive
        (ert-run-tests-batch-and-exit "test-")
      (ert "test-"))))

(provide 'ert-gwt-test)

;;; ert-gwt-test.el ends here
