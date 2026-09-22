;;; ert-gwt-test.el --- Tests for ert-gwt -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl

;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for `ert-gwt-deftest' and its internal helpers, plus the
;; entry point `ert-gwt-test-run' that reloads the package from
;; source and runs every test.

;;; Code:

(require 'cl-lib)
(require 'ert)

;; Bootstrap `load-path' so loading this entry file from any working
;; directory can find the package root.
(defun ert-gwt-test--package-root ()
  "Return the root directory of the ert-gwt package.
Works even after loading finished, when `load-file-name' and
`buffer-file-name' are both nil: it falls back to a `load-path'
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

;;; Slugify

(ert-deftest ert-gwt-test--slugify-basic ()
  (ert-info ("Slugify lowercases and hyphenates a plain description")
    (should (equal (ert-gwt--slugify "User logs in")
                   "user-logs-in"))))

(ert-deftest ert-gwt-test--slugify-punctuation-and-space ()
  (ert-info ("Slug of \"  Hello, World!  \" trims edges and\n collapses punctuation runs to one hyphen, expecting\n \"hello-world\"")
    (should (equal (ert-gwt--slugify "  Hello, World!  ")
                   "hello-world"))))

(ert-deftest ert-gwt-test--slugify-only-punctuation ()
  (ert-info ("Slug of an all-punctuation string is the empty string")
    (should (equal (ert-gwt--slugify "---") ""))))

(ert-deftest ert-gwt-test--slugify-mixed-case ()
  (ert-info ("Slug of \"Agent Deletes Mail\" is \"agent-deletes-mail\"")
    (should (equal (ert-gwt--slugify "Agent Deletes Mail")
                   "agent-deletes-mail"))))

;;; Clause parsing

(ert-deftest ert-gwt-test--parse-full-clauses ()
  (let ((parsed (ert-gwt--parse-clauses
                 '((:describe "a test")
                   (:given ((x 1)) (setq x (1+ x)))
                   (:when (ignore x))
                   (:then (not (null x)))
                   (:then t)))))
    (ert-info ("A full clause set parses into describe, bindings,\n setup and thens")
      (should (equal (plist-get parsed :describe) "a test"))
      (should (equal (plist-get parsed :bindings) '((x 1))))
      (should (equal (plist-get parsed :setup)
                     '((setq x (1+ x)))))
      (should (= (length (plist-get parsed :thens)) 2)))))

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

(ert-deftest ert-gwt-test--name-uses-describe-slug ()
  (let ((parsed (ert-gwt--parse-clauses
                 '((:describe "Login Redirects")
                   (:when t) (:then t)))))
    (ert-info ("A :describe clause yields the slug name\n test-login-redirects")
      (should (eq (ert-gwt--name parsed)
                  'test-login-redirects)))))

;;; End-to-end macro behavior

(ert-gwt-deftest
    (:describe "passing test without given bindings")
  (:when t)
  (:then t))

(ert-gwt-deftest
    (:describe "given bindings are visible to when and then")
  (:given ((x 1) (y 2)))
  (:when nil)
  (:then (= (+ x y) 3)))

(ert-gwt-deftest
    (:describe "given setup forms run before when")
  (:given ((z 0))
          (setq z (1+ z)))
  (:when (setq z (* 2 z)))
  (:then (= z 2)))

(ert-gwt-deftest
    (:describe "multiple then clauses all checked")
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
           (ert-get-test 'ert-gwt-test--name-uses-describe-slug))
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
  "Reload the ert-gwt package from source and run its tests.

This command is interactive so `M-x ert-gwt-test-run' works from
any directory; the interactivity exception applies to the test
entry runner like keybindings do.

Clears prior ERT tests, unloads the package features, resets the
module variables that must be re-defined, reloads the main module
and any lisp/ submodules, then loads every -test.el file.  In
batch mode exits with the test result as the process status; in
interactive use opens the `ert' browser for the prefix
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
