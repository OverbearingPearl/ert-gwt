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
;; The macro generates a unique ERT test name: either `test-gwt-N'
;; or, when a `:describe' string clause is present, a slugified
;; `test-<slug>' name that reads well in ERT reports.
;;
;; `:given' accepts `let'-style bindings followed by arbitrary setup
;; forms.  `:when' holds the action under test.  Each `:then' form is
;; wrapped in `should', so plain ERT expectations apply.
;;
;; There are no dependencies beyond ERT itself.  One macro, one
;; counter, one `provide'.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defvar ert-gwt--counter 0
  "Counter used to generate unique names for anonymous tests.")

(when (boundp 'load-file-name)
  (let ((dir (file-name-directory load-file-name)))
    (add-to-list 'load-path dir)
    (let ((lisp-dir (expand-file-name "lisp" dir)))
      (when (file-directory-p lisp-dir)
        (add-to-list 'load-path lisp-dir)))))

(defun ert-gwt--slugify (string)
  "Return a slug derived from STRING for use in a test name."
  (let ((slug (downcase string)))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug))
    slug))

(defun ert-gwt--parse-clauses (clauses)
  "Parse GWT CLAUSES into a property list of parsed parts.

Recognized clauses are `:describe' (a string), `:given' (a list of
`let'-style bindings followed by optional setup forms), `:when' (a
single form) and `:then' (one or more forms)."
  (let (describe bindings setup when when-p thens)
    (dolist (clause clauses)
      (pcase clause
        (`(:describe ,(and s (pred stringp)))
         (setq describe s))
        (`(:given . ,rest)
         (let ((first (car rest)))
           (if (and (consp first) (listp (car first)))
               (setq bindings first
                     setup (cdr rest))
             (setq bindings nil
                   setup rest))))
        (`(:when ,form)
         (when when-p
           (error "ert-gwt-deftest: Only one :when clause allowed"))
         (setq when form
               when-p t))
        (`(:then ,form)
         (push form thens))
        (_
         (error "ert-gwt-deftest: Unrecognized clause %S" clause))))
    (unless when-p
      (error "ert-gwt-deftest: Missing :when clause"))
    (unless thens
      (error "ert-gwt-deftest: Missing :then clause"))
    (list :describe describe
          :bindings bindings
          :setup setup
          :when when
          :thens (nreverse thens))))

(defun ert-gwt--name (parsed)
  "Return the ERT test name for a PARSED clause list."
  (let ((describe (plist-get parsed :describe)))
    (if describe
        (intern (format "test-%s" (ert-gwt--slugify describe)))
      (intern (format "test-gwt-%d"
                      (cl-incf ert-gwt--counter))))))

(defmacro ert-gwt-deftest (&rest clauses)
  "Define an anonymous GWT-style ERT test from CLAUSES.

Each element of CLAUSES is one of:

  (:describe STRING)         optional; derives the test name
  (:given BINDINGS FORM...)  let-style bindings plus setup forms
  (:when FORM)               the action under test (exactly one)
  (:then FORM)               an expectation (one or more)

The macro expands to an `ert-deftest' whose body binds BINDINGS,
runs the setup forms and the `:when' form, and wraps every `:then'
form in `should'."
  (declare (indent 0))
  (let* ((parsed (ert-gwt--parse-clauses clauses))
         (name (ert-gwt--name parsed))
         (doc (or (plist-get parsed :describe) "anonymous GWT test"))
         (bindings (plist-get parsed :bindings))
         (setup (plist-get parsed :setup))
         (when-form (plist-get parsed :when))
         (thens (plist-get parsed :thens)))
    `(ert-deftest ,name ()
       ,doc
       (let ,bindings
         ,@setup
         ,when-form
         ,@(mapcar (lambda (then) `(should ,then)) thens)))))

(provide 'ert-gwt)

;;; ert-gwt.el ends here
