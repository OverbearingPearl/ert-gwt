;;; deps.el --- Ensure required packages are available -*- lexical-binding: t; -*-

;;; Commentary:

;; package-lint is the only third-party dependency.  Prefer the user's
;; existing ELPA directory; otherwise install into a project-local
;; cache (needs network exactly once, then fully offline).

;;; Code:

(require 'package)

(defun p/try-require-package-lint ()
  "Load package-lint, returning non-nil on success."
  (condition-case nil
      (progn (require 'package-lint) t)
    (error nil)))

;; Try the user's ELPA directory first.
(let ((package-user-dir (expand-file-name "~/.emacs.d/elpa")))
  (package-initialize)
  (unless (p/try-require-package-lint)
    ;; Fall back to a project-local cache.
    (setq package-user-dir
          (or (getenv "P_ELPA_DIR")
              (expand-file-name ".cache/elpa" (p/repo-root))))
    (setq package-archives
          '(("gnu" . "https://elpa.gnu.org/packages/")
            ("melpa" . "https://melpa.org/packages/")))
    (setq package-check-signature nil)
    (package-initialize)
    (unless (p/try-require-package-lint)
      (package-refresh-contents)
      (package-install 'package-lint)
      (require 'package-lint))))
