;;; gwt-tests.el --- Tests for ert-gwt -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `ert-gwt-deftest' and its internal helpers.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defun gwt-tests--probe ()
  "Return the answer used to check that stubs are undone."
  "original-answer")

;; Let `require' find the package root when this file is loaded directly.
(when load-file-name
  (add-to-list 'load-path
               (expand-file-name ".." (file-name-directory load-file-name))))

(require 'ert-gwt)

(defun gwt-tests--line-at (loc)
  "Return the line number of LOC's position in LOC's buffer."
  (with-current-buffer (car loc)
    (line-number-at-pos (cdr loc))))

(defun gwt-tests--defined-names (before)
  "Return names defined since BEFORE."
  (cl-set-difference
   (mapcar #'ert-test-name (ert-select-tests t t))
   before))

(defun gwt-tests--delete-tests (names)
  "Delete each ERT test named in NAMES."
  (dolist (name names)
    (when (ert-test-boundp name)
      (ert-delete-test name))))

(defun gwt-tests--kill-file-buffer (file)
  "Kill any buffer visiting FILE."
  (let ((file (file-truename file)))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (buffer-file-name buf)
                 (equal (file-truename (buffer-file-name buf)) file))
        (kill-buffer buf)))))

;;; Clause parsing

(ert-deftest gwt-tests--parse-full-clauses ()
  (let ((parsed (ert-gwt--parse-clauses
                 '((:given ((x 1)) (setq x (1+ x)))
                   (:when (ignore x))
                   (:then (not (null x)))
                   (:then t)))))
    (ert-info ("A full clause set parses into a list of givens\n plus thens")
      (should (equal (plist-get parsed :givens)
                     '((((x 1)) (setq x (1+ x))))))
      (should (= (length (plist-get parsed :thens)) 2)))))

(ert-deftest gwt-tests--parse-then-multiple-clauses ()
  "Repeated :then clauses concatenate their forms."
  (let ((parsed (ert-gwt--parse-clauses
                 '((:given ((x 1)))
                   (:when t)
                   (:then (should (= x 1)))
                   (:then (should (> x 0)))
                   (:then t)))))
    (ert-info ("Three :then clauses concatenate in order")
      (should (equal (plist-get parsed :thens)
                     '((should (= x 1)) (should (> x 0)) t))))))

(ert-deftest gwt-tests--parse-rejects-missing-when ()
  (ert-info ("Parsing clauses without :when must signal an error")
    (should-error (ert-gwt--parse-clauses '((:then t))))))

(ert-deftest gwt-tests--parse-rejects-missing-then ()
  (ert-info ("Parsing clauses without :then must signal an error")
    (should-error (ert-gwt--parse-clauses '((:when t))))))

(ert-deftest gwt-tests--parse-rejects-unknown-clause ()
  (ert-info ("Parsing an unknown :bogus clause must signal an error")
    (should-error (ert-gwt--parse-clauses
                   '((:when t) (:then t) (:bogus t))))))

(ert-deftest gwt-tests--parse-rejects-second-when ()
  (ert-info ("Parsing a second :when clause must signal an error")
    (should-error (ert-gwt--parse-clauses
                   '((:when t) (:when t) (:then t))))))

;;; Naming

(ert-deftest gwt-tests--name-rejects-describe ()
  (ert-info ("A :describe clause is rejected since :describe was removed\n and the error message mentions :describe")
    (let ((err (should-error (ert-gwt--parse-clauses
                              '((:describe "Login Redirects")
                                (:when t) (:then t))))))
      (should (string-match-p ":describe" (error-message-string err))))))

(ert-deftest gwt-tests--name-anonymous-counter ()
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

(ert-deftest gwt-tests--parse-cleanup-and-bare-given ()
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

(ert-deftest gwt-tests--deftest-cleanup-runs-on-failure ()
  (unwind-protect
      (progn
        (defvar gwt-tests--cleanup-ran)
        (let ((gwt-tests--cleanup-ran nil)
              (load-file-name nil)
              expected-name)
          (ert-info ("Evaluating ert-gwt-deftest registers the anonymous test")
            (setq expected-name
                  (eval '(ert-gwt-deftest
                           (:cleanup (setq gwt-tests--cleanup-ran t))
                           (:when (error "Boom"))
                           (:then t))
                        t))
            (should (symbolp expected-name))
            (should (ert-test-boundp expected-name)))
          (ert-info ("Running the failing test still runs :cleanup, flag should be t")
            (setq gwt-tests--cleanup-ran nil)
            (condition-case nil
                (ert-run-test (ert-get-test expected-name))
              (error nil))
            (should (eq gwt-tests--cleanup-ran t)))
          (ert-info ("The anonymous test is deleted so nothing leaks")
            (ert-delete-test expected-name)
            (should-not (ert-test-boundp expected-name)))))
    (condition-case nil
        (makunbound 'gwt-tests--cleanup-ran)
      (error nil))))

(ert-deftest gwt-tests--docstring-lists-clauses ()
  "The generated ERT documentation names the three clauses."
  (let ((load-file-name nil)
        name)
    (unwind-protect
        (progn
          (setq name
                (eval '(ert-gwt-deftest
                         (:given ((x 1)))
                         (:when (ignore x))
                         (:then (should (= x 1))))
                      t))
          (let ((text (ert-test-documentation (ert-get-test name))))
            (should (string-match-p "GIVEN: ((x 1))" text))
            (should (string-match-p "WHEN: (ignore x)" text))
            (should (string-match-p "THEN: (should (= x 1))" text))
            (should-not (string-match-p "Anonymous GWT-style" text))))
      (when (and name (ert-test-boundp name))
        (ert-delete-test name)))))

(ert-deftest gwt-tests--expand-givens-nesting ()
  "Check the body structure produced by `ert-gwt--expand-givens'."
  (let ((expanded (ert-gwt--expand-givens
                   '((((foo 1))) (((bar 2)) (setq bar (1+ bar))))
                   '(= foo bar)
                   '(t)
                   nil)))
    (should (consp expanded))
    (should (eq (car expanded) 'ert-info))
    (should (stringp (car (nth 1 expanded))))
    (let ((letf* (nth 2 expanded)))
      (should (eq (car letf*) 'cl-letf*))
      (should (equal (cadr letf*) '((foo 1) (bar 2))))
      (let ((body (cddr letf*)))
        (should (equal (car body) '(setq bar (1+ bar))))
        ;; No `:cleanup', so no `unwind-protect': Emacs 30 warns about
        ;; one with no unwind forms.
        (let* ((leaf (nth 1 body))
               (progn (nth 2 leaf)))
          (should (eq (car leaf) 'let))
          (should (eq (car progn) 'progn))
          (should (= (length progn) 4))
          (let* ((when-info (nth 1 progn))
                 (inner (nthcdr 2 when-info))
                 (cc (car inner)))
            (should (eq (car when-info) 'ert-info))
            (should (consp (nth 1 when-info)))
            (should (= (length inner) 1))
            (should (eq (car cc) 'condition-case))
            ;; The WHEN value is kept in a variable, which also keeps
            ;; Emacs 30 from warning that it is unused.
            (should (equal (nth 2 cc)
                           '(setq ert-gwt--when-value (= foo bar)))))
          (let ((then-cond (nth 2 progn)))
            (should (eq (car then-cond) 'cond)))
          (let ((summary (nth 3 progn)))
            (should (eq (car summary) 'when))))))))

(ert-deftest gwt-tests--expand-givens-cleanups-wrap ()
  "Cleanups are the unwind forms of an `unwind-protect'."
  (let ((expanded (ert-gwt--expand-givens
                   nil 't nil '((cleanup-one) (cleanup-two)))))
    (should (eq (car expanded) 'ert-info))
    (let* ((leaf (nth 2 (nth 2 expanded)))
           (up (nth 2 leaf)))
      (should (eq (car up) 'unwind-protect))
      (should (eq (car (cadr up)) 'progn))
      (should (equal (cddr up) '((cleanup-one) (cleanup-two)))))))

(ert-deftest gwt-tests--no-double-should-in-expansion ()
  "A `:then' body that already asserts is not wrapped in `should'."
  (let* ((expansion (macroexpand '(ert-gwt-deftest (:given nil) (:when nil) (:then (should-not nil)))))
         (text (format "%s" expansion)))
    (should (string-match "(should-not nil)" text))
    (should-not (string-match "(should (should-not nil))" text))))

(ert-deftest gwt-tests--temp-file-registration ()
  "`ert-gwt--temp-file' creates the file and records it for cleanup."
  (let (ert-gwt--tracked-files path)
    (unwind-protect
        (progn
          (setq path (ert-gwt--temp-file "gwt-test"))
          (should (file-exists-p path))
          (should (member path ert-gwt--tracked-files)))
      (when (and path (file-exists-p path))
        (delete-file path)))))

(ert-deftest gwt-tests--find-definition-real-source ()
  "A real ert-gwt-deftest form is found at its opening paren."
  (let* ((before (mapcar #'ert-test-name (ert-select-tests t t)))
         (file (make-temp-file "gwt-source-" nil ".el"))
         names)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; gwt-source.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert ";; A comment before the form.\n")
            (insert "(ert-gwt-deftest\n"
                    "  (:given ((x 1)))\n"
                    "  (:when (ignore x))\n"
                    "  (:then (should (= x 1))))\n"))
          (load file nil t)
          (setq names (gwt-tests--defined-names before))
          (should (= (length names) 1))
          (let* ((name (car names))
                 (loc (find-definition-noselect name 'ert--test)))
            (ert-info ("The defining file is the source file")
              (should (equal (file-truename (buffer-file-name (car loc)))
                             (file-truename file))))
            (ert-info ("Point lands on the opening paren, not point-min")
              (should (= (gwt-tests--line-at loc) 5))
              (with-current-buffer (car loc)
                (goto-char (cdr loc))
                (should (looking-at-p "(ert-gwt-deftest"))))
            (ert-info ("ert-test-location reports the same line")
              (should (string-match-p
                       ":5\\'"
                       (ert-test-location (ert-get-test name)))))
            (ert-info ("The direct find-function entry point agrees")
              (should (equal (find-function-search-for-symbol name nil file)
                             loc)))
            (ert-info ("ert-find-test-other-window uses the same finder")
              (ert-find-test-other-window name)
              (with-current-buffer (window-buffer (selected-window))
                (should (= (line-number-at-pos (point)) 5))
                (should (looking-at-p "(ert-gwt-deftest"))))))
      (gwt-tests--delete-tests names)
      (gwt-tests--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest gwt-tests--find-definition-duplicate-clauses ()
  "Two identical blocks get distinct names and their own source lines."
  (let* ((before (mapcar #'ert-test-name (ert-select-tests t t)))
         (file (make-temp-file "gwt-source-" nil ".el"))
         names)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; gwt-source.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert "(ert-gwt-deftest\n"
                    "  (:given ((x 1)))\n"
                    "  (:when (ignore x))\n"
                    "  (:then (should (= x 1))))\n\n"
                    "(ert-gwt-deftest\n"
                    "  (:given ((x 1)))\n"
                    "  (:when (ignore x))\n"
                    "  (:then (should (= x 1))))\n"))
          (load file nil t)
          (setq names (gwt-tests--defined-names before))
          (should (= (length names) 2))
          (should-not (equal (nth 0 names) (nth 1 names)))
          (let ((lines
                 (mapcar
                  (lambda (name)
                    (let ((loc (find-definition-noselect name 'ert--test)))
                      (gwt-tests--line-at loc)))
                  names)))
            (should (equal (sort lines #'<) '(4 9))))
          (dolist (name names)
            (let ((definition (get name 'ert-gwt--definition)))
              (should (equal (plist-get definition :file)
                             (file-truename file)))
              (should (memql (plist-get definition :occurrence) '(1 2))))))
      (gwt-tests--delete-tests names)
      (gwt-tests--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest gwt-tests--compiled-without-source ()
  "A compiled-only test reports the .elc and jumps with a clear error."
  (let* ((before (mapcar #'ert-test-name (ert-select-tests t t)))
         (file (make-temp-file "gwt-elconly-" nil ".el"))
         (elc (concat (file-name-sans-extension file) ".elc"))
         names name)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; gwt-elconly.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert "(ert-gwt-deftest\n"
                    "  (:given ((x 1)))\n"
                    "  (:when (ignore x))\n"
                    "  (:then (should (= x 1))))\n"))
          (should (byte-compile-file file))
          (delete-file file)
          (load elc nil t)
          (setq names (gwt-tests--defined-names before))
          (should (= (length names) 1))
          (setq name (car names))
          (should (equal (ert-test-file-name (ert-get-test name)) elc))
          (should-error (find-definition-noselect name 'ert--test)
                        :type 'user-error))
      (gwt-tests--delete-tests names)
      (gwt-tests--kill-file-buffer file)
      (gwt-tests--kill-file-buffer elc)
      (when (file-exists-p file)
        (delete-file file))
      (when (file-exists-p elc)
        (delete-file elc)))))

(ert-deftest gwt-tests--find-definition-inside-wrapper ()
  "A definition inside `progn' and after whitespace is still found."
  (let* ((before (mapcar #'ert-test-name (ert-select-tests t t)))
         (file (make-temp-file "gwt-wrapped-" nil ".el"))
         names)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; gwt-wrapped.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert "(progn\n"
                    "  (  ert-gwt-deftest\n"
                    "    (:given ((x 1)))\n"
                    "    (:when (ignore x))\n"
                    "    (:then (should (= x 1)))))\n"))
          (load file nil t)
          (setq names (gwt-tests--defined-names before))
          (should (= (length names) 1))
          (let ((loc (find-definition-noselect (car names) 'ert--test)))
            (should (= (gwt-tests--line-at loc) 5))
            (with-current-buffer (car loc)
              (goto-char (cdr loc))
              (should (looking-at-p "(  ert-gwt-deftest")))))
      (gwt-tests--delete-tests names)
      (gwt-tests--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest gwt-tests--find-definition-stale-signals ()
  "A changed clause gives a useful error instead of a silent miss."
  (let* ((before (mapcar #'ert-test-name (ert-select-tests t t)))
         (file (make-temp-file "gwt-stale-" nil ".el"))
         names name)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; gwt-stale.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert "(ert-gwt-deftest\n"
                    "  (:given ((x 1)))\n"
                    "  (:when (ignore x))\n"
                    "  (:then (should (= x 1))))\n"))
          (load file nil t)
          (setq names (gwt-tests--defined-names before))
          (should (= (length names) 1))
          (setq name (car names))
          (with-temp-file file
            (insert ";;; gwt-stale.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert "(ert-gwt-deftest\n"
                    "  (:given ((x 2)))\n"
                    "  (:when (ignore x))\n"
                    "  (:then (should (= x 2))))\n"))
          (should-error (find-definition-noselect name 'ert--test)
                        :type 'user-error))
      (gwt-tests--delete-tests names)
      (gwt-tests--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest gwt-tests--find-definition-byte-compiled ()
  "A test loaded from .elc still points at the .el form."
  (let* ((before (mapcar #'ert-test-name (ert-select-tests t t)))
         (file (make-temp-file "gwt-compiled-" nil ".el"))
         (elc (concat (file-name-sans-extension file) ".elc"))
         names)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; gwt-compiled.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert "(ert-gwt-deftest\n"
                    "  (:given ((x 1)))\n"
                    "  (:when (ignore x))\n"
                    "  (:then (should (= x 1))))\n"))
          (should (byte-compile-file file))
          (load elc nil t)
          (setq names (gwt-tests--defined-names before))
          (should (= (length names) 1))
          (let* ((name (car names))
                 (definition (get name 'ert-gwt--definition))
                 (loc (find-definition-noselect name 'ert--test)))
            (ert-info ("The compiled name uses the source file, not test-gwt")
              (should-not (string-prefix-p "test-gwt" (symbol-name name)))
              (should (string-prefix-p
                       (file-name-base file)
                       (symbol-name name))))
            (ert-info ("The recorded file is the .el source")
              (should (equal (plist-get definition :file)
                             (file-truename file))))
            (ert-info ("Finding the .elc test opens the .el form")
              (should (equal (file-truename (buffer-file-name (car loc)))
                             (file-truename file)))
              (should (= (gwt-tests--line-at loc) 4)))
            (ert-info ("ert-test-file-name reports the source file")
              (should (equal (file-truename
                              (ert-test-file-name (ert-get-test name)))
                             (file-truename file))))))
      (gwt-tests--delete-tests names)
      (gwt-tests--kill-file-buffer file)
      (dolist (path (list file elc))
        (when (file-exists-p path)
          (delete-file path))))))

(ert-deftest gwt-tests--find-definition-passes-through ()
  "The `find-function` advice leaves ordinary functions alone."
  (let ((loc (find-definition-noselect 'ert-gwt--parse-clauses nil)))
    (should loc)
    (should (integer-or-marker-p (cdr loc)))
    (should (equal (file-truename (buffer-file-name (car loc)))
                   (file-truename
                    (symbol-file 'ert-gwt--parse-clauses 'defun))))))

(ert-deftest gwt-tests--name-ignores-print-variables ()
  "The name hash covers the whole form, not a truncated print."
  (let ((clauses '((:given ((x 1 2 3 4 5))) (:when (ignore x)))))
    (let ((one (make-hash-table :test #'equal))
          (two (make-hash-table :test #'equal)))
      (should
       (equal
        (let ((print-length nil) (print-level nil))
          (ert-gwt-name-from-clauses "print" clauses one))
        (let ((print-length 2) (print-level 1))
          (ert-gwt-name-from-clauses "print" clauses two)))))))

(ert-deftest gwt-tests--name-separates-files-with-the-same-base ()
  "Equal clauses in same-named files get different names."
  (let* ((dir-a (make-temp-file "gwt-a-" t))
         (dir-b (make-temp-file "gwt-b-" t))
         (file-a (expand-file-name "same-test.el" dir-a))
         (file-b (expand-file-name "same-test.el" dir-b))
         (body (concat ";;; same-test.el --- test -*- lexical-binding: t; -*-\n"
                       "(require 'ert-gwt)\n\n"
                       "(ert-gwt-deftest\n"
                       "  (:given ((x 1)))\n"
                       "  (:when (ignore x))\n"
                       "  (:then (should (= x 1))))\n"
                       "(provide 'same-test)\n"))
         (before (mapcar #'ert-test-name (ert-select-tests t t)))
         names)
    (unwind-protect
        (progn
          (with-temp-file file-a (insert body))
          (with-temp-file file-b (insert body))
          (load file-a nil t)
          (load file-b nil t)
          (setq names
                (cl-set-difference
                 (mapcar #'ert-test-name (ert-select-tests t t))
                 before))
          (should (= (length names) 2))
          (should-not (equal (nth 0 names) (nth 1 names)))
          (dolist (name names)
            (should (string-prefix-p "same-test-" (symbol-name name)))))
      (gwt-tests--delete-tests names)
      (gwt-tests--kill-file-buffer file-a)
      (gwt-tests--kill-file-buffer file-b)
      (delete-directory dir-a t)
      (delete-directory dir-b t))))

(ert-deftest gwt-tests--name-context-follows-project-relative-path ()
  "The context ignores the checkout directory but keeps the project name."
  (let* ((base (make-temp-file "gwt-context-" t))
         (proj-a (expand-file-name "a/proj" base))
         (proj-b (expand-file-name "b/proj" base))
         (file-a (expand-file-name "test/sample.el" proj-a))
         (file-b (expand-file-name "test/sample.el" proj-b)))
    (unwind-protect
        (progn
          (dolist (proj (list proj-a proj-b))
            (make-directory (expand-file-name ".git" proj) t)
            (make-directory (expand-file-name "test" proj) t))
          (should (equal (ert-gwt-name-context file-a)
                         "proj/test/sample.el"))
          (should (equal (ert-gwt-name-context file-a)
                         (ert-gwt-name-context file-b))))
      (delete-directory base t))))

(ert-deftest gwt-tests--load-twice-replaces-test ()
  "Loading the same file twice keeps one test with one name."
  (let* ((before (mapcar #'ert-test-name (ert-select-tests t t)))
         (file (make-temp-file "gwt-reload-" nil ".el"))
         names first)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; gwt-reload.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert "(ert-gwt-deftest\n"
                    "  (:given ((x 41)))\n"
                    "  (:when (setq x (1+ x)))\n"
                    "  (:then (should (= x 42))))\n"))
          (load file nil t)
          (setq names (gwt-tests--defined-names before))
          (should (= (length names) 1))
          (setq first (car names))
          (load file nil t)
          (setq names (gwt-tests--defined-names before))
          (ert-info ("A second load replaces the first test")
            (should (= (length names) 1))
            (should (equal (car names) first))
            (should (ert-test-boundp first))
            (should (= (length (ert-select-tests t t))
                       (1+ (length before))))))
      (gwt-tests--delete-tests names)
      (gwt-tests--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest gwt-tests--eval-buffer-twice-replaces-test ()
  "Evaluating the same buffer twice keeps one test with one name."
  (let* ((before (mapcar #'ert-test-name (ert-select-tests t t)))
         (file (make-temp-file "gwt-eval-" nil ".el"))
         names first buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; gwt-eval.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert "(ert-gwt-deftest\n"
                    "  (:given ((x 10)))\n"
                    "  (:when (setq x (* x 2)))\n"
                    "  (:then (should (= x 20))))\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer (eval-buffer))
          (setq names (gwt-tests--defined-names before))
          (should (= (length names) 1))
          (setq first (car names))
          (with-current-buffer buffer (eval-buffer))
          (setq names (gwt-tests--defined-names before))
          (ert-info ("A second evaluation replaces the first test")
            (should (= (length names) 1))
            (should (equal (car names) first))
            (should (ert-test-boundp first))
            (should (= (length (ert-select-tests t t))
                       (1+ (length before))))))
      (gwt-tests--delete-tests names)
      (gwt-tests--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest gwt-tests--reload-removes-old-name ()
  "Changing a clause removes the old test instead of leaving it behind."
  (let* ((before (mapcar #'ert-test-name (ert-select-tests t t)))
         (file (make-temp-file "gwt-stale-" nil ".el"))
         names old)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; gwt-stale.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert "(ert-gwt-deftest\n"
                    "  (:given ((x 1)))\n"
                    "  (:when (ignore x))\n"
                    "  (:then (should (= x 1))))\n"))
          (load file nil t)
          (setq names (gwt-tests--defined-names before))
          (should (= (length names) 1))
          (setq old (car names))
          (with-temp-file file
            (insert ";;; gwt-stale.el --- test -*- lexical-binding: t; -*-\n")
            (insert "(require 'ert-gwt)\n\n")
            (insert "(ert-gwt-deftest\n"
                    "  (:given ((x 2)))\n"
                    "  (:when (ignore x))\n"
                    "  (:then (should (= x 2))))\n"))
          (load file nil t)
          (setq names (gwt-tests--defined-names before))
          (ert-info ("Only the new test remains")
            (should (= (length names) 1))
            (should-not (equal (car names) old))
            (should-not (ert-test-boundp old))))
      (gwt-tests--delete-tests names)
      (gwt-tests--kill-file-buffer file)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest gwt-tests--cleanup ()
  "`ert-gwt--cleanup' kills new buffers, deletes tracked files and resets the list."
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

(ert-deftest gwt-tests--name-and-cleanup-branches ()
  "Check name fallbacks, duplicate suffixes and directory cleanup."
  (ert-info ("ert-gwt--name: no source file falls back to the \"test-gwt\" prefix")
    (let (load-file-name
          (buffer-file-name nil))
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
    (let* ((dir (make-temp-file "gwt-tests-dir-" t))
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

;; The WHEN value is only echoed in the ERT status line, never used.  A
;; pure call there must not look like a discarded value to Emacs 30's
;; byte-compiler; `make lint' byte-compiles this file, so the test's real
;; assertion is that it compiles without that warning.
(ert-gwt-deftest
  (:given ((x '(:answer 42))
           (y '(:question 7))))
  (:when (list (plist-get x :answer) (plist-get y :question)))
  (:then (should (= 42 (plist-get x :answer)))))

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

(ert-gwt-deftest
  (:given (((symbol-function 'gwt-tests--probe)
            (lambda () :stub-probe))))
  (:when (gwt-tests--probe))
  (:then (should (eq (gwt-tests--probe) :stub-probe)))
  (:cleanup (ert-info ("Probe must still return the stub at cleanup")
              (should (eq (gwt-tests--probe) :stub-probe)))))

;;; Test isolation

(ert-deftest gwt-tests--side-effect-free ()
  "Running a GWT test must not leave extra buffers behind."
  (let ((before (buffer-list)))
    (unwind-protect
        (progn
          (ert-run-test
           (ert-get-test 'gwt-tests--name-rejects-describe))
          (ert-info ("Buffer-list is unchanged by the test run")
            (should (equal before (buffer-list)))))
      nil)))

(provide 'gwt-tests)

;;; gwt-tests.el ends here
