# ert-gwt

GWT-style BDD testing for Emacs Lisp, built on ERT.

Write Given-When-Then tests without inventing names for every test case.
Anonymous, zero-dependency, stays inside the ERT ecosystem.

## Why

Emacs Lisp already has ERT. What it lacks is a lightweight way to express
Given-When-Then structure without introducing a new framework or creating
symbol names like:

    login-given-user-on-login-page-when-submits-valid-credentials-then-redirects-to-home

`ert-gwt` lets you write the structure instead of encoding it in the name:

```elisp
(require 'ert-gwt)

(ert-gwt-deftest
  (:given ((user (make-user "alice" "secret")))
   (activate-user user))
  (:when  (login user))
  (:then  (login-redirects-to-home-p user)))
```

The macro generates a unique ERT test name for you. You focus on the three
clauses.

## Features

- **Content-based naming.** Test names are derived from content: a file prefix plus the first 8 hex digits of the SHA-1 hash of the clause body.
  Blocks with identical content in the same file get `-2`, `-3` suffixes.
  Editing other tests never causes this test's name to drift.
- **GWT as structure, not strings.** `:given`, `:when`, `:then` are
  syntactic clauses, not parsed text. No `.feature` files, no regex step matching.
- **ERT-based.** Uses `ert-deftest` and `should`. Your existing ERT
  filters, batch runners, and CI integrations keep working.
- **Zero dependencies.** No Buttercup, Ecukes, or Cask required. One `defmacro`
  plus a hash table for name bookkeeping.

## Installation

### MELPA (once accepted)

```elisp
(use-package ert-gwt
  :ensure t)
```

### Manual

Put `ert-gwt.el` on your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/ert-gwt")
(require 'ert-gwt)
```

## Usage

### Basic

```elisp
(require 'ert-gwt)

(ert-gwt-deftest
  (:given ((user (make-user "alice" "secret"))))
  (:when (login user))
  (:then (login-redirects-to-home-p user)))
```

The macroexpansion of the above example is:

```elisp
(ert-deftest ert-gwt-3f2a9c1d ()
  (let ((ert-gwt--pre-buffers (buffer-list)))
    (unwind-protect
        (progn
          (ert-info ((ert-gwt--clause-text "GIVEN" '((user (make-user "alice" "secret")))))
            (let* ((user (make-user "alice" "secret")))
              (let ((ert-gwt--then-notes nil)
                    (ert-gwt--aborted nil)
                    (ert-gwt--first-error nil))
                (ert-info ((ert-gwt--clause-text "WHEN" '(login user)))
                  (condition-case ert-gwt--err
                      (login user)
                    (error
                     (setq ert-gwt--aborted t)
                     (unless ert-gwt--first-error
                       (setq ert-gwt--first-error ert-gwt--err)))))
                (ert-info ((ert-gwt--clause-text "THEN" '(login-redirects-to-home-p user)))
                  (cond
                   (ert-gwt--aborted
                    (setq ert-gwt--then-notes
                          (cons "− THEN: (login-redirects-to-home-p user)"
                                ert-gwt--then-notes)))
                   (t
                    (condition-case ert-gwt--err
                        (progn
                          (ert-info ((ert-gwt--clause-text "THEN" '(login-redirects-to-home-p user)))
                            (should (login-redirects-to-home-p user)))
                          (setq ert-gwt--then-notes
                                (cons "✓ THEN: (login-redirects-to-home-p user)"
                                      ert-gwt--then-notes)))
                      (error
                       (unless ert-gwt--first-error
                         (setq ert-gwt--first-error ert-gwt--err))
                       (setq ert-gwt--then-notes
                             (cons "✗ THEN: (login-redirects-to-home-p user)"
                                   ert-gwt--then-notes)))))))
                (when ert-gwt--first-error
                  (ert-info ((ert-gwt--clause-text "WHEN" '(login user)))
                    (ert-info ((concat
                                (ert-gwt--clause-text "GIVEN" '((user (make-user "alice" "secret"))))
                                "\n"
                                (mapconcat #'identity (nreverse ert-gwt--then-notes) "\n")))
                      (signal (car ert-gwt--first-error)
                              (cdr ert-gwt--first-error)))))))))
      (ert-gwt--cleanup ert-gwt--pre-buffers))))
```

Note: ERT's internal `should` machinery is abbreviated here for brevity; in the real expansion each clause body retains the full `should` forms the user wrote.

The test name is derived from a hash of the test's content and remains stable regardless of other tests being added or removed.


### Multiple `:then` clauses

```elisp
(ert-gwt-deftest
  (:given ((agent (make-mail-agent))
           (mail  (make-mail :id 42))))
  (:when  (agent-delete agent mail))
  (:then  (agent-asked-confirmation-p agent))
  (:then  (not (mail-deleted-p mail))))
```

Each `:then` expression is wrapped in `should` automatically.

### Setup inside `:given`

```elisp
(ert-gwt-deftest
  (:given ((user (make-user "alice" "secret")))
   (activate-user user)
   (setf (user-login-attempts user) 3))
  (:when  (login user))
  (:then  (account-locked-p user)))
```

`:given` supports both `let`-style bindings and setup forms.

### Clause reference

| Clause | Required? | How many |
|---|---|---|
| `:given` | optional | zero or more, nested in order |
| `:when` | required | exactly one |
| `:then` | required | one or more |
| `:cleanup` | optional | zero or more, run in order |

Automatic cleanup always runs unconditionally, even when the test
fails: buffers created during the test are killed under
`unwind-protect`, and temporary files created and tracked via
`ert-gwt--temp-file` are deleted. You may call `ert-gwt--temp-file`
inside `:given` to create temporary files that are removed
automatically afterwards.

### The standard example

```elisp
(ert-gwt-deftest
  (:given ((file (ert-gwt--temp-file "data.txt"))
           (old "old contents")
           (new "new contents"))
          (with-temp-file file (insert old)))
  (:given (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt) t)))))
  (:when (with-temp-file file (insert new)))
  (:then (should (equal "new contents"
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string)))))
  (:cleanup (delete-file file)))
```

Note what the example does and does not do. Everything that sets up
the world — the temp file (made with `ert-gwt--temp-file` so it is
tracked), its old and new contents, and the stub answering "y" —
belongs in `:given`. A stub is written as a given too: a `cl-letf`
rebinding placed inside `:given`, which works because each `:given`
segment expands to a `let` that wraps all later givens, the `:when`,
every `:then`, and the `:cleanup` forms, so the stub is simply part of
the given world and GWT never needs a mock concept of its own. The
`:when` is exactly one user action, and each `:then` states an
observable outcome in business language. The `:cleanup` clause deletes
the temp file regardless of the test's outcome.

## Design principles

1. **Structure belongs in structure, not in names.**
   GWT is a shape of a test, not a naming convention. `ert-gwt` puts
   Given/When/Then where they belong: as clauses of a macro.

2. **Stay in ERT.**
   ERT is already the standard testing framework for Emacs Lisp.
   `ert-gwt` does not replace it; it extends its syntax.

3. **No framework, no dependencies.**
   One macro, one hash table, one `provide`. If you can `require`
   something, you can use `ert-gwt`.

## Comparison

| | **ert-gwt** | Buttercup | Ecukes + Espuds |
|---|---|---|---|
| GWT as structure | ✅ | ❌ (`describe`/`it`) | ⚠️ via `.feature` |
| Natural language files | ❌ | ❌ | ✅ |
| Built on ERT | ✅ | ❌ | ❌ |
| Zero dependencies | ✅ | ❌ | ❌ |
| Anonymous tests | ✅ | ❌ | ❌ |
| Spy / Mock | ❌ | ✅ | ⚠️ |
| Nested contexts | ❌ | ✅ | ✅ |

`ert-gwt` is not trying to be Buttercup or Ecukes. It is the smallest
possible thing that adds GWT structure to ERT without leaving it.

## Use cases

- **Unit tests** for Emacs Lisp packages, where you want GWT readability
  without a full BDD framework.
- **Agent / tooling code**, where behavior is naturally described as
  Given state, When action, Then outcome.
- **Existing ERT projects**, where you want GWT syntax without migrating
  to Buttercup or Ecukes.

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).

## Contributing

Issues and pull requests are welcome. If you find a bug or want a feature,
open an issue first so we can discuss the design.

---

**ert-gwt** — GWT as structure. ERT as engine. Nothing else.
