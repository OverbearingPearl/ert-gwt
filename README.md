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

- **Anonymous by default.** No name required. The macro generates
  `test-gwt-N`.
- **GWT as structure, not strings.** `:given`, `:when`, `:then` are
  syntactic clauses, not parsed text. No `.feature` files, no regex step
  matching.
- **Built on ERT.** Uses `ert-deftest` and `should`. Your existing ERT
  filters, batch runners, and CI integration keep working.
- **Zero dependencies.** No Buttercup, no Ecukes, no Cask. One `defmacro`
  plus a counter.

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
(ert-gwt-deftest
  (:given ((user (make-user "alice" "secret"))))
  (:when  (login user))
  (:then  (login-redirects-to-home-p user)))
```

Expands to:

```elisp
(ert-deftest test-gwt-1 ()
  "Anonymous GWT-style test defined by `ert-gwt-deftest'."
  (let ((user (make-user "alice" "secret")))
    (login user)
    (should (login-redirects-to-home-p user))))
```

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
(ert-deftest replacing-a-file ()
  (ert-gwt
   :given "a temp file holding an old draft"
   (let ((path (ert-gwt--temp-file "old draft\n"))
         (new-content "polished draft\n"))
     (cl-letf (((symbol-function 'read-string)
                (lambda (&rest _) new-content)))
       :given "the editor asks for replacement text"
       :when "I replace the file's contents"
       (ert-gwt--replace-contents path)
       :then "the file contains the new draft"
       (should (equal (ert-gwt--read path) new-content))
       :then "the old draft is gone"
       (should-not (equal (ert-gwt--read path) "old draft\n"))))))
```

Note what the example does and does not do. The stub of the collaborator's
response (`cl-letf` on `read-string`) lives inside the first `:given`, which is
legitimate because each `:given` segment expands to a `let` wrapping every
later given, the `:when`, and all `:then`s — so the stub is simply part of the
given world, and GWT never needs a mock concept of its own. The `:when` is
exactly one user action, and each `:then` states an observable outcome in the
user's language, so the test reads the way the user experiences it.

## Design principles

1. **Structure belongs in structure, not in names.**
   GWT is a shape of a test, not a naming convention. `ert-gwt` puts
   Given/When/Then where they belong: as clauses of a macro.

2. **Stay in ERT.**
   ERT is already the standard testing framework for Emacs Lisp.
   `ert-gwt` does not replace it; it extends its syntax.

3. **No framework, no dependencies.**
   One macro, one counter, one `provide`. If you can `require` something,
   you can use `ert-gwt`.

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
