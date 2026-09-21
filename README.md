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

(gwt-deftest
  (:given ((user (make-user "alice" "secret")))
   (activate-user user))
  (:when  (login user))
  (:then  (login-redirects-to-home-p user)))
```

The macro generates a unique ERT test name for you. You focus on the three
clauses.

## Features

- **Anonymous by default.** No name required. The macro generates
  `test-gwt-N` or derives a name from `:describe`.
- **GWT as structure, not strings.** `:given`, `:when`, `:then` are
  syntactic clauses, not parsed text. No `.feature` files, no regex step
  matching.
- **Built on ERT.** Uses `ert-deftest` and `should`. Your existing ERT
  filters, batch runners, and CI integration keep working.
- **Zero dependencies.** No Buttercup, no Ecukes, no Cask. One `defmacro`
  plus a counter.
- **Optional `:describe`.** If you want a readable name in the test report,
  add it. Otherwise, the macro stays out of your way.

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
(gwt-deftest
  (:given ((user (make-user "alice" "secret"))))
  (:when  (login user))
  (:then  (login-redirects-to-home-p user)))
```

Expands to:

```elisp
(ert-deftest test-gwt-1 ()
  "anonymous GWT test"
  (let ((user (make-user "alice" "secret")))
    (login user)
    (should (login-redirects-to-home-p user))))
```

### With `:describe`

```elisp
(gwt-deftest
  (:describe "login redirects to home")
  (:given ((user (make-user "alice" "secret"))))
  (:when  (login user))
  (:then  (login-redirects-to-home-p user)))
```

The test name becomes `test-login-redirects-to-home`, which is more
readable in ERT reports.

### Multiple `:then` clauses

```elisp
(gwt-deftest
  (:given ((agent (make-mail-agent))
           (mail  (make-mail :id 42))))
  (:when  (agent-delete agent mail))
  (:then  (agent-asked-confirmation-p agent))
  (:then  (not (mail-deleted-p mail))))
```

Each `:then` expression is wrapped in `should` automatically.

### Setup inside `:given`

```elisp
(gwt-deftest
  (:given ((user (make-user "alice" "secret")))
   (activate-user user)
   (setf (user-login-attempts user) 3))
  (:when  (login user))
  (:then  (account-locked-p user)))
```

`:given` supports both `let`-style bindings and setup forms.

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

4. **Anonymous by default, readable on demand.**
   You should not have to name a test to write it. But if you want a name
   in the report, `:describe` gives it to you.

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
