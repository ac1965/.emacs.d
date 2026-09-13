# AGENTS.md
# ac1965/.emacs.d — Agent Instructions

## 1. Project Overview

This repository is YAMASHITA Takao's (ac1965) personal Emacs configuration.
It is a literate program: `README.org` is the single source of truth,
tangled via `org-babel-tangle` into 97 `.el` targets (88 under `lisp/`,
including `lisp/modules.el`; 7 under `personal/`; `early-init.el` and
`init.el` at the repository root) across a strict 10-layer architecture.

All prose in `README.org` is written in Japanese
(`#+LANGUAGE: ja`). English is reserved for: headings, `:CUSTOM_ID:`
values, `;;; <file>.el ends here` footers, `lexical-binding` cookies,
Copyright/Author/License headers, docstrings, code symbols, and
package names. Do not translate prose into English, and do not write
new prose in English.

### Primary source

`README.org` is executable configuration, not documentation. It contains:

- Emacs Lisp source blocks tangled to `.el` files
- `leaf` package declarations (this project uses `leaf`, not
  `use-package`)
- variables, customization, key bindings, hooks, functions, macros
- a Changelog section
- Appendix diagram sources (Graphviz `.dot` / Mermaid `.mmd`)

Before touching any Emacs configuration, locate and read the relevant
`#+begin_src emacs-lisp ... #+end_src` block in `README.org`. Never
treat a block as inert documentation.

### Derived files

`early-init.el`, `init.el`, `Makefile`, `LICENSE`, `lisp/modules.el`,
and everything under `lisp/{core,ui,auth,completion,orgx,vcs,dev,utils}/`
and `personal/` are tangled output. `ChangeLog` is likewise generated
(via `make changelog-tangle`, an `ox-ascii` export of the Changelog
subtree). Do not edit any of these by hand — including `Makefile`
itself. Edit the corresponding source block in `README.org`, then
regenerate with `make reload`.

`personal/` sits directly under `.emacs.d/`, not under `lisp/`. The
other module directories (`core/`, `ui/`, `auth/`, `completion/`,
`orgx/`, `vcs/`, `dev/`, `utils/`) live under `lisp/`
(`lisp/core/`, `lisp/ui/`, ...) — do not assume they are top-level.

---

## 2. Source of Truth

`README.org` header args currently in force:

```
#+PROPERTY: header-args:emacs-lisp :lexical t :noweb no-export :mkdirp yes :comments no
```

Note `:comments no` (not `:comments link`) — this was changed
deliberately because `:comments link` pushed the `lexical-binding`
cookie to line 2 and broke it. Do not reintroduce `:comments link`.

### Directory layout

```
.emacs.d/
├── README.org
├── Makefile        (tangled from README.org — do not hand-edit)
├── early-init.el
├── init.el
├── lisp/
│   ├── modules.el
│   ├── core/       (15 tangle targets)
│   ├── ui/         (16)
│   ├── auth/       (3)
│   ├── completion/ (12)
│   ├── orgx/       (12; 8 canonical + 4 optional)
│   ├── vcs/        (4)
│   ├── dev/        (15)
│   └── utils/      (10)
├── personal/       (7) — user/device overlay, loaded before lisp/modules.el
├── .var/           runtime state — do not delete
├── .cache/         transient — safe to delete, auto-regenerates
└── .etc/           external resources
```

`.var/`, `.cache/`, and `.etc/` (dot-prefixed) live directly under
`.emacs.d/`, alongside `lisp/` and `personal/`. The canonical
directory-ensure helper is `my/ensure-directory-exists` in
`early-init.el`.

`design_spec.org` was a companion doc that has been retired and
removed entirely: its 13 diagram `.dot` sources now live directly in
the README.org Appendix (`Appendix: 01_boot_flow.dot` through
`13_personal_override_contract.dot`), each embedded next to the
module it documents. The Makefile's `DESIGN_SPEC` variable and the
two-file `dot-tangle`/`mmd-tangle` logic it drove were deleted along
with it — do not reintroduce a `DESIGN_SPEC`-style guard, and do not
assume `design_spec.org` exists.

---

## 3. Architecture

Strict 10-layer dependency flow, top to bottom:

```
early-init → core → ui → auth → completion → orgx → vcs → dev → utils → personal
```

Invariants:

- upper layers may depend on lower layers; never the reverse
- no auto-discovery of dependencies — all side effects are explicit
- module loading is deterministic, driven by `my:modules` in
  `lisp/modules.el`
- optional extras load through `my:modules-extra`, currently
  `(ui-visual-aids orgx-typography orgx-brain orgx-citar ui-macos)`,
  set in `personal/user.el`
- `orgx-roam-ui` is deliberately excluded from the extras list by
  default — including it would force-load `org-roam`/`org` at
  startup and defeat Org's lazy loading
- some modules (`dev-lsp-eglot`, `dev-lsp-mode`, `ui-doom-modeline`,
  `ui-nano-modeline`, `ui-nano-palette`) are autoload-only, loaded on
  demand via `core-switches` / `ui-theme`, and appear in neither list
- LSP backend (`eglot` / `lsp-mode` / `lsp-bridge`) is selected via
  `core-switches` — code must stay backend-agnostic
- mail modules (`auth-mail`, `dev-mail`, `utils-notmuch`) do not exist
  in this repo; do not assume they do

---

## 4. Before Making Changes

1. Run `git status`.
2. Locate the relevant section and `#+begin_src` block in
   `README.org`.
3. Read the surrounding Org section, including the module's
   Commentary text.
4. Confirm the block's `:tangle` target and layer.
5. Check `lisp/modules.el` for load order and any layer it depends on.
6. Search `README.org` for existing configuration before adding
   anything — avoid duplicate declarations across modules.
7. Do not trust Changelog prose as ground truth. Verify claims
   against the actual source block. たかお's stated preference is to
   cross-check code before asserting facts.
8. The repository itself was recreated on 2026-09-10, with everything
   before that squashed into a single initial commit. Changelog
   entries older than that commit do not correspond 1:1 with `git
   log` history — only entries added after it do.

---

## 5. Modification Rules

### Minimal changes

Prefer the smallest change that correctly implements the request. Do
not:

- rewrite unrelated configuration
- reorganize sections without being asked
- change formatting without a functional reason
- replace working code with a "better" pattern absent justification
- remove configuration merely because an alternative seems preferable

### Coding rules (enforced; verify before submitting a change)

1. Every tangled file starts with a `lexical-binding: t` cookie.
2. The `provide` symbol matches the file name, minus `.el`.
3. Built-in packages use `:straight nil` in `leaf` declarations.
4. `leaf` keyword order: `:straight` → `:ensure` → `:after` →
   `:require` → `:pre-setq` → `:custom` → `:bind` → `:hook` →
   `:init` → `:config`.
5. `leaf :custom` cannot evaluate expressions — anything requiring
   runtime evaluation goes in `:config` with `setopt`.
6. Variable assignment: `setopt` for `defcustom`; `setq` for
   `defvar`/internal mutable state. Known `setq` exceptions (cannot
   use `setopt`): `org-agenda-files`, `org-capture-templates`,
   `org-todo-keywords`, `org-refile-targets`,
   `my:modules-extra` (must be deterministic before `defcustom`
   evaluation, set from `personal/user.el` — see the "ロード順に関する
   例外" note in the `modules.el` Design Notes). `org-roam-db-connector`
   *is* a `defcustom` and uses `setopt`; don't add it back to this
   exception list.
7. Naming: `my:` for path variables, `my/` for public commands,
   `module-` for public API, `module--` for private symbols.
8. `defun` only at module top level — never inside a `leaf` block or
   `with-eval-after-load`.
9. Every module that owns `defcustom` variables declares its own
   `defgroup`.
10. Every public `defun` carries a docstring.

### Daemon-mode pattern

- Never gate code on `(display-graphic-p)` at load time — it
  permanently breaks package loading under `emacsclient -c` daemon
  mode. Branch on `(daemonp)` to `after-make-frame-functions`
  instead.
- Code that *sets* values is safe directly in
  `after-make-frame-functions`.
- Code that *queries* frame state (colors, faces) needs an additional
  one-tick `(run-with-timer 0 nil ...)` deferral on top of that.

### Org lazy-loading

- The `orgx` layer uses `declare-function` plus a function-internal
  `require` (not `:require t`) to avoid forcing Org to load at
  startup.

---

## 6. Changelog Rules

**The Fix-ID scheme is retired (permanent, since 2026-07).** Do not
use `*** Fix <ID>: ...` headings, severity emoji (🔴🟠🟡), or
"cites the rule violated" phrasing, even if asked to follow an older
template, unless the user explicitly asks to revive the scheme.

Current format:

```org
** <Japanese description of the change>
```

Plain heading, Japanese prose, no Fix ID, no severity marker. Put the
rationale for a change in the module's own Commentary section, not in
the Changelog. History beyond that lives in `git log`.

When you finish an edit to `README.org`:

1. Append a Changelog entry in the current format.
2. Verify structural integrity: balanced `#+begin_src`/`#+end_src`,
   unique `:CUSTOM_ID:` values, paren-depth on every touched Elisp
   block.
3. Scan only inside `#+begin_src emacs-lisp ... #+end_src` blocks —
   not Changelog prose — when running emphasis/paren checks, to avoid
   double-counting.
4. Check `AGENTS.md` against the edit and update it in the same
   change if it drifted — see §10.

---

## 7. Org Markup Constraints

- Bold cannot wrap a verbatim/code span:
  `*なぜ =foo= なのか*` is invalid — the outer bold consumes the
  span. Split the emphasis instead.
- `org-emphasis-regexp-components` (needed to allow Japanese
  punctuation as emphasis border characters) must be set inside
  `(with-eval-after-load 'org ...)`, never inside a `leaf org
  :config` block.
- Diagram blocks (Graphviz/Mermaid) use `bgcolor="white"` for GitHub
  legibility.

---

## 8. Build and Verification

Use the Makefile; do not hand-tangle.

| Target | Purpose |
|---|---|
| `make tangle` | tangle `README.org` into `.el` targets |
| `make reload` | `clean` + `tangle` + `check-cookies` — preferred over plain `tangle` to avoid stale `.elc` files |
| `make lint` | runs `check-tangle` + `check-emphasis` + `check-cookies` + `check-fboundp-guards` + `checkdoc` |
| `make check-tangle` | detects src blocks that cannot inherit `:tangle` due to heading-level mistakes |
| `make check-emphasis` | detects invalid Org emphasis markup, including bold-wrapping-verbatim (Python-based, not Elisp) |
| `make check-cookies` | verifies every tangled `.el` starts with a `lexical-binding: t` cookie |
| `make check-fboundp-guards` | checks `(fboundp 'X)`-guarded calls against `#'X` references for symbols that can't actually resolve (Python-based; needs a prior tangle) |
| `make checkdoc` | Elisp docstring/style checks |
| `make package-lint` | package metadata checks (optional; requires `package-lint` on `load-path`) |

Standard workflow for a change:

1. Edit `README.org`.
2. `make reload`.
3. Restart Emacs, or reconnect with `emacsclient -c` if running as a
   daemon.
4. `make lint` before considering the change done.

---

## 9. Communication Style

たかお communicates tersely, often only in Japanese. A short
confirmation (「続ける」, a single character) means proceed
autonomously without re-confirming. Corrections arrive as raw error
messages — treat them as the specification for the fix, not as a
prompt to ask clarifying questions first.

---

## 10. Keeping AGENTS.md in Sync with README.org

**Permanent rule.** `README.org` is the single source of truth
(§1); `AGENTS.md` is a derived summary of it and drifts silently the
moment README.org changes underneath it — it did (see the 2026-09-13
sync: stale module counts, a `lisp/` layout that had moved out from
under a flat tree, a retired `DESIGN_SPEC` guard nobody removed, a
`setq` exception that had become `setopt`). Do not let it happen
again silently.

Every time `README.org` is edited — not only on large refactors —
check whether the edit invalidates any factual claim `AGENTS.md`
makes, and update `AGENTS.md` in the same change if so. Concretely,
re-check:

- Module/file counts per layer (§1, §2) and the total tangle-target
  count, whenever a module is added, removed, or moved between
  `lisp/<layer>/` and `personal/`.
- The directory-layout tree (§2) and the "Derived files" list (§1),
  whenever a top-level file or directory is added, removed, or
  relocated (e.g. something moving in or out of `lisp/`).
- The layer list and its invariants (§3) — `my:modules`,
  `my:modules-extra`, the autoload-only module list, the LSP-backend
  switch — whenever `lisp/modules.el` or `personal/user.el` changes
  what loads where.
- The numbered coding rules and their exceptions (§5) — especially
  the `setopt`/`setq` split in rule 6, since individual variables can
  cross from one to the other as the config evolves. Rules 3, 4, and
  6 are cited by number directly in `README.org` comments
  (`grep -n "コーディング規則" README.org`); do not renumber them,
  only correct their content.
- The Makefile target table (§8), whenever `Makefile`'s `lint`
  dependency list, `reload` recipe, or available targets change —
  `README.org`'s own `* Makefile` section (`:tangle Makefile`) is the
  ground truth, not the `help:` target's echo text, which can itself
  go stale.
- Any claim naming a specific file, script, or variable — verify it
  still exists (`grep`/`find`) before trusting it, per §4.7.

When in doubt about whether something drifted, verify against the
actual `README.org` content and the tangled output on disk (as this
sync did) rather than trusting either document's prose.
