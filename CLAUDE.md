# CLAUDE.md

Edit `init.org`, never `init.el` directly. Document configuration in org prose. Tangle with `C-c C-v t` or:

    emacs --batch -l org --eval '(org-babel-tangle-file "init.org")'

Always commit `init.el` together with `init.org` changes. Blocks with `:tangle no` or under `COMMENT` headings are excluded.

## Configuration guidelines

`use-package` with `:straight t`. Add new packages in the appropriate `init.org` section. Document the code blocks. Use literate programming style.

## Machine-Specific Config (untracked, never commit)

- `local-pre-init.el` -- loaded **before** main config (AI backends, proxy, env vars)
- `local-init.el` -- loaded **after** main config (overrides)

## AI Tooling

Keybindings under `C-q a` (`my/ai-map`). Backend selection via variables in `local-pre-init.el`. Details in the "AI tool setup" section of `init.org`.

## Directory Layout

- `lisp/` -- manually maintained elisp, loaded via `load-path`
- `sh/` -- desktop integration scripts (IntelliJ IDEA, Emacs). Not used by AI tooling.

## Org-mode

Files in `~/org/` (GPG-encrypted `.org.gpg`). Finnish-language tags. GTD workflow with work/home context views.
