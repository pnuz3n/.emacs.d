# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

`init.org` is the primary configuration file, written in literate programming style. It is tangled to `init.el` using `org-babel-tangle`. Always edit `init.org`, not `init.el` directly.

## Tangling

After editing `init.org`, regenerate `init.el` by running `org-babel-tangle` (C-c C-v t) in Emacs on the `init.org` buffer. The `:tangle` header arg on each source block controls whether it gets included -- blocks with `:tangle no` or under `COMMENT` headings are excluded.

## Package Management

Uses **straight.el** for package management. Packages are declared with `use-package` and `:straight t`. To add a new package, add a `use-package` block in the appropriate section of `init.org`.

## Machine-Specific Configuration

Two hook points for per-machine overrides (both are untracked, never commit them):
- `local-pre-init.el` -- loaded **before** the main config. Controls AI backend selection, proxy settings, environment variables.
- `local-init.el` -- loaded **after** the main config. Machine-specific additions and overrides.

## AI Tooling

All AI keybindings live under the `C-q a` prefix (`my/ai-map`). Backend selection is controlled by variables in `local-pre-init.el`. See the "AI tool setup" section in `init.org` for full documentation and examples.

- **gptel** -- LLM chat/rewrite. Uses Bedrock or Claude API (controlled by `my/use-bedrock-gptel`).
- **gptel-autocomplete** -- Code completion. Uses Ollama or main backend (controlled by `my/use-ollama-autocomplete`).
- **ellama** -- Summarization/text improvement. Always uses Ollama.
- **aidermacs** -- Aider integration. Model set via `AIDER_MODEL` env var.
- **claude-code-ide** -- Claude Code integration via vterm.

## Custom Elisp

`lisp/` contains manually maintained mode files loaded via `load-path`, not straight.el.

## Org-mode

Org files in `~/org/` with GPG encryption (`.org.gpg`). Finnish-language tags (`tapaaminen`, `puhelu`, `muistiinpano`). GTD workflow with work (`työ`) and home context agenda views.
