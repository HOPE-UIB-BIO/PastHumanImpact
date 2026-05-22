# PastHumanImpact Agent Guide

This file is the universal entry point for coding assistants working in this
repository. Canonical guidance lives in `.ai/`; this file routes tasks to the
correct source documents.

## Required Reading

| Task | Read first |
| --- | --- |
| Any repository work | `AGENTS.md` |
| R scripts, target pipelines, data processing, and visualisation | `.ai/r-coding.md` |
| R functions, roxygen2 docs, and function tests | `.ai/r-functions.md` |
| Contract-first function and test work | `.ai/r-functions.md` |
| Manuscript, Markdown, and Quarto-style narrative work | `.ai/manuscript.md` |
| Git workflow, branch safety, worktrees, and review workflow | `.ai/git-workflow.md` |
| Debugging and bug fixes | `.ai/debugging.md` |
| Reviewing changed files | `.ai/review-checklist.md` |
| Reusable agent prompts | `.ai/agents/changes-reviewer.agent.md`, `.ai/agents/plan-large-changes.agent.md` |

## Tool Adapters

- GitHub Copilot uses files under `.github/` for instruction routing, commit
  message guidance, and custom agent discovery.
- Copilot function and test work should also read `.ai/r-functions.md`.
- Claude and Gemini use root redirect files that point back to `AGENTS.md`.
- Cursor uses `.cursor/rules/*.mdc` to route file globs back to `.ai/`.
- Other tools should load this file first and then follow the relevant `.ai/`
  links.

## Expected Entry Files

| Tool | Fresh repo root | Nested working directory |
| --- | --- | --- |
| Codex CLI | `AGENTS.md` | Nearest repo-root `AGENTS.md` |
| Claude Code | `CLAUDE.md` -> `AGENTS.md` | Repo-root `CLAUDE.md` -> `AGENTS.md` |
| Gemini | `GEMINI.md` -> `AGENTS.md` | Repo-root `GEMINI.md` -> `AGENTS.md` |
| Cursor | `.cursor/rules/*.mdc` | Repo-root `.cursor/rules/*.mdc` |
| GitHub Copilot | `.github/copilot-instructions.md` | `.github/instructions/*.instructions.md` by `applyTo` |

## Compatibility Notes

The `.ai/` files are canonical. Tool-native files are adapters and may not
provide identical runtime behavior across assistants. Keep adapters short and
point them back to `.ai/` so guidance does not drift.
