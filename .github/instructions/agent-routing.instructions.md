---
description: >
  Rules for launching subagents: what instruction files and source code must
  be pasted into the prompt for each subagent type so the subagent has all
  context it needs to follow project conventions.
---

# Assistant and Subagent Routing

Canonical assistant guidance now lives in `.ai/`. This Copilot instruction file
is retained as a compatibility adapter for tools that still inspect
`.github/instructions/`.

## Canonical Task Map

| Task | Canonical guidance |
|---|---|
| General routing | `AGENTS.md` |
| R coding, tidyverse, performance, visualisation | `.ai/r-coding.md` |
| R functions, roxygen2 documentation, tests | `.ai/r-functions.md` |
| Contract-first function and test work | `.ai/r-functions.md` |
| Debugging and bug fixes | `.ai/debugging.md` |
| Git, branches, worktrees, commits, review workflow | `.ai/git-workflow.md` |
| Manuscript, Markdown, and Quarto-style narrative work | `.ai/manuscript.md` |
| Review checklist | `.ai/review-checklist.md` |
| Change-review agent | `.ai/agents/changes-reviewer.agent.md` |
| Large-change planning agent | `.ai/agents/plan-large-changes.agent.md` |

## Compatibility Wrappers

- `.github/copilot-instructions.md` remains for Copilot repository-level
  discovery.
- `.github/instructions/*.instructions.md` remains for Copilot `applyTo`
  routing.
- `.ai/r-functions.md` is the canonical home for contract-first function and
  test guardrails.
- `.github/agents/*.agent.md` remains for Copilot custom-agent discovery.
- `CLAUDE.md`, `GEMINI.md`, and `.cursor/rules/*.mdc` route other tools to the
  same canonical `.ai/` files.

The `.ai/` files are the source of truth. Do not duplicate long-form policy in
adapter files.
