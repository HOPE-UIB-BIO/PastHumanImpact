# PastHumanImpact Copilot Instructions

This file is a GitHub Copilot compatibility adapter.

Canonical assistant guidance now lives in `.ai/`, with `AGENTS.md` as the
universal entry point. Keep this file in place so Copilot continues to discover
repository-level instructions.

## Canonical Guidance

| Task | Canonical file |
|---|---|
| Universal assistant routing | `AGENTS.md` |
| R coding, tidyverse, performance, visualisation | `.ai/r-coding.md` |
| R functions, roxygen2 docs, function tests | `.ai/r-functions.md` |
| Contract-first function and test work | `.ai/r-functions.md` |
| Git, branches, worktrees, commits, review workflow | `.ai/git-workflow.md` |
| Manuscript, Markdown, and Quarto-style narrative work | `.ai/manuscript.md` |
| Debugging and bug fixes | `.ai/debugging.md` |
| Review checklist | `.ai/review-checklist.md` |
| Custom agent definitions | `.ai/agents/*.agent.md` |

## Copilot-Specific Compatibility

- `.github/instructions/*.instructions.md` files remain present for Copilot
  `applyTo` routing.
- `.ai/r-functions.md` is the canonical contract-first function and test
  guidance.
- `.github/agents/*.agent.md` files remain present for Copilot custom-agent
  discovery.
- Adapter files should stay short and point to `.ai/` so canonical guidance does
  not drift.
