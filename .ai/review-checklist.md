# Review Checklist

Canonical checklist for reviewing changes in this repository. This checklist is
extracted from the `changes-reviewer` agent workflow and is intended for humans
and assistants that need a compact validation target.

## General Review Flow

1. Identify every file created or edited in the current change.
2. Map each file to the canonical guidance that applies from `.ai/`.
3. Read the changed files and review behavior, style, documentation, tests, and
   workflow safety.
4. Report findings first, then summarize the change and any residual risk.

## Required Guidance by File Type

- All R files: `.ai/r-coding.md`.
- R functions and function tests: `.ai/r-functions.md` plus `.ai/r-coding.md`.
- Visualisation R code: `.ai/r-coding.md` visualisation section.
- Manuscript and Quarto files: `.ai/manuscript.md`, plus `.ai/r-coding.md`
  for R chunks.
- Debugging work: `.ai/debugging.md`.
- Branching, commits, worktrees, and review workflow: `.ai/git-workflow.md`.

## R Code Checks

- Script header and section headers follow project conventions.
- Lines stay within the project line-length rule.
- Indentation uses two spaces and no tabs.
- Assignment uses `<-`; RHS function calls start on a new line after assignment.
- Non-base calls use explicit namespaces.
- No `library()` or `require()` inside functions.
- Paths use `here::here()` where appropriate.
- Names follow project naming conventions.
- Use `TRUE` and `FALSE`, not `T` or `F`.

## Function and Test Checks

- Functions in `R/functions/` have roxygen2 documentation with `@param`,
  `@return`, and `@examples`.
- Function arguments are validated with project-approved patterns.
- Errors and warnings use project-approved `cli` patterns.
- Function files avoid side effects and keep one primary function per file.
- Test files are named `test-<function_name>.R` and use `testthat` conventions.
- Tests cover happy paths, edge cases, and error conditions without depending on
  implementation internals.

## Visualisation and Quarto Checks

- Visualisation code uses the project canvas and save conventions.
- Manuscript and Quarto files follow project structure, chunk, rendering, and
  narrative traceability conventions.
- Generated documentation changes do not introduce stale paths or broken links.

## Migration and Adapter Checks

- Canonical guidance lives in `.ai/`.
- Root and tool-native adapters point to existing canonical files.
- Copilot wrapper frontmatter remains valid and preserves `applyTo` routing.
- Intentional tool differences are documented instead of silently copied.
