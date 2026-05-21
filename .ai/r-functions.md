# R Function, Documentation, and Test Guidance

Canonical guidance for authoring R functions, roxygen2 documentation, and
function-level tests in this repository.

## Function Locations

Project functions live under `R/functions/`, grouped by analysis domain:

- `R/functions/climate/`
- `R/functions/data_wrangling/`
- `R/functions/events/`
- `R/functions/hvarpart/`
- `R/functions/modelling/`
- `R/functions/PAPs/`
- `R/functions/procrustes/`
- `R/functions/spd/`
- `R/functions/visualisation/`

Each function file should contain one primary function and should be named after
that function. Keep helper functions private to the file only when they are
small and tightly coupled; otherwise promote them to their own file.

## Function Style

- Follow `.ai/r-coding.md` for naming, namespaces, line width, and formatting.
- Function names are verbs in `snake_case`.
- Always use explicit argument names in function calls where practical.
- Always end project functions with an explicit `return(res_object)`.
- Do not call `library()` or `require()` inside functions.
- Avoid global side effects. A function should not source files, mutate global
  variables, or write files unless writing is its documented purpose.

## Argument and Runtime Checks

Use different tools for different failures:

- `assertthat::assert_that()` for caller-supplied argument contracts: type,
  length, required columns, allowed values, and file existence.
- `cli::cli_abort()` for internal/data-content failures discovered after
  processing begins.
- `cli::cli_warn()` and `cli::cli_inform()` for warnings and progress messages.

Any function that prints progress or informational messages must accept a
`verbose = TRUE` argument and guard messages with `if (isTRUE(verbose))`.
Errors should not be silenced by `verbose`.

## Roxygen2

All new or substantially edited functions should have roxygen2 documentation
immediately above the function definition.

Use this structure:

```r
#' @title Short title
#' @description
#' One or two sentences describing what the function computes.
#' @param data_input
#' Description of the input data.
#' @param verbose
#' Logical. If `TRUE` (default), progress messages are printed.
#' @return
#' Description of the returned object, including type and key columns.
#' @details
#' Important assumptions, units, or edge-case behavior.
#' @examples
#' \dontrun{
#' result <- function_name(data_input = data_example)
#' }
function_name <- function(data_input, verbose = TRUE) {
  ...
}
```

Keep roxygen lines within the R code line limit. Documentation should state the
intended behavior; tests should follow that intent rather than current bugs.

## Tests

Function-level tests live in the project `testthat` layout:

```text
R/tests/testthat/test-<function_name>.R
```

Every function in `R/functions/` should have a matching test file unless a
documented exception is recorded in the active task notes or progress tracker.

Project test execution is routed through helpers in `R/testing/run_tests.R`:

- `run_project_tests()` for the full suite.
- `run_test_file()` for one focused test file.

## Test File Conventions

Treat the rules below as a hard checklist for every new test file.

### File and Structure

- Use one file per function: `test-<function_name>.R`.
- Do not add project banner headers to test files.
- Do not redefine or source the function under test inside the test file.
- Use multiple `testthat::test_that()` blocks grouped by behavior.
- Keep test names short and explicit, for example
  `"function_name() validates required columns"`.

### Style Rules Frequently Missed in Tests

- Follow `.ai/r-coding.md` line width for all R code, including
  `testthat::test_that()` description strings.
- Do not use `$` for data-frame column access in test assertions. Prefer
  `dplyr::pull(data_frame, column_name)`.
- When the right-hand side is a function call, put it on the next line after
  `<-`.
- Use explicit namespaces in test files. At minimum, all `testthat` calls must
  be written as `testthat::...`.
- Do not call `library()` inside test files.

### Setup and Fixtures

- Use small, hand-checkable fixtures by default.
- Use deterministic seeds for randomness: `set.seed(900723)`.
- Prefer temporary directories and files for side effects (`tempdir()`,
  `tempfile()`).
- Avoid fixtures that require unavailable external data unless the test is
  explicitly marked as constrained.

### Required Coverage Per Function

Each function test file should include:

1. Happy-path behavior with valid input.
2. Input validation failures (wrong type, missing columns, invalid values).
3. Output contract checks (class/type, names, dimensions, core fields).
4. At least one edge case (empty input, single-row input, boundary values, or
   `NA` handling) where relevant.
5. Warnings/messages/side effects where the function contract implies them.

### Intended Behavior Over Accidental Behavior

When implementation and intent diverge, write tests against intended behavior.
Infer intent in this order:

1. Function name.
2. Roxygen contract and inline comments.
3. Argument names/defaults.
4. Implementation details (only for technical shape constraints).

If a task establishes a different test location, document it in the relevant
change and keep future tests consistent with that location.

Focused run pattern:

```r
source("R/testing/run_tests.R")

run_test_file(
  test_file = file.path("R", "tests", "testthat", "test-<function_name>.R")
)
```

Full suite pattern:

```r
source("R/testing/run_tests.R")

run_project_tests(
  test_dir = file.path("R", "tests", "testthat")
)
```

For new function work, treat the relevant test run as a required final
validation step before the change is considered complete.

## Test-Driven Workflow

For new functions or behavior changes:

1. Write or update the roxygen2 contract first.
2. Write focused tests for the intended behavior.
3. Verify the tests fail against the old/stub behavior when practical.
4. Implement the function.
5. Run the focused test or equivalent executable check.
6. Run the full test suite.
7. Run the smallest affected pipeline manifest or target stage when the
  function feeds a targets pipeline.

### Completion Gate

Do not treat a function task as complete until:

- The focused test file passes.
- The full suite passes via `run_project_tests()`.
- Any affected pipeline contract checks pass (for pipeline-coupled functions).

For changes that touch shared helpers, pipeline contracts, or data structures,
broaden validation according to `.ai/debugging.md`.

## Anonymous Functions

Inside `purrr` calls, use the tilde formula style with `.x`, `.y`, or
`..1`/`..2` for `pmap()`. Do not use backslash lambdas inside `purrr` calls.

```r
vec_values |>
  purrr::map(
    .f = ~ transform_value(.x)
  )
```

When `purrr::map()` calls are nested, bind the outer `.x` to a named object
before entering the inner map so the code is unambiguous.
