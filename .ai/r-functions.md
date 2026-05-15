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

PastHumanImpact does not currently have a dedicated project-wide test runner in
the repository tree. When adding tests for new or risky function work, prefer a
standard `testthat` layout:

```text
R/tests/testthat/test-<function_name>.R
```

If a task establishes a different test location, document it in the relevant
change and keep future tests consistent with that location.

Test files should:

- source project configuration before using project functions
- use `testthat::` namespaces for all expectations
- avoid `library()` calls
- cover happy paths, edge cases, and invalid inputs
- use small hand-checkable fixtures
- set `set.seed(1234)` for randomness, matching `R/00_Config_file.R`

Standard setup for focused tests:

```r
library(here)

source(
  here::here("R/00_Config_file.R")
)

testthat::test_file(
  here::here("R/tests/testthat/test-<function_name>.R")
)
```

## Test-Driven Workflow

For new functions or behavior changes:

1. Write or update the roxygen2 contract first.
2. Write focused tests for the intended behavior.
3. Verify the tests fail against the old/stub behavior when practical.
4. Implement the function.
5. Run the focused test or equivalent executable check.
6. Run the smallest affected pipeline manifest or target stage when the function
   feeds a targets pipeline.

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
