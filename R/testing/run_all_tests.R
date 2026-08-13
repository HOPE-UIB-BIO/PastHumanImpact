library(here)

here::here(
  "R/functions/workflow/configuration/is_absolute_project_path.R"
) |>
  source()

here::here("R/functions/workflow/testing/run_project_tests.R") |>
  source()

here::here("R/functions/workflow/testing/diagnose_function_contract_coverage.R") |>
  source()

here::here("R/functions/workflow/testing/validate_function_names.R") |>
  source()

validate_function_names()

diagnose_function_contract_coverage(
  fail_on_missing = FALSE
)

here::here("R/tests/testthat") |>
  run_project_tests()
