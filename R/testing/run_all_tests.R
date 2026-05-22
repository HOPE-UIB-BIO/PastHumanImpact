library(here)

here::here("R/functions/testing/run_project_tests.R") |>
  source()

here::here("R/functions/testing/check_function_contract_coverage.R") |>
  source()

check_function_contract_coverage(
  fail_on_missing = FALSE
)

here::here("R/tests/testthat") |>
  run_project_tests()
