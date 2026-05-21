library(here)

here::here("R/functions/testing/run_project_tests.R") |>
  source()

here::here("R/tests/testthat") |>
  run_project_tests()
