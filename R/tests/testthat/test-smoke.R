testthat::test_that("project config is available to test files", {
  testthat::expect_true(exists("set_seed"))
  testthat::expect_true(exists("prepare_age_factor"))
})

testthat::test_that("set_seed is a single integer-like numeric", {
  testthat::expect_true(is.numeric(set_seed))
  testthat::expect_length(set_seed, 1L)
  testthat::expect_identical(set_seed %% 1, 0)
})

testthat::test_that("core project functions are callable", {
  testthat::expect_true(is.function(prepare_age_factor))
  testthat::expect_true(is.function(prepare_climatezone_factor))
  testthat::expect_true(is.function(prepare_region_factor))
})