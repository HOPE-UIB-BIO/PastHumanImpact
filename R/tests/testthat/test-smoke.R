testthat::test_that("project config is available to test files", {
  testthat::expect_true(exists("set_seed"))
  testthat::expect_true(exists("add_age_as_factor"))
})

testthat::test_that("set_seed is a single integer-like numeric", {
  testthat::expect_true(is.numeric(set_seed))
  testthat::expect_length(set_seed, 1L)
  testthat::expect_identical(set_seed %% 1, 0)
})

testthat::test_that("core project functions are callable", {
  testthat::expect_true(is.function(add_age_as_factor))
  testthat::expect_true(is.function(add_climatezone_as_factor))
  testthat::expect_true(is.function(add_region_as_factor))
})