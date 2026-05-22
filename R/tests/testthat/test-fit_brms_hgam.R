testthat::test_that("fit_brms_hgam validates integer chains", {
  data_source <-
    data.frame(
      dataset_id = c("d1", "d1", "d2"),
      age = c(0, 1, 0),
      var = c(1.0, 1.5, 2.0),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    fit_brms_hgam(
      data_source = data_source,
      chains = 1.5,
      verbose = FALSE
    ),
    "'chains' must be an integer"
  )
})

testthat::test_that("fit_brms_hgam validates integer sel_k", {
  data_source <-
    data.frame(
      dataset_id = c("d1", "d1", "d2"),
      age = c(0, 1, 0),
      var = c(1.0, 1.5, 2.0),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    fit_brms_hgam(
      data_source = data_source,
      sel_k = 2.2,
      verbose = FALSE
    ),
    "'sel_k' must be an integer"
  )
})

testthat::test_that("fit_brms_hgam returns NA when model fitting fails", {
  data_source <-
    data.frame(
      dataset_id = c("d1", "d1", "d2"),
      age = c(0, 1, 0),
      var = c(1.0, 1.5, 2.0),
      stringsAsFactors = FALSE
    )

  result <-
    fit_brms_hgam(
      data_source = data_source,
      error_family = "not_a_valid_family",
      chains = 1,
      verbose = FALSE
    )

  testthat::expect_identical(result, NA_real_)
})