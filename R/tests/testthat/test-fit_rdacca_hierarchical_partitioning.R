testthat::test_that("fit_rdacca_hierarchical_partitioning() validates top-level input types", {
  testthat::expect_error(
    fit_rdacca_hierarchical_partitioning(
      dv = "not_data",
      iv = data.frame(x1 = c(1, 2), x2 = c(3, 4))
    ),
    regexp = "must be a data.frame, matrix, or dist"
  )

  testthat::expect_error(
    fit_rdacca_hierarchical_partitioning(
      dv = data.frame(y1 = c(1, 2), y2 = c(3, 4)),
      iv = "not_predictors"
    ),
    regexp = "must be a data.frame, matrix, or list"
  )
})

testthat::test_that("fit_rdacca_hierarchical_partitioning() rejects NA values", {
  dv <- data.frame(y1 = c(1, NA, 3), y2 = c(2, 3, 4))
  iv <- data.frame(x1 = c(1, 2, 3), x2 = c(3, 2, 1))

  testthat::expect_error(
    fit_rdacca_hierarchical_partitioning(dv = dv, iv = iv),
    regexp = "NA/NaN/Inf"
  )
})

testthat::test_that("fit_rdacca_hierarchical_partitioning() requires enough predictor columns", {
  dv <- data.frame(y1 = c(1, 2, 3), y2 = c(2, 3, 4))
  iv <- data.frame(x1 = c(1, 2, 3))

  testthat::expect_error(
    fit_rdacca_hierarchical_partitioning(dv = dv, iv = iv),
    regexp = "Insufficient number of predictors"
  )
})

testthat::test_that("fit_rdacca_hierarchical_partitioning() validates list predictor groups", {
  dv <- data.frame(y1 = c(1, 2, 3), y2 = c(2, 3, 4))
  iv <- list(
    human = data.frame(x1 = c(1, 2, 3)),
    bad_group = c(1, 2, 3)
  )

  testthat::expect_error(
    fit_rdacca_hierarchical_partitioning(dv = dv, iv = iv),
    regexp = "data.frame is required"
  )
})
