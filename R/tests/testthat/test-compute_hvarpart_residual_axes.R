testthat::test_that(
  "compute_hvarpart_residual_axes() returns no more than three axes",
  {
    set.seed(4)
    response <- matrix(stats::rnorm(60), nrow = 12)
    predictors <- matrix(stats::rnorm(24), nrow = 12)
    result <- compute_hvarpart_residual_axes(response, predictors)

    testthat::expect_lte(ncol(result), 3L)
    testthat::expect_equal(nrow(result), 12L)
  }
)

testthat::test_that(
  "compute_hvarpart_residual_axes() handles a perfect model",
  {
    predictor <- matrix(stats::rnorm(20), ncol = 1)
    result <-
      compute_hvarpart_residual_axes(
        response = cbind(predictor, predictor * 2),
        predictors = predictor
      )

    testthat::expect_s3_class(result, "tbl_df")
    testthat::expect_equal(nrow(result), 20L)
    testthat::expect_equal(ncol(result), 0L)
  }
)
