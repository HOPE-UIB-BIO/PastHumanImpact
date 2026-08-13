testthat::test_that(
  "scale_temporal_age() centers and scales age",
  {
    result <-
      scale_temporal_age(
        tibble::tibble(age = c(0, 500, 1000))
      )

    testthat::expect_equal(mean(result$time), 0)
    testthat::expect_equal(stats::sd(result$time), 1)
  }
)
