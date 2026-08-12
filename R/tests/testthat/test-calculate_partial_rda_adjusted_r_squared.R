testthat::test_that(
  "calculate_partial_rda_adjusted_r_squared() returns one value",
  {
    set.seed(900723)
    mat_explanatory <- matrix(stats::rnorm(60), nrow = 30)
    mat_response <-
      cbind(
        mat_explanatory[, 1] + stats::rnorm(30, sd = 0.2),
        mat_explanatory[, 2] + stats::rnorm(30, sd = 0.2)
      )

    result_value <-
      calculate_partial_rda_adjusted_r_squared(
        response = mat_response,
        explanatory = mat_explanatory
      )

    testthat::expect_type(result_value, "double")
    testthat::expect_length(result_value, 1L)
    testthat::expect_true(is.finite(result_value))
  }
)

testthat::test_that(
  "calculate_partial_rda_adjusted_r_squared() validates rows",
  {
    testthat::expect_error(
      calculate_partial_rda_adjusted_r_squared(
        response = matrix(1:6, nrow = 3),
        explanatory = matrix(1:8, nrow = 4)
      ),
      "contract"
    )
  }
)
