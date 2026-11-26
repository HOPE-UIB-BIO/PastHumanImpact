testthat::test_that(
  "compute_adjusted_balance_estimate() matches a weighted stratum model",
  {
    set.seed(900723)

    data_model <-
      tibble::tibble(
        stratum = base::factor(base::rep(c("a", "b"), each = 10)),
        mem = stats::rnorm(20),
        response = stats::rnorm(20),
        weight = stats::runif(20, min = 0.2, max = 1)
      )

    mat_design <-
      stats::model.matrix(~ 0 + stratum + mem, data = data_model)

    mat_design_adjusted <-
      stats::model.matrix(
        ~ 0 + stratum + mem,
        data = dplyr::mutate(data_model, mem = 0)
      )

    res <-
      compute_adjusted_balance_estimate(
        mat_design = mat_design,
        mat_design_adjusted = mat_design_adjusted,
        vec_response = dplyr::pull(data_model, "response"),
        vec_weight = dplyr::pull(data_model, "weight")
      )

    mod_reference <-
      stats::lm(
        response ~ 0 + stratum + mem,
        data = data_model,
        weights = data_model[["weight"]]
      )

    vec_predicted <-
      stats::predict(
        mod_reference,
        newdata = dplyr::mutate(data_model, mem = 0)
      )

    value_expected <-
      stats::weighted.mean(vec_predicted, dplyr::pull(data_model, "weight"))

    testthat::expect_type(res, "double")
    testthat::expect_length(res, 1L)
    testthat::expect_equal(res, value_expected, tolerance = 1e-10)
  }
)

testthat::test_that(
  "compute_adjusted_balance_estimate() treats aliased terms as zero",
  {
    mat_design <-
      cbind(
        a = c(1, 1, 0, 0),
        b = c(0, 0, 1, 1),
        mem = c(0, 0, 0, 0)
      )

    res <-
      compute_adjusted_balance_estimate(
        mat_design = mat_design,
        mat_design_adjusted = mat_design,
        vec_response = c(0.1, 0.3, -0.2, 0),
        vec_weight = c(1, 1, 1, 1)
      )

    testthat::expect_equal(res, 0.05, tolerance = 1e-10)
  }
)

testthat::test_that(
  "compute_adjusted_balance_estimate() validates its inputs",
  {
    testthat::expect_error(
      compute_adjusted_balance_estimate(
        mat_design = base::matrix(1, nrow = 2, ncol = 1),
        mat_design_adjusted = base::matrix(1, nrow = 3, ncol = 1),
        vec_response = c(1, 2),
        vec_weight = c(1, 1)
      ),
      regexp = "Adjusted-balance inputs"
    )
  }
)
