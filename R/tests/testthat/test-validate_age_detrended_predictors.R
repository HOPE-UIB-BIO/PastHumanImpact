testthat::test_that("validate_age_detrended_predictors() passes residuals", {
  data_unit <-
    tibble::tibble(age = c(1, 2, 3, 4, 5), proxy = c(2, 4, 3, 7, 6))

  data_unit[["proxy_age_detrended"]] <-
    unname(stats::residuals(stats::lm(proxy ~ age, data = data_unit)))

  data_nested <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      data_merge = list(
        data_unit,
        tibble::tibble(age = 1:3, proxy_age_detrended = c(0, 0, 0))
      )
    )

  result <-
    validate_age_detrended_predictors(
      data_source = data_nested,
      detrended_columns = "proxy_age_detrended"
    )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(
    result,
    c(
      "dataset_id", "predictor", "n_finite", "constant",
      "correlation_with_age", "mean_value", "passed"
    )
  )
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true(all(dplyr::pull(result, "passed")))
  testthat::expect_identical(dplyr::pull(result, "constant"), c(FALSE, TRUE))
  testthat::expect_true(is.na(dplyr::pull(result, "correlation_with_age")[[2]]))
})

testthat::test_that("validate_age_detrended_predictors() aborts on trend", {
  data_nested <-
    tibble::tibble(
      dataset_id = "a",
      data_merge = list(tibble::tibble(
        age = c(1, 2, 3, 4),
        proxy_age_detrended = c(-1.5, -0.5, 0.5, 1.5)
      ))
    )

  testthat::expect_error(
    validate_age_detrended_predictors(
      data_source = data_nested,
      detrended_columns = "proxy_age_detrended"
    ),
    "age"
  )
})

testthat::test_that("validate_age_detrended_predictors() validates input", {
  data_nested <-
    tibble::tibble(
      dataset_id = "a",
      data_merge = list(tibble::tibble(age = 1:3, x = c(0, 0, 0)))
    )

  testthat::expect_error(
    validate_age_detrended_predictors(
      data_source = data_nested,
      detrended_columns = "absent"
    )
  )
  testthat::expect_error(
    validate_age_detrended_predictors(
      data_source = data_nested,
      detrended_columns = "x",
      unit_column = "absent"
    )
  )
})
