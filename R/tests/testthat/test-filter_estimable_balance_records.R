testthat::test_that("filter_estimable_balance_records() keeps estimable rows", {
  data_records <-
    tibble::tibble(
      dataset_id = c("1", "2", "3", "4", "5"),
      status = c(
        "estimated", "estimated_residual_temporal_dependence",
        "model_error", "estimated", "estimated"
      ),
      signed_difference = c(0.1, -0.2, 0.3, NA, 0.1),
      signed_weight = c(0.5, 0.4, 0.3, 0.2, 0.1),
      zero_balance = c(0.2, -1, 0.5, 0.1, NA),
      zero_weight = c(0.3, 0.2, 0.1, 0.4, 0)
    )

  res <-
    filter_estimable_balance_records(data_records = data_records)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, base::names(data_records))
  testthat::expect_equal(dplyr::pull(res, "dataset_id"), c("1", "2"))
})

testthat::test_that("filter_estimable_balance_records() validates columns", {
  testthat::expect_error(
    filter_estimable_balance_records(
      data_records = tibble::tibble(status = "estimated")
    ),
    regexp = "Balance records"
  )
})

testthat::test_that("filter_estimable_balance_records() handles no rows", {
  data_records <-
    tibble::tibble(
      status = character(),
      signed_difference = numeric(),
      signed_weight = numeric(),
      zero_balance = numeric(),
      zero_weight = numeric()
    )

  res <-
    filter_estimable_balance_records(data_records = data_records)

  testthat::expect_equal(base::nrow(res), 0L)
})
