testthat::test_that("pairing calculates 500 minus 250 and reversals", {
  data_source <-
    tibble::tibble(
      dataset_id = rep(c("a", "b"), each = 2),
      radius_km = rep(c(250L, 500L), 2),
      status = c("estimated", "estimated", "failed", "estimated"),
      balance = c(-0.2, 0.1, NA, 0.3),
      ranking = c("climate", "human", NA, "human")
    )

  result <-
    prepare_spd_radius_paired_comparison(
      data_source = data_source,
      key_cols = "dataset_id",
      balance_cols = "balance",
      ranking_cols = "ranking",
      estimable_statuses = "estimated"
    )

  testthat::expect_equal(
    result[["balance_delta_500_minus_250"]][1],
    0.3
  )
  testthat::expect_true(result[["ranking_reversal"]][1])
  testthat::expect_identical(
    result[["robustness_classification"]],
    c("ranking_reversal", "gained_estimability")
  )
})

testthat::test_that("pairing reports every requested ranking reversal", {
  data_source <-
    tibble::tibble(
      dataset_id = c("a", "a"),
      radius_km = c(250L, 500L),
      status = "estimated",
      balance = c(0.1, 0.2),
      signed_ranking = c("human", "human"),
      zero_ranking = c("climate", "human")
    )

  result <-
    prepare_spd_radius_paired_comparison(
      data_source = data_source,
      key_cols = "dataset_id",
      balance_cols = "balance",
      ranking_cols = c("signed_ranking", "zero_ranking"),
      estimable_statuses = "estimated"
    )

  testthat::expect_false(result[["signed_ranking_reversal"]])
  testthat::expect_true(result[["zero_ranking_reversal"]])
  testthat::expect_false(result[["ranking_reversal"]])
})

testthat::test_that("pairing retains missing profile values", {
  data_source <-
    tibble::tibble(
      dataset_id = c("a", "b", "b"),
      radius_km = c(250L, 250L, 500L),
      status = c("failed", "estimated", "estimated"),
      balance = c(NA, 0.1, 0.2),
      ranking = c(NA, "human", "human")
    )

  result <-
    prepare_spd_radius_paired_comparison(
      data_source = data_source,
      key_cols = "dataset_id",
      balance_cols = "balance",
      ranking_cols = "ranking",
      estimable_statuses = "estimated"
    )

  testthat::expect_true(is.na(result[["status_500_km"]][1]))
  testthat::expect_false(result[["matched_estimable"]][1])
  testthat::expect_identical(
    result[["robustness_classification"]][1],
    "missing_500_km_result"
  )
})

testthat::test_that("pairing rejects duplicate analytical units", {
  data_source <-
    tibble::tibble(
      dataset_id = c("a", "a", "a"),
      radius_km = c(250L, 250L, 500L),
      status = "estimated",
      balance = 0,
      ranking = "tie"
    )

  testthat::expect_error(
    prepare_spd_radius_paired_comparison(
      data_source = data_source,
      key_cols = "dataset_id",
      balance_cols = "balance",
      ranking_cols = "ranking",
      estimable_statuses = "estimated"
    ),
    regexp = "unique analytical-unit"
  )
})
