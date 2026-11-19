build_paired_balance_fixture <- function() {
  res <-
    tibble::tibble(
      model_id = rep(c("ref", "cmp"), each = 3L),
      dataset_id = rep(c("a", "b", "c"), times = 2L),
      human = c(0.2, 0.1, 0.3, 0.1, 0.1, 0.1),
      climate = c(0.1, 0.2, 0.1, 0.2, 0.2, 0.05),
      time = 0.05,
      total_adjusted_r_squared = 0.4,
      signed_difference = c(0.1, -0.1, 0.2, -0.1, -0.1, 0.05),
      signed_balance = c(0.25, -0.25, 0.5, -0.25, -0.25, 0.125),
      zero_balance = c(1 / 3, -1 / 3, 0.5, -1 / 3, -1 / 3, 1 / 3),
      signed_ranking = c(
        "human", "climate", "human", "climate", "climate", "human"
      ),
      zero_ranking = c(
        "human", "climate", "human", "climate", "climate", "human"
      )
    )

  return(res)
}

testthat::test_that("summarise_paired_hvarpart_balance_change() pairs", {
  result <-
    summarise_paired_hvarpart_balance_change(
      data_records = build_paired_balance_fixture(),
      reference_model_id = "ref",
      comparison_model_id = "cmp"
    )

  data_pairs <-
    result[["pairs"]] |>
    dplyr::arrange(.data[["dataset_id"]])

  testthat::expect_named(
    result,
    c("pairs", "quantity_summary", "ranking_summary")
  )
  testthat::expect_equal(nrow(data_pairs), 3L)
  testthat::expect_equal(
    dplyr::pull(data_pairs, "change_human"),
    c(-0.1, 0, -0.2)
  )
  testthat::expect_identical(
    dplyr::pull(data_pairs, "zero_ranking_changed"),
    c(TRUE, FALSE, FALSE)
  )
  testthat::expect_true(all(
    c("reference_model_id", "comparison_model_id") %in% names(data_pairs)
  ))
})

testthat::test_that("summarise_paired_hvarpart_balance_change() summaries", {
  result <-
    summarise_paired_hvarpart_balance_change(
      data_records = build_paired_balance_fixture(),
      reference_model_id = "ref",
      comparison_model_id = "cmp"
    )

  data_human <-
    result[["quantity_summary"]] |>
    dplyr::filter(.data[["quantity"]] == "human")

  testthat::expect_equal(dplyr::pull(data_human, "n_records"), 3L)
  testthat::expect_equal(dplyr::pull(data_human, "reference_median"), 0.2)
  testthat::expect_equal(dplyr::pull(data_human, "change_median"), -0.1)

  data_zero <-
    result[["ranking_summary"]] |>
    dplyr::filter(.data[["profile"]] == "zero_truncated")

  testthat::expect_equal(dplyr::pull(data_zero, "reference_n_human"), 2L)
  testthat::expect_equal(dplyr::pull(data_zero, "comparison_n_human"), 1L)
  testthat::expect_equal(dplyr::pull(data_zero, "n_ranking_changed"), 1L)
})

testthat::test_that("summarise_paired_hvarpart_balance_change() validates", {
  data_unmatched <-
    build_paired_balance_fixture() |>
    dplyr::filter(
      !(.data[["model_id"]] == "cmp" & .data[["dataset_id"]] == "c")
    )

  testthat::expect_error(
    summarise_paired_hvarpart_balance_change(
      data_records = data_unmatched,
      reference_model_id = "ref",
      comparison_model_id = "cmp"
    ),
    "same records"
  )
  testthat::expect_error(
    summarise_paired_hvarpart_balance_change(
      data_records = build_paired_balance_fixture(),
      reference_model_id = "ref",
      comparison_model_id = "absent"
    )
  )
})
