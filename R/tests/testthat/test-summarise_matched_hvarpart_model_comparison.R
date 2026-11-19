build_matched_comparison_fixture <- function() {
  data_status <-
    tidyr::expand_grid(
      model_id = c("ref", "cmp", "other"),
      dataset_id = c("a", "b", "c")
    ) |>
    dplyr::mutate(
      status = dplyr::if_else(
        .data[["model_id"]] == "other" & .data[["dataset_id"]] == "c",
        "insufficient_unique_ages",
        "estimated"
      ),
      n_samples = 13L,
      residual_df = 6L
    )

  data_records <-
    data_status |>
    dplyr::filter(.data[["status"]] == "estimated") |>
    dplyr::select(dplyr::all_of(c("model_id", "dataset_id"))) |>
    dplyr::mutate(
      human = dplyr::if_else(.data[["model_id"]] == "ref", 0.2, 0.1),
      climate = 0.15,
      time = 0.05,
      total_adjusted_r_squared = 0.4,
      signed_difference = .data[["human"]] - .data[["climate"]],
      signed_weight = 0.4,
      signed_balance = .data[["signed_difference"]] / 0.4,
      zero_balance = .data[["signed_difference"]] / 0.35,
      zero_weight = 0.35,
      signed_ranking = dplyr::if_else(
        .data[["signed_difference"]] > 0, "human", "climate"
      ),
      zero_ranking = .data[["signed_ranking"]]
    )

  data_commonality <-
    data_records |>
    dplyr::select(dplyr::all_of(c("model_id", "dataset_id"))) |>
    tidyr::expand_grid(groups = c("human", "human&time")) |>
    dplyr::mutate(fraction = 0.05, total_fraction = 0.1)

  res <-
    list(
      records = data_records,
      status = data_status,
      commonality = data_commonality
    )

  return(res)
}

testthat::test_that("summarise_matched_hvarpart_model_comparison() runs", {
  list_fixture <-
    build_matched_comparison_fixture()

  result <-
    summarise_matched_hvarpart_model_comparison(
      data_records = list_fixture[["records"]],
      data_status = list_fixture[["status"]],
      data_commonality = list_fixture[["commonality"]],
      cohort_model_ids = c("ref", "cmp", "other"),
      reference_model_id = "ref",
      comparison_model_id = "cmp",
      min_residual_df = 5L,
      comparison_labels = tibble::tibble(test = "demo", cohort = "common")
    )

  testthat::expect_named(
    result,
    c(
      "cohort", "exclusions", "records", "pairs", "quantity_summary",
      "ranking_summary", "commonality_values", "commonality_summary"
    )
  )
  testthat::expect_equal(nrow(result[["pairs"]]), 2L)
  testthat::expect_setequal(
    dplyr::pull(result[["records"]], "model_id"),
    c("ref", "cmp", "other")
  )
  testthat::expect_setequal(
    dplyr::pull(result[["commonality_values"]], "model_id"),
    c("ref", "cmp")
  )
  testthat::expect_equal(nrow(result[["commonality_values"]]), 4L)
  testthat::expect_identical(
    names(result[["quantity_summary"]])[1:3],
    c("test", "cohort", "min_residual_df")
  )
  testthat::expect_false("test" %in% names(result[["records"]]))
  testthat::expect_equal(
    dplyr::pull(result[["cohort"]], "n_matched"),
    2L
  )
})

testthat::test_that("summarise_matched_hvarpart_model_comparison() checks", {
  list_fixture <-
    build_matched_comparison_fixture()

  testthat::expect_error(
    summarise_matched_hvarpart_model_comparison(
      data_records = list_fixture[["records"]],
      data_status = list_fixture[["status"]],
      data_commonality = list_fixture[["commonality"]],
      cohort_model_ids = c("cmp", "other"),
      reference_model_id = "ref",
      comparison_model_id = "cmp",
      min_residual_df = 5L
    )
  )
  testthat::expect_error(
    summarise_matched_hvarpart_model_comparison(
      data_records = list_fixture[["records"]],
      data_status = list_fixture[["status"]],
      data_commonality = list_fixture[["commonality"]],
      cohort_model_ids = c("ref", "cmp"),
      reference_model_id = "ref",
      comparison_model_id = "cmp",
      min_residual_df = 5L,
      comparison_labels = tibble::tibble(test = c("a", "b"))
    )
  )
})
