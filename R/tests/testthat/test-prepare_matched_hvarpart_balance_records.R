build_matched_balance_fixture <- function() {
  data_status <-
    tidyr::expand_grid(
      model_id = c("ref", "cmp"),
      dataset_id = c("a", "b", "c", "d", "e")
    ) |>
    dplyr::mutate(
      status = dplyr::if_else(
        .data[["model_id"]] == "ref" & .data[["dataset_id"]] == "c",
        "insufficient_residual_df",
        "estimated"
      ),
      n_samples = dplyr::if_else(
        .data[["model_id"]] == "cmp" & .data[["dataset_id"]] == "d",
        12L,
        13L
      ),
      residual_df = dplyr::case_when(
        .data[["dataset_id"]] == "b" ~ 4L,
        .data[["dataset_id"]] == "c" ~ 3L,
        .default = 6L
      )
    )

  data_records <-
    data_status |>
    dplyr::filter(.data[["status"]] == "estimated") |>
    dplyr::select(dplyr::all_of(c("model_id", "dataset_id"))) |>
    dplyr::mutate(
      signed_difference = 0.1,
      signed_weight = 0.5,
      zero_balance = dplyr::if_else(
        .data[["model_id"]] == "cmp" & .data[["dataset_id"]] == "e",
        NA_real_,
        0.2
      ),
      zero_weight = 0.4
    )

  res <- list(records = data_records, status = data_status)

  return(res)
}

testthat::test_that("prepare_matched_hvarpart_balance_records() matches", {
  list_fixture <-
    build_matched_balance_fixture()

  result <-
    prepare_matched_hvarpart_balance_records(
      data_records = list_fixture[["records"]],
      data_status = list_fixture[["status"]],
      model_ids = c("ref", "cmp"),
      min_residual_df = 4L
    )

  testthat::expect_named(result, c("records", "exclusions", "cohort"))
  testthat::expect_setequal(
    unique(dplyr::pull(result[["records"]], "dataset_id")),
    c("a", "b")
  )
  testthat::expect_equal(nrow(result[["records"]]), 4L)
  testthat::expect_true(all(
    c("n_samples", "residual_df") %in% names(result[["records"]])
  ))

  data_reasons <-
    result[["exclusions"]] |>
    dplyr::arrange(.data[["dataset_id"]], .data[["model_id"]])

  testthat::expect_identical(
    dplyr::pull(data_reasons, "reason"),
    c(
      "below_min_residual_df", "not_estimable", "unequal_n_samples",
      "invalid_balance"
    )
  )
  testthat::expect_identical(
    dplyr::pull(data_reasons, "dataset_id"),
    c("c", "c", "d", "e")
  )
  testthat::expect_equal(dplyr::pull(result[["cohort"]], "n_candidates"), 5L)
  testthat::expect_equal(dplyr::pull(result[["cohort"]], "n_matched"), 2L)
})

testthat::test_that("prepare_matched_hvarpart_balance_records() threshold", {
  list_fixture <-
    build_matched_balance_fixture()

  result <-
    prepare_matched_hvarpart_balance_records(
      data_records = list_fixture[["records"]],
      data_status = list_fixture[["status"]],
      model_ids = c("ref", "cmp"),
      min_residual_df = 5L
    )

  testthat::expect_identical(
    unique(dplyr::pull(result[["records"]], "dataset_id")),
    "a"
  )
  testthat::expect_true(
    "below_min_residual_df" %in% dplyr::pull(result[["exclusions"]], "reason")
  )
})

testthat::test_that("prepare_matched_hvarpart_balance_records() validates", {
  list_fixture <-
    build_matched_balance_fixture()

  testthat::expect_error(
    prepare_matched_hvarpart_balance_records(
      data_records = list_fixture[["records"]],
      data_status = list_fixture[["status"]],
      model_ids = "ref",
      min_residual_df = 4L
    )
  )
  testthat::expect_error(
    prepare_matched_hvarpart_balance_records(
      data_records = list_fixture[["records"]],
      data_status = dplyr::select(list_fixture[["status"]], -"n_samples"),
      model_ids = c("ref", "cmp"),
      min_residual_df = 4L
    )
  )
})
