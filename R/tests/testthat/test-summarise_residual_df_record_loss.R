testthat::test_that("summarise_residual_df_record_loss() counts losses", {
  data_status <-
    tibble::tibble(
      model_id = c(rep("full", 6L), rep("filtered", 2L)),
      dataset_id = c("a", "b", "c", "d", "e", "f", "a", "b"),
      region = c("Europe", "Europe", "Asia", "Asia", "Asia", "Asia",
        "Europe", "Europe"),
      status = c(
        "estimated", "estimated_residual_temporal_dependence",
        "insufficient_residual_df", "insufficient_unique_ages",
        "missing_human_predictor", "rank_deficient",
        "estimated", "estimated"
      ),
      residual_df = c(4L, 6L, 3L, NA, NA, 2L, 6L, 7L)
    )

  result <-
    summarise_residual_df_record_loss(data_status = data_status)

  data_full_4 <-
    result |>
    dplyr::filter(
      .data[["model_id"]] == "full",
      .data[["min_residual_df"]] == 4L
    )

  data_full_5 <-
    result |>
    dplyr::filter(
      .data[["model_id"]] == "full",
      .data[["min_residual_df"]] == 5L
    )

  testthat::expect_equal(nrow(result), 4L)
  testthat::expect_equal(dplyr::pull(data_full_4, "n_records"), 6L)
  testthat::expect_equal(dplyr::pull(data_full_4, "n_estimable"), 2L)
  testthat::expect_equal(
    dplyr::pull(data_full_4, "n_lost_insufficient_residual_df"),
    1L
  )
  testthat::expect_equal(
    dplyr::pull(data_full_4, "n_lost_insufficient_unique_ages"),
    1L
  )
  testthat::expect_equal(
    dplyr::pull(data_full_4, "n_lost_missing_predictor"),
    1L
  )
  testthat::expect_equal(dplyr::pull(data_full_4, "n_lost_other"), 1L)
  testthat::expect_equal(dplyr::pull(data_full_5, "n_estimable"), 1L)
  testthat::expect_equal(
    dplyr::pull(data_full_5, "n_lost_insufficient_residual_df"),
    2L
  )
  testthat::expect_equal(
    dplyr::pull(data_full_5, "n_lost_relative_to_lowest_threshold"),
    1L
  )
})

testthat::test_that("summarise_residual_df_record_loss() groups", {
  data_status <-
    tibble::tibble(
      model_id = "full",
      dataset_id = c("a", "b", "c"),
      region = c("Europe", "Asia", "Asia"),
      status = "estimated",
      residual_df = c(4L, 5L, 4L)
    )

  result <-
    summarise_residual_df_record_loss(
      data_status = data_status,
      thresholds = 5L,
      group_columns = "region"
    )

  testthat::expect_named(
    result,
    c(
      "model_id", "min_residual_df", "region", "n_records", "n_estimable",
      "n_lost", "n_lost_insufficient_residual_df",
      "n_lost_insufficient_unique_ages", "n_lost_missing_predictor",
      "n_lost_other", "n_lost_relative_to_lowest_threshold"
    )
  )
  testthat::expect_equal(
    result |>
      dplyr::arrange(.data[["region"]]) |>
      dplyr::pull("n_estimable"),
    c(1L, 0L)
  )
})

testthat::test_that("summarise_residual_df_record_loss() validates", {
  data_status <-
    tibble::tibble(
      model_id = "full", status = "estimated", residual_df = 4L
    )

  testthat::expect_error(
    summarise_residual_df_record_loss(
      data_status = data_status,
      thresholds = 3L
    )
  )
  testthat::expect_error(
    summarise_residual_df_record_loss(
      data_status = dplyr::select(data_status, -"residual_df")
    )
  )

  result <-
    summarise_residual_df_record_loss(data_status = data_status[0, ])

  testthat::expect_equal(nrow(result), 0L)
})
