testthat::test_that(
  "summarise_climate_retention_frequency() counts retained predictors",
  {
    data_selection <-
      tibble::tibble(
        window_id = "w",
        dataset_id = c("1", "2", "3"),
        region = c("A", "A", "B"),
        climate_predictors = list(c("a", "b"), "a", "b")
      )

    res <-
      summarise_climate_retention_frequency(
        data_climate_selection = data_selection,
        climate_candidates = c("a", "b"),
        group_columns = "window_id"
      )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_named(
      res,
      c(
        "window_id", "region", "predictor", "n_units", "n_retained",
        "share_retained"
      )
    )

    data_expected <-
      tibble::tibble(
        region = c("A", "A", "All", "All", "B", "B"),
        predictor = c("a", "b", "a", "b", "a", "b"),
        n_units = c(2L, 2L, 3L, 3L, 1L, 1L),
        n_retained = c(2L, 1L, 2L, 2L, 0L, 1L)
      )

    data_actual <-
      res |>
      dplyr::arrange(.data[["region"]], .data[["predictor"]]) |>
      dplyr::select(
        dplyr::all_of(c("region", "predictor", "n_units", "n_retained"))
      )

    testthat::expect_equal(data_actual, data_expected)
    testthat::expect_equal(
      res |>
        dplyr::filter(
          .data[["region"]] == "All",
          .data[["predictor"]] == "a"
        ) |>
        dplyr::pull("share_retained"),
      2 / 3
    )
  }
)

testthat::test_that(
  "summarise_climate_retention_frequency() validates its inputs",
  {
    testthat::expect_error(
      summarise_climate_retention_frequency(
        data_climate_selection = tibble::tibble(region = "A"),
        climate_candidates = "a"
      ),
      regexp = "Climate-retention inputs"
    )
  }
)
