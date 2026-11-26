testthat::test_that(
  "validate_shared_climate_selection_across_runs() accepts shared sets",
  {
    data_shared <-
      tibble::tibble(
        dataset_id = c("1", "2"),
        climate_set = c("a + b", NA)
      )

    data_used <-
      tidyr::expand_grid(
        profile_id = c("spd_run", "kk10_run"),
        dataset_id = c("1", "2")
      ) |>
      dplyr::mutate(
        climate_set = dplyr::if_else(
          .data[["dataset_id"]] == "1",
          "a + b",
          NA_character_
        )
      )

    res <-
      validate_shared_climate_selection_across_runs(
        data_used = data_used,
        data_shared = data_shared,
        unit_columns = "dataset_id"
      )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_named(
      res,
      c("dataset_id", "n_runs", "climate_set", "identical_climate_set")
    )
    testthat::expect_equal(dplyr::pull(res, "n_runs"), c(2L, 2L))
    testthat::expect_true(
      base::all(dplyr::pull(res, "identical_climate_set"))
    )
  }
)

testthat::test_that(
  "validate_shared_climate_selection_across_runs() stops on differences",
  {
    data_shared <-
      tibble::tibble(dataset_id = "1", climate_set = "a + b")

    data_used <-
      tibble::tibble(
        profile_id = c("spd_run", "kk10_run"),
        dataset_id = "1",
        climate_set = c("a + b", "a")
      )

    testthat::expect_error(
      validate_shared_climate_selection_across_runs(
        data_used = data_used,
        data_shared = data_shared,
        unit_columns = "dataset_id"
      ),
      regexp = "differ from the shared selection"
    )

    testthat::expect_error(
      validate_shared_climate_selection_across_runs(
        data_used = dplyr::mutate(data_used, dataset_id = "9"),
        data_shared = data_shared,
        unit_columns = "dataset_id"
      ),
      regexp = "differ from the shared selection"
    )
  }
)

testthat::test_that(
  "validate_shared_climate_selection_across_runs() validates inputs",
  {
    testthat::expect_error(
      validate_shared_climate_selection_across_runs(
        data_used = tibble::tibble(dataset_id = "1"),
        data_shared = tibble::tibble(dataset_id = "1"),
        unit_columns = "dataset_id"
      ),
      regexp = "Shared-climate check inputs"
    )
  }
)
