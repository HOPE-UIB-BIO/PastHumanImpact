testthat::test_that(
  "prepare_shared_climate_hvarpart_designs() shares climate across proxies",
  {
    set.seed(900723)

    data_units <-
      tibble::tibble(
        dataset_id = c("1", "2"),
        data_merge = list(
          tibble::tibble(
            temp = stats::rnorm(12),
            prec = stats::rnorm(12),
            spd = stats::rnorm(12),
            kk10 = stats::rnorm(12)
          ),
          tibble::tibble(
            temp = stats::rnorm(12),
            prec = stats::rnorm(12),
            spd = stats::rnorm(12),
            kk10 = NA_real_
          )
        )
      )

    data_climate <-
      tibble::tibble(
        dataset_id = c("1", "2"),
        climate_status = "eligible_for_design_check",
        climate_predictors = list(c("temp", "prec"), "temp"),
        climate_set = c("prec + temp", "temp"),
        selection_error = NA_character_,
        selection_audit = list(tibble::tibble(), tibble::tibble())
      )

    data_specs <-
      tibble::tibble(
        model_id = c("spd_run", "kk10_run"),
        model_label = c("SPD", "KK10"),
        human_candidates = list("spd", "kk10"),
        human_preference = list("spd", "kk10"),
        exploratory = FALSE
      )

    res <-
      prepare_shared_climate_hvarpart_designs(
        data_units = data_units,
        data_climate_selection = data_climate,
        model_specifications = data_specs,
        unit_columns = "dataset_id"
      )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_equal(base::nrow(res), 4L)
    testthat::expect_true(
      base::all(
        c(
          "dataset_id", "model_id", "model_label", "exploratory",
          "selection_status", "selection_error", "n_rows_without_human",
          "climate_set", "data_merge", "predictor_vars", "selection_audit"
        ) %in% base::names(res)
      )
    )

    list_climate_unit_one <-
      res |>
      dplyr::filter(.data[["dataset_id"]] == "1") |>
      dplyr::pull("predictor_vars") |>
      purrr::map("climate")

    testthat::expect_equal(
      list_climate_unit_one,
      list(c("temp", "prec"), c("temp", "prec"))
    )

    vec_human <-
      res |>
      dplyr::filter(.data[["dataset_id"]] == "1") |>
      dplyr::pull("predictor_vars") |>
      purrr::map_chr("human")

    testthat::expect_equal(vec_human, c("spd", "kk10"))

    vec_status_unit_two <-
      res |>
      dplyr::filter(.data[["dataset_id"]] == "2") |>
      dplyr::pull("selection_status")

    testthat::expect_equal(
      vec_status_unit_two,
      c("eligible_for_design_check", "missing_human_predictor")
    )
    testthat::expect_equal(
      dplyr::pull(res, "n_rows_without_human"),
      c(0L, 0L, 0L, 0L)
    )
  }
)

testthat::test_that(
  "prepare_shared_climate_hvarpart_designs() drops rows without the proxy",
  {
    set.seed(900723)

    data_units <-
      tibble::tibble(
        region = "Europe",
        age = 2000,
        data_merge = list(
          tibble::tibble(
            temp = stats::rnorm(10),
            spd = c(stats::rnorm(8), NA, NA)
          )
        )
      )

    data_climate <-
      tibble::tibble(
        region = "Europe",
        age = 2000,
        climate_status = "missing_climate_predictor",
        climate_predictors = list(character()),
        climate_set = NA_character_,
        selection_error = NA_character_,
        selection_audit = list(tibble::tibble())
      )

    data_specs <-
      tibble::tibble(
        model_id = "spd_run",
        model_label = "SPD",
        human_candidates = list("spd"),
        human_preference = list("spd"),
        exploratory = FALSE
      )

    res <-
      prepare_shared_climate_hvarpart_designs(
        data_units = data_units,
        data_climate_selection = data_climate,
        model_specifications = data_specs,
        unit_columns = c("region", "age"),
        drop_rows_without_human = TRUE
      )

    testthat::expect_equal(dplyr::pull(res, "n_rows_without_human"), 2L)
    testthat::expect_equal(
      base::nrow(dplyr::pull(res, "data_merge")[[1]]),
      8L
    )
    testthat::expect_equal(
      dplyr::pull(res, "selection_status"),
      "missing_climate_predictor"
    )
  }
)

testthat::test_that(
  "prepare_shared_climate_hvarpart_designs() needs one selection per unit",
  {
    data_units <-
      tibble::tibble(
        dataset_id = c("1", "2"),
        data_merge = list(
          tibble::tibble(temp = 1:3, spd = 1:3),
          tibble::tibble(temp = 1:3, spd = 1:3)
        )
      )

    data_climate <-
      tibble::tibble(
        dataset_id = "1",
        climate_status = "eligible_for_design_check",
        climate_predictors = list("temp"),
        climate_set = "temp",
        selection_error = NA_character_,
        selection_audit = list(tibble::tibble())
      )

    data_specs <-
      tibble::tibble(
        model_id = "spd_run",
        model_label = "SPD",
        human_candidates = list("spd"),
        human_preference = list("spd"),
        exploratory = FALSE
      )

    testthat::expect_error(
      prepare_shared_climate_hvarpart_designs(
        data_units = data_units,
        data_climate_selection = data_climate,
        model_specifications = data_specs,
        unit_columns = "dataset_id"
      ),
      regexp = "one climate selection"
    )

    testthat::expect_error(
      prepare_shared_climate_hvarpart_designs(
        data_units = data_units,
        data_climate_selection = data_climate,
        model_specifications = tibble::tibble(model_id = "x"),
        unit_columns = "dataset_id"
      ),
      regexp = "Shared-climate design inputs"
    )
  }
)
