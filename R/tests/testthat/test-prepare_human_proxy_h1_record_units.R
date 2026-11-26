testthat::test_that(
  "prepare_human_proxy_h1_record_units() builds windowed histories",
  {
    data_properties <-
      tibble::tibble(
        dataset_id = c("1", "2"),
        data_merge = list(
          tibble::tibble(
            age = c(500, 2000, 2500, 9000),
            n0 = c(1, 2, 3, 4)
          ),
          tibble::tibble(age = 2000, n0 = 5)
        )
      )

    data_predictors <-
      tibble::tibble(
        dataset_id = c("1", "2"),
        data_merge = list(
          tibble::tibble(
            age = c(500, 2000, 2500, 9000),
            temp_annual = c(10, 11, 12, 13)
          ),
          tibble::tibble(age = 2000, temp_annual = 14)
        )
      )

    data_proxy_columns <-
      tibble::tibble(
        dataset_id = c("1", "1", "2"),
        age = c(2000, 2500, 2000),
        spd__spd_buffer = c(0.2, 0.3, 0.4)
      )

    data_meta <-
      tibble::tibble(
        dataset_id = c("1", "2"),
        region = c("Europe", "Latin America"),
        data_publicity = c("public", "private")
      )

    data_windows <-
      tibble::tibble(
        window_id = c("common_cohort_2000_8000", "native_500_11500"),
        age_min_bp = c(2000, 500),
        age_max_bp = c(8000, 11500)
      )

    res <-
      prepare_human_proxy_h1_record_units(
        data_properties = data_properties,
        data_predictors = data_predictors,
        data_proxy_columns = data_proxy_columns,
        data_meta = data_meta,
        data_windows = data_windows,
        response_vars = "n0",
        predictor_vars = "temp_annual"
      )

    testthat::expect_type(res, "list")
    testthat::expect_named(res, c("data", "audit"))

    data_units <-
      res[["data"]]

    testthat::expect_named(
      data_units,
      c("window_id", "age_min_bp", "age_max_bp", "dataset_id", "data_merge")
    )
    testthat::expect_equal(base::nrow(data_units), 2L)
    testthat::expect_equal(
      base::unique(dplyr::pull(data_units, "dataset_id")),
      "1"
    )

    data_common <-
      data_units |>
      dplyr::filter(.data[["window_id"]] == "common_cohort_2000_8000") |>
      dplyr::pull("data_merge") |>
      purrr::pluck(1)

    testthat::expect_equal(dplyr::pull(data_common, "age"), c(2000, 2500))
    testthat::expect_equal(
      dplyr::pull(data_common, "spd__spd_buffer"),
      c(0.2, 0.3)
    )

    data_native <-
      data_units |>
      dplyr::filter(.data[["window_id"]] == "native_500_11500") |>
      dplyr::pull("data_merge") |>
      purrr::pluck(1)

    testthat::expect_equal(
      dplyr::pull(data_native, "age"),
      c(500, 2000, 2500, 9000)
    )
    testthat::expect_equal(
      dplyr::pull(data_native, "spd__spd_buffer"),
      c(NA, 0.2, 0.3, NA)
    )
    testthat::expect_true(
      base::all(c("n0", "temp_annual") %in% base::names(data_native))
    )
  }
)

testthat::test_that(
  "prepare_human_proxy_h1_record_units() validates its inputs",
  {
    data_nested <-
      tibble::tibble(
        dataset_id = "1",
        data_merge = list(tibble::tibble(age = 2000, n0 = 1))
      )

    testthat::expect_error(
      prepare_human_proxy_h1_record_units(
        data_properties = data_nested,
        data_predictors = data_nested,
        data_proxy_columns = tibble::tibble(dataset_id = "1", age = 2000),
        data_meta = tibble::tibble(
          dataset_id = "1",
          region = "Europe",
          data_publicity = "public"
        ),
        data_windows = tibble::tibble(window_id = "x"),
        response_vars = "n0",
        predictor_vars = "temp_annual"
      ),
      regexp = "record-unit inputs"
    )
  }
)
