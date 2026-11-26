testthat::test_that(
  "prepare_human_proxy_predictor_columns() pivots kept combinations",
  {
    data_proxy_values <-
      tibble::tibble(
        dataset_id = c("1", "1", "1", "2", "2"),
        age_bp = c(2000, 2500, 2000, 2000, 2000),
        proxy = c("spd", "spd", "kk10", "spd", "kk10"),
        support = c(
          "spd_buffer", "spd_buffer", "cell", "spd_buffer", "cell"
        ),
        value_transformed = c(0.1, 0.2, 0.5, 0.3, 0.7)
      )

    res <-
      prepare_human_proxy_predictor_columns(
        data_proxy_values = data_proxy_values,
        data_column_specs = tibble::tibble(
          proxy = "spd",
          support = "spd_buffer"
        )
      )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_named(res, c("dataset_id", "age", "spd__spd_buffer"))
    testthat::expect_equal(base::nrow(res), 3L)
    testthat::expect_type(dplyr::pull(res, "dataset_id"), "character")

    value_record_one <-
      res |>
      dplyr::filter(
        .data[["dataset_id"]] == "1",
        .data[["age"]] == 2500
      ) |>
      dplyr::pull("spd__spd_buffer")

    testthat::expect_equal(value_record_one, 0.2)
  }
)

testthat::test_that(
  "prepare_human_proxy_predictor_columns() masks unavailable records",
  {
    data_proxy_values <-
      tibble::tibble(
        dataset_id = c("1", "1", "2", "2"),
        age_bp = 2000,
        proxy = c("spd", "kk10", "spd", "kk10"),
        support = "spd_buffer",
        value_transformed = c(0, 0.5, 0.3, 0.7)
      )

    res <-
      prepare_human_proxy_predictor_columns(
        data_proxy_values = data_proxy_values,
        data_column_specs = tibble::tibble(
          proxy = c("spd", "kk10"),
          support = "spd_buffer"
        ),
        data_unavailable = tibble::tibble(dataset_id = "1", proxy = "spd")
      ) |>
      dplyr::arrange(.data[["dataset_id"]])

    testthat::expect_equal(
      dplyr::pull(res, "spd__spd_buffer"),
      c(NA, 0.3)
    )
    testthat::expect_equal(
      dplyr::pull(res, "kk10__spd_buffer"),
      c(0.5, 0.7)
    )
  }
)

testthat::test_that(
  "prepare_human_proxy_predictor_columns() validates its inputs",
  {
    testthat::expect_error(
      prepare_human_proxy_predictor_columns(
        data_proxy_values = tibble::tibble(dataset_id = "1"),
        data_column_specs = tibble::tibble(proxy = "spd", support = "cell")
      ),
      regexp = "predictor-column inputs"
    )

    data_duplicated <-
      tibble::tibble(
        dataset_id = "1",
        age_bp = c(2000, 2000),
        proxy = "spd",
        support = "spd_buffer",
        value_transformed = c(0.1, 0.2)
      )

    testthat::expect_error(
      prepare_human_proxy_predictor_columns(
        data_proxy_values = data_duplicated,
        data_column_specs = tibble::tibble(
          proxy = "spd",
          support = "spd_buffer"
        )
      ),
      regexp = "more than once"
    )
  }
)
