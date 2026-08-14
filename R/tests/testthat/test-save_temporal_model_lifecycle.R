testthat::test_that(
  "temporal lifecycle save publishes the reconciled table",
  {
    directory <- tempfile()
    dir.create(directory)

    data_config <-
      tibble::tibble(
        model_id = "model_a",
        need_to_run = FALSE
      )

    path <-
      save_temporal_model_lifecycle(
        data_config = data_config,
        directory = directory,
        current_date = as.Date("2026-08-13")
      )

    testthat::expect_true(file.exists(path))
    testthat::expect_equal(
      as.data.frame(
        readr::read_csv(path, show_col_types = FALSE)
      ),
      as.data.frame(data_config)
    )
  }
)
