testthat::test_that(
  "aggregate_hvar_dataset_ages() averages responses and audits repeats",
  {
    data_input <-
      tibble::tibble(
        dataset_id = "core_1",
        data_merge = list(
          tibble::tibble(
            age = c(500, 500, 1000),
            response = c(1, 3, 5),
            predictor = c(2, 2, 4)
          )
        )
      )
    result <-
      aggregate_hvar_dataset_ages(
        data_source = data_input,
        response_vars = "response",
        predictor_vars = "predictor"
      )

    testthat::expect_equal(result[["data"]][["data_merge"]][[1]]$response,
                           c(2, 5))
    testthat::expect_equal(result[["audit"]]$n_collapsed_rows, 1L)
  }
)

testthat::test_that(
  "aggregate_hvar_dataset_ages() rejects missing ages",
  {
    data_input <-
      tibble::tibble(
        dataset_id = "core_1",
        data_merge = list(
          tibble::tibble(
            age = c(500, NA_real_),
            response = c(1, 2),
            predictor = c(1, 1)
          )
        )
      )

    testthat::expect_error(
      aggregate_hvar_dataset_ages(
        data_source = data_input,
        response_vars = "response",
        predictor_vars = "predictor"
      ),
      "finite numeric age"
    )
  }
)

testthat::test_that(
  "aggregate_hvar_dataset_ages() rejects predictor conflicts",
  {
    data_input <-
      tibble::tibble(
        dataset_id = "core_1",
        data_merge = list(
          tibble::tibble(
            age = c(500, 500),
            response = c(1, 2),
            predictor = c(1, 2)
          )
        )
      )

    testthat::expect_error(
      aggregate_hvar_dataset_ages(
        data_source = data_input,
        response_vars = "response",
        predictor_vars = "predictor"
      ),
      "conflict"
    )
  }
)
