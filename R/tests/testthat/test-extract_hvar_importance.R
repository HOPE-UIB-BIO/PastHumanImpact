testthat::test_that("extract_hvar_importance() computes ratio-based importance", {
  data_hvar <-
    tibble::tibble(
      dataset_id = c(1, 2),
      varhp = list(
        list(
          summary_table = tibble::tibble(
            predictor = c("human", "climate"),
            Unique = c(-0.1, 0.2),
            Individual = c(0.3, 0.7)
          )
        ),
        list(
          summary_table = tibble::tibble(
            predictor = c("human", "climate"),
            Unique = c(0.4, 0.5),
            Individual = c(0.4, 0.6)
          )
        )
      )
    )

  res <-
    extract_hvar_importance(
      data_hvar = data_hvar,
      model_label = "baseline"
    )

  testthat::expect_true(all(c(
    "dataset_id",
    "predictor",
    "model",
    "importance_ratio",
    "sum_importance",
    "individual",
    "unique"
  ) %in% names(res)))

  testthat::expect_equal(nrow(res), 4)
  testthat::expect_true(all(dplyr::pull(res, model) == "baseline"))

  ratio_dataset_1 <-
    res |>
    dplyr::filter(dataset_id == 1) |>
    dplyr::pull(importance_ratio)

  testthat::expect_equal(sum(ratio_dataset_1), 1, tolerance = 1e-8)

  unique_dataset_1 <-
    res |>
    dplyr::filter(dataset_id == 1, predictor == "human") |>
    dplyr::pull(unique)

  testthat::expect_equal(unique_dataset_1, 0.000001)
})

testthat::test_that("extract_hvar_importance() handles NULL varhp entries", {
  data_hvar <-
    tibble::tibble(
      dataset_id = 1,
      varhp = list(NULL)
    )

  res <-
    extract_hvar_importance(
      data_hvar = data_hvar,
      model_label = "reduced"
    )

  testthat::expect_equal(nrow(res), 2)
  testthat::expect_true(all(is.na(dplyr::pull(res, importance_ratio))))
})

testthat::test_that("extract_hvar_importance() validates input contract", {
  testthat::expect_error(
    extract_hvar_importance(
      data_hvar = "not_df",
      model_label = "baseline"
    ),
    regexp = "must be a data frame"
  )

  testthat::expect_error(
    extract_hvar_importance(
      data_hvar = tibble::tibble(dataset_id = 1),
      model_label = "baseline"
    ),
    regexp = "must contain"
  )

  testthat::expect_error(
    extract_hvar_importance(
      data_hvar = tibble::tibble(dataset_id = 1, varhp = list(NULL)),
      model_label = c("a", "b")
    ),
    regexp = "single character"
  )
})
