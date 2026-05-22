testthat::test_that("get_roc_for_modelling() computes weights and nests roc data", {
  data_source_roc <-
    data.frame(
      PAP_roc = I(list(data.frame(
        dataset_id = c(1, 1),
        Age = c(100, 200),
        ROC = c(0.2, 0.4),
        ROC_up = c(0.3, 0.5),
        ROC_dw = c(0.1, 0.35)
      ))),
      stringsAsFactors = FALSE
    )

  result <-
    get_roc_for_modelling(data_source_roc)

  vec_var_name <-
    result[["var_name"]]

  data_roc <-
    purrr::pluck(result, "data_to_fit", 1)

  testthat::expect_identical(vec_var_name, "roc")
  testthat::expect_equal(
    as.data.frame(data_roc),
    data.frame(
      dataset_id = c(1, 1),
      age = c(100, 200),
      value = c(0.2, 0.4),
      var_weight = c(0.875, 1.1666666666666667)
    )
  )
})

testthat::test_that("get_roc_for_modelling() returns expected columns", {
  data_source_roc <-
    data.frame(
      PAP_roc = I(list(data.frame(
        dataset_id = c(2, 2),
        Age = c(300, 400),
        ROC = c(0.5, 0.7),
        ROC_up = c(0.6, 0.8),
        ROC_dw = c(0.4, 0.6)
      ))),
      stringsAsFactors = FALSE
    )

  result <- get_roc_for_modelling(data_source_roc)

  testthat::expect_identical(dplyr::pull(result, var_name), "roc")
  testthat::expect_true(
    all(c("dataset_id", "age", "value", "var_weight") %in% names(dplyr::pull(result, data_to_fit)[[1]]))
  )
})

testthat::test_that("get_roc_for_modelling() drops NA ROC rows", {
  data_source_roc <-
    data.frame(
      PAP_roc = I(list(data.frame(
        dataset_id = c(1, 1, 1),
        Age = c(100, 200, 300),
        ROC = c(0.2, NA, 0.6),
        ROC_up = c(0.3, 0.5, 0.8),
        ROC_dw = c(0.1, 0.4, 0.5)
      ))),
      stringsAsFactors = FALSE
    )

  result <- get_roc_for_modelling(data_source_roc)
  data_roc <- dplyr::pull(result, data_to_fit)[[1]]

  testthat::expect_equal(nrow(data_roc), 2L)
  testthat::expect_identical(dplyr::pull(data_roc, age), c(100, 300))
})

testthat::test_that("get_roc_for_modelling() validates PAP_roc column", {
  bad_source <-
    data.frame(
      dataset_id = 1,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_roc_for_modelling(bad_source),
    regexp = "PAP_roc"
  )
})