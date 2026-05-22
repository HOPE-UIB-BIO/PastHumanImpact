testthat::test_that("get_data_combined() joins properties and predictors by age", {
  data_source_properties <-
    data.frame(
      dataset_id = 1,
      data_merge = I(list(data.frame(age = c(1, 2), diversity = c(10, 20))))
    )

  data_source_predictors <-
    data.frame(
      dataset_id = 1,
      data_merge = I(list(data.frame(age = c(1, 2), predictor = c("human", "climate"))))
    )

  res_data <-
    get_data_combined(
      data_source_properties = data_source_properties,
      data_source_predictors = data_source_predictors
    )

  testthat::expect_identical(dplyr::pull(res_data, dataset_id), 1)
  testthat::expect_identical(
    purrr::pluck(res_data, "data_merge", 1),
    data.frame(
      age = c(1, 2),
      diversity = c(10, 20),
      predictor = c("human", "climate"),
      stringsAsFactors = FALSE
    )
  )
})

testthat::test_that("get_data_combined() keeps only intersecting dataset_id", {
  data_source_properties <-
    data.frame(
      dataset_id = c(1, 2),
      data_merge = I(list(
        data.frame(age = c(1, 2), diversity = c(10, 20)),
        data.frame(age = c(1, 2), diversity = c(30, 40))
      ))
    )

  data_source_predictors <-
    data.frame(
      dataset_id = c(2, 3),
      data_merge = I(list(
        data.frame(age = c(1, 2), predictor = c("a", "b")),
        data.frame(age = c(1, 2), predictor = c("c", "d"))
      ))
    )

  res_data <-
    get_data_combined(
      data_source_properties = data_source_properties,
      data_source_predictors = data_source_predictors
    )

  testthat::expect_identical(dplyr::pull(res_data, dataset_id), 2)
})

testthat::test_that("get_data_combined() returns merged age intersection", {
  data_source_properties <-
    data.frame(
      dataset_id = 7,
      data_merge = I(list(data.frame(age = c(1, 2, 3), x = c(10, 20, 30))))
    )

  data_source_predictors <-
    data.frame(
      dataset_id = 7,
      data_merge = I(list(data.frame(age = c(2, 3, 4), y = c(5, 6, 7))))
    )

  res_data <-
    get_data_combined(
      data_source_properties = data_source_properties,
      data_source_predictors = data_source_predictors
    )

  data_merge <-
    dplyr::pull(res_data, data_merge)[[1]]

  testthat::expect_identical(dplyr::pull(data_merge, age), c(2, 3))
})

testthat::test_that("get_data_combined() validates required columns", {
  data_source_properties <-
    data.frame(
      dataset_id = 1,
      wrong_column = I(list(data.frame(age = 1, x = 10)))
    )

  data_source_predictors <-
    data.frame(
      dataset_id = 1,
      data_merge = I(list(data.frame(age = 1, y = 20)))
    )

  testthat::expect_error(
    get_data_combined(
      data_source_properties = data_source_properties,
      data_source_predictors = data_source_predictors
    ),
    regexp = "must contain dataset_id and data_merge"
  )
})