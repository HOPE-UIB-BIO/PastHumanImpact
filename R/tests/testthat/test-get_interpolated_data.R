testthat::test_that("get_interpolated_data() reshapes interpolated values by dataset", {
  data_source <-
    data.frame(
      data_to_fit = I(list(data.frame(
        dataset_id = c(1, 1, 1, 1, 1, 1),
        var_name = c("var_a", "var_a", "var_a", "var_b", "var_b", "var_b"),
        age = c(0, 1, 2, 0, 1, 2),
        value = c(0, 1, 2, 10, 11, 12)
      ))),
      stringsAsFactors = FALSE
    )

  result <-
    get_interpolated_data(
      data_source = data_source,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear",
      rule = 2,
      age_min = 0,
      age_max = 2,
      timestep = 1,
      verbose = FALSE
    )

  vec_dataset_id <-
    result[["dataset_id"]]

  data_result <-
    purrr::pluck(result, "data", 1)

  testthat::expect_identical(vec_dataset_id, 1)
  testthat::expect_identical(
    as.data.frame(data_result),
    data.frame(
      age = c(0, 1, 2),
      var_a = c(0, 1, 2),
      var_b = c(10, 11, 12)
    )
  )
})

testthat::test_that("get_interpolated_data() preserves expected columns in nested data", {
  data_source <-
    data.frame(
      data_to_fit = I(list(data.frame(
        dataset_id = c(2, 2, 2, 2),
        var_name = c("x", "x", "y", "y"),
        age = c(0, 2, 0, 2),
        value = c(10, 20, 100, 200)
      ))),
      stringsAsFactors = FALSE
    )

  result <-
    get_interpolated_data(
      data_source = data_source,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear",
      rule = 2,
      age_min = 0,
      age_max = 2,
      timestep = 1,
      verbose = FALSE
    )

  data_nested <- dplyr::pull(result, data)[[1]]
  testthat::expect_true(all(c("age", "x", "y") %in% names(data_nested)))
  testthat::expect_equal(nrow(data_nested), 3L)
})

testthat::test_that("get_interpolated_data() interpolates linearly between points", {
  data_source <-
    data.frame(
      data_to_fit = I(list(data.frame(
        dataset_id = c(1, 1),
        var_name = c("var_a", "var_a"),
        age = c(0, 2),
        value = c(0, 2)
      ))),
      stringsAsFactors = FALSE
    )

  result <-
    get_interpolated_data(
      data_source = data_source,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear",
      rule = 2,
      age_min = 0,
      age_max = 2,
      timestep = 1,
      verbose = FALSE
    )

  data_nested <- dplyr::pull(result, data)[[1]]
  testthat::expect_identical(dplyr::pull(data_nested, var_a), c(0, 1, 2))
})