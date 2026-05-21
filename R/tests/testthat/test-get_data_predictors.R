testthat::test_that("get_data_predictors() joins SPD and climate data", {
  data_source_spd_events <-
    data.frame(
      dataset_id = 1,
      data_merge = I(list(data.frame(age = c(1, 2), spd = c(10, 20))))
    )

  data_source_climate <-
    data.frame(
      dataset_id = 1,
      data = I(list(data.frame(age = c(1, 2), temp = c(5, 6))))
    )

  res_data <-
    get_data_predictors(
      data_source_spd_events = data_source_spd_events,
      data_source_climate = data_source_climate
    )

  testthat::expect_identical(dplyr::pull(res_data, dataset_id), 1)
  testthat::expect_identical(
    purrr::pluck(res_data, "data_merge", 1),
    data.frame(
      age = c(1, 2),
      spd = c(10, 20),
      temp = c(5, 6)
    )
  )
})

testthat::test_that("get_data_predictors() keeps only intersecting dataset_id", {
  data_source_spd_events <-
    data.frame(
      dataset_id = c(1, 2),
      data_merge = I(list(
        data.frame(age = c(1, 2), spd = c(10, 20)),
        data.frame(age = c(1, 2), spd = c(30, 40))
      ))
    )

  data_source_climate <-
    data.frame(
      dataset_id = c(2, 3),
      data = I(list(
        data.frame(age = c(1, 2), temp = c(7, 8)),
        data.frame(age = c(1, 2), temp = c(9, 10))
      ))
    )

  res_data <-
    get_data_predictors(
      data_source_spd_events = data_source_spd_events,
      data_source_climate = data_source_climate
    )

  testthat::expect_identical(dplyr::pull(res_data, dataset_id), 2)
})

testthat::test_that("get_data_predictors() removes rows with NA after merge", {
  data_source_spd_events <-
    data.frame(
      dataset_id = 9,
      data_merge = I(list(data.frame(age = c(1, 2), spd = c(10, NA))))
    )

  data_source_climate <-
    data.frame(
      dataset_id = 9,
      data = I(list(data.frame(age = c(1, 2), temp = c(5, 6))))
    )

  res_data <-
    get_data_predictors(
      data_source_spd_events = data_source_spd_events,
      data_source_climate = data_source_climate
    )

  data_merge <- purrr::pluck(res_data, "data_merge", 1)
  testthat::expect_equal(nrow(data_merge), 1L)
  testthat::expect_identical(dplyr::pull(data_merge, age), 1)
})