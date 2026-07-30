testthat::test_that("prepare_raw_temporal_data combines raw sources", {
  data_diversity <-
    tibble::tibble(
      var_name = "n0",
      data_to_fit = list(
        tibble::tibble(
          dataset_id = c("d1", "d2"),
          age = c(1000, 1000),
          value = c(10, 20),
          var_weight = 1
        )
      )
    )
  data_roc <-
    tibble::tibble(
      var_name = "roc",
      data_to_fit = list(
        tibble::tibble(
          dataset_id = "d1",
          age = 1500,
          value = 0.2,
          var_weight = 1
        )
      )
    )
  data_climate <-
    tibble::tibble(
      var_name = "temp_annual",
      data_to_fit = list(
        tibble::tibble(
          dataset_id = "d1",
          age = 2000,
          value = 5
        )
      )
    )
  data_spd <-
    tibble::tibble(
      var_name = "value",
      data_to_fit = list(
        tibble::tibble(
          dataset_id = c("d1", "d1"),
          age = c(1500, 2500),
          value = c(0.2, 0.5)
        )
      )
    )

  result <-
    prepare_raw_temporal_data(
      data_diversity = data_diversity,
      data_roc = data_roc,
      data_climate = data_climate,
      data_spd = data_spd,
      dataset_ids = "d1",
      age_min = 500,
      age_max = 3000
    )

  testthat::expect_identical(nrow(result), 4L)
  testthat::expect_setequal(
    result[["variable"]],
    c("n0", "roc", "temp_annual", "spd")
  )
  testthat::expect_true(all(result[["dataset_id"]] == "d1"))
  testthat::expect_false(any(
    result[["variable"]] == "spd" & result[["age"]] < 2000
  ))
  testthat::expect_false(any(
    result[["variable"]] %in%
      c("density_diversity", "density_turnover")
  ))
})

testthat::test_that("prepare_raw_temporal_data validates sources", {
  testthat::expect_error(
    prepare_raw_temporal_data(
      data_diversity = tibble::tibble(),
      data_roc = tibble::tibble(),
      data_climate = tibble::tibble(),
      data_spd = tibble::tibble()
    ),
    regexp = "must contain"
  )
})
