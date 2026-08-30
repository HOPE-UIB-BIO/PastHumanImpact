testthat::test_that("prepare_spd_radius_predictors() replaces only SPD", {
  predictors <-
    tibble::tibble(
      dataset_id = 1L,
      region = "Europe",
      data_merge = list(
        tibble::tibble(
          age = c(0, 500),
          spd = c(9, 9),
          temp_annual = c(1, 2)
        )
      )
    )
  spd <-
    tibble::tibble(
      dataset_id = c(1L, 1L),
      radius_km = c(250L, 500L),
      available = c(TRUE, TRUE),
      spd = list(
        tibble::tibble(age = c(500, 0), value = c(0.2, 0.1)),
        tibble::tibble(age = c(500, 0), value = c(0.4, 0.3))
      )
    )

  result <-
    prepare_spd_radius_predictors(
      data_predictors = predictors,
      data_spd_by_radius = spd,
      age_min = 0,
      age_max = 500,
      timestep = 500
    )

  testthat::expect_identical(result[["radius_km"]], c(250L, 500L))
  testthat::expect_identical(
    result[["data_merge"]][[1]][["spd"]],
    c(0.1, 0.2)
  )
  testthat::expect_identical(
    result[["data_merge"]][[2]][["temp_annual"]],
    c(1, 2)
  )
})

testthat::test_that("prepare_spd_radius_predictors() requires all radii", {
  predictors <-
    tibble::tibble(
      dataset_id = 1:2,
      data_merge = list(
        tibble::tibble(age = 0, spd = 1),
        tibble::tibble(age = 0, spd = 1)
      )
    )
  spd <-
    tibble::tibble(
      dataset_id = 1L,
      radius_km = 250L,
      available = TRUE,
      spd = list(tibble::tibble(age = 0, value = 1))
    )

  testthat::expect_error(
    prepare_spd_radius_predictors(predictors, spd),
    regexp = "every SPD radius"
  )
})

testthat::test_that("prepare_spd_radius_predictors() retains unavailable rows", {
  predictors <-
    tibble::tibble(
      dataset_id = 1L,
      data_merge = list(
        tibble::tibble(age = c(0, 500), spd = c(9, 9), climate = c(1, 2))
      )
    )
  spd <-
    tibble::tibble(
      dataset_id = c(1L, 1L),
      radius_km = c(250L, 500L),
      available = c(FALSE, TRUE),
      spd = list(
        tibble::tibble(age = c(500, 0), value = c(0, 0)),
        tibble::tibble(age = c(500, 0), value = c(0.2, 0.1))
      )
    )

  result <-
    prepare_spd_radius_predictors(
      predictors,
      spd,
      age_min = 0,
      age_max = 500,
      timestep = 500
    )

  testthat::expect_false(result[["available"]][[1]])
  testthat::expect_identical(
    result[["data_merge"]][[1]][["spd"]],
    c(0, 0)
  )
  testthat::expect_identical(
    result[["data_merge"]][[1]][["climate"]],
    c(1, 2)
  )
})
