testthat::test_that("validate_spd_radius_products() accepts matched products", {
  data_spd <-
    tibble::tibble(
      dataset_id = c(1L, 1L),
      radius_km = c(250L, 500L),
      spd = list(
        tibble::tibble(age = 0:1, value = c(0, 1)),
        tibble::tibble(age = 0:1, value = c(0, 2))
      ),
      n_time_points = c(2L, 2L),
      n_finite_values = c(2L, 2L),
      n_nonzero_values = c(1L, 1L),
      available = c(TRUE, TRUE),
      availability_status = c("available", "available")
    )

  testthat::expect_silent(
    validate_spd_radius_products(data_spd)
  )
})

testthat::test_that(
  "validate_spd_radius_products() rejects impossible availability",
  {
    data_spd <-
      tibble::tibble(
        dataset_id = c(1L, 1L),
        radius_km = c(250L, 500L),
        spd = list(
          tibble::tibble(age = 0:1, value = c(0, 1)),
          tibble::tibble(age = 0:1, value = c(0, 0))
        ),
        n_time_points = c(2L, 2L),
        n_finite_values = c(2L, 2L),
        n_nonzero_values = c(1L, 0L),
        available = c(TRUE, FALSE),
        availability_status = c("available", "unavailable")
      )

    testthat::expect_error(
      validate_spd_radius_products(data_spd),
      regexp = "cannot be unavailable"
    )
  }
)

testthat::test_that(
  "validate_spd_radius_products() rejects different dataset age grids",
  {
    data_spd <-
      tibble::tibble(
        dataset_id = rep(c(1L, 2L), each = 2L),
        radius_km = rep(c(250L, 500L), 2L),
        spd = list(
          tibble::tibble(age = 0:1, value = c(0, 1)),
          tibble::tibble(age = 0:1, value = c(0, 2)),
          tibble::tibble(age = 1:2, value = c(0, 1)),
          tibble::tibble(age = 1:2, value = c(0, 2))
        ),
        n_time_points = rep(2L, 4L),
        n_finite_values = rep(2L, 4L),
        n_nonzero_values = rep(1L, 4L),
        available = rep(TRUE, 4L),
        availability_status = rep("available", 4L)
      )

    testthat::expect_error(
      validate_spd_radius_products(data_spd),
      regexp = "common age grid"
    )
  }
)
