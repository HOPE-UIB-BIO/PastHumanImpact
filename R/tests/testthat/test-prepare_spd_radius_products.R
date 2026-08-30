testthat::test_that("prepare_spd_radius_products() makes explicit radii", {
  data_source <-
    tibble::tibble(
      dataset_id = 1L,
      spd = list(
        tibble::tibble(
          age = c(1, 0),
          `250` = c(0, 0),
          `500` = c(0.2, 0.1)
        )
      )
    )

  result <-
    prepare_spd_radius_products(data_source)

  testthat::expect_identical(result[["radius_km"]], c(250L, 500L))
  testthat::expect_identical(result[["available"]], c(FALSE, TRUE))
  testthat::expect_identical(
    result[["availability_status"]],
    c("unavailable", "available")
  )
  testthat::expect_identical(result[["n_time_points"]], c(2L, 2L))
  testthat::expect_identical(
    result[["spd"]][[2]][["value"]],
    c(0.2, 0.1)
  )
})

testthat::test_that("prepare_spd_radius_products() validates nested input", {
  testthat::expect_error(
    prepare_spd_radius_products(
      tibble::tibble(dataset_id = 1L, spd = list(1))
    ),
    regexp = "required contract"
  )
})
