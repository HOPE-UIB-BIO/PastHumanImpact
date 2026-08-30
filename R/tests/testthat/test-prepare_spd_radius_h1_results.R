testthat::test_that("prepare_spd_radius_h1_results() binds and orders", {
  results <-
    list(
      list(status = tibble::tibble(radius_km = 500L, value = 2)),
      list(status = tibble::tibble(radius_km = 250L, value = 1))
    )

  result <-
    prepare_spd_radius_h1_results(results, "status")

  testthat::expect_identical(result[["radius_km"]], c(250L, 500L))
  testthat::expect_identical(result[["value"]], c(1, 2))
})

testthat::test_that("prepare_spd_radius_h1_results() rejects absent tables", {
  testthat::expect_error(
    prepare_spd_radius_h1_results(list(list(a = tibble::tibble())), "b"),
    regexp = "contract"
  )
})
