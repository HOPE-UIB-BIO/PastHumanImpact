testthat::test_that("prepare_spd_analysis_products() selects radii", {
  data_spd_by_radius <-
    tibble::tibble(
      dataset_id = rep(c("a", "b", "c"), each = 2L),
      radius_km = rep(c(250L, 500L), times = 3L),
      available = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
      spd = purrr::map(
        seq_len(6L),
        ~ tibble::tibble(age = 0, value = as.numeric(.x))
      )
    )

  result <-
    prepare_spd_analysis_products(data_spd_by_radius)

  testthat::expect_named(
    result,
    c(
      "data_spd_strict_250",
      "data_spd_strict_500",
      "data_spd_250_with_500_fallback"
    )
  )
  testthat::expect_identical(
    dplyr::pull(result[["data_spd_strict_250"]], distance),
    rep(250, 3L)
  )
  testthat::expect_identical(
    dplyr::pull(result[["data_spd_strict_500"]], distance),
    rep(500, 3L)
  )
  testthat::expect_identical(
    dplyr::pull(
      result[["data_spd_250_with_500_fallback"]],
      distance
    ),
    c(250, 500, 500)
  )
})

testthat::test_that("prepare_spd_analysis_products() requires both radii", {
  data_spd_by_radius <-
    tibble::tibble(
      dataset_id = "a",
      radius_km = 250L,
      available = TRUE,
      spd = list(tibble::tibble(age = 0, value = 1))
    )

  testthat::expect_error(
    prepare_spd_analysis_products(data_spd_by_radius),
    regexp = "one 250 km and one 500 km"
  )
})
