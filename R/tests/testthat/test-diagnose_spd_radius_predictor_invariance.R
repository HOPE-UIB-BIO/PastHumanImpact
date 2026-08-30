testthat::test_that(
  "diagnose_spd_radius_predictor_invariance() finds changes",
  {
  data_source <-
    tibble::tibble(
      dataset_id = c(1L, 1L, 2L, 2L),
      radius_km = rep(c(250L, 500L), 2),
      data_merge = list(
        tibble::tibble(age = 0, spd = 1, climate = 1),
        tibble::tibble(age = 0, spd = 2, climate = 1),
        tibble::tibble(age = 0, spd = 1, climate = 1),
        tibble::tibble(age = 0, spd = 2, climate = 2)
      )
    )

  result <-
    diagnose_spd_radius_predictor_invariance(data_source)

  testthat::expect_identical(
    result[["non_spd_identical"]],
    c(TRUE, FALSE)
  )
  }
)
