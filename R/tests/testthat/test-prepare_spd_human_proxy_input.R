testthat::test_that("prepare_spd_human_proxy_input() unnests common ages", {
  data_spd <-
    tibble::tibble(
      dataset_id = c(1L, 2L),
      distance = c(250, 500),
      spd = list(
        tibble::tibble(age = c(1500, 2000, 2500), value = c(1, 4, 9)),
        tibble::tibble(age = c(2000, 2500, 3000), value = c(16, 25, 36))
      )
    )

  result <-
    prepare_spd_human_proxy_input(
      data_spd = data_spd,
      age_min = 2000,
      age_max = 3000,
      age_step = 500
    )

  testthat::expect_named(
    result,
    c("dataset_id", "age_bp", "spd", "radius_km")
  )
  testthat::expect_identical(unique(result[["dataset_id"]]), c("1", "2"))
  testthat::expect_true(all(result[["age_bp"]] %in% c(2000, 2500, 3000)))
  testthat::expect_equal(result[["radius_km"]], c(250, 250, 500, 500, 500))
})
