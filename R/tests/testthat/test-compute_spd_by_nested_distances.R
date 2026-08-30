testthat::test_that("nested-distance SPD retains unavailable radius rows", {
  data_source <-
    tibble::tibble(
      dataset_id = "test",
      curve_name = "intcal20",
      rc = list(
        tibble::tibble(
          LabID = "a",
          Age = 1000,
          Error = 30,
          dist = 100
        )
      )
    )

  result <-
    compute_spd_by_nested_distances(
      data_source_c14 = data_source,
      data_source_dist_vec = c(`250` = 250, `500` = 500),
      age_from = 0,
      age_to = 10,
      min_n_dates = 50
    )

  testthat::expect_identical(
    names(result[["spd"]][[1]]),
    c("age", "250", "500")
  )
  testthat::expect_true(all(result[["spd"]][[1]][["250"]] == 0))
  testthat::expect_true(all(result[["spd"]][[1]][["500"]] == 0))
})
