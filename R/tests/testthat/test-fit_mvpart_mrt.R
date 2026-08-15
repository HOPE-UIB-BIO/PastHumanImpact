testthat::test_that("fit_mvpart_mrt() returns plain-data summaries", {
  data_counts <-
    data.frame(
      sample_id = c("a", "b", "c"),
      taxon_a = c(5, 4, 1),
      taxon_b = c(1, 2, 5)
    )

  data_levels <-
    data.frame(
      sample_id = c("a", "b", "c"),
      age = c(0, 500, 1000)
    )

  fit_backend <-
    purrr::as_mapper(~ list(where = c(2, 2, 7)))

  summary_backend <-
    purrr::as_mapper(~ list(splits = data.frame(index = 500)))

  res_result <-
    fit_mvpart_mrt(
      data_source_counts = data_counts,
      data_source_levels = data_levels,
      n_rand = 2,
      fit_backend = fit_backend,
      summary_backend = summary_backend
    )

  testthat::expect_equal(res_result[["mrt_groups"]], 2)

  testthat::expect_equal(res_result[["change_points"]], 500)
})
