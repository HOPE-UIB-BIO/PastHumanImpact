testthat::test_that("compute_mvpart_change_points() returns split indices", {
  summary_backend <-
    purrr::as_mapper(
      ~ list(splits = data.frame(index = c(100, 200)))
    )

  vec_result <-
    compute_mvpart_change_points(
      model = list(),
      summary_backend = summary_backend
    )

  testthat::expect_equal(vec_result, c(100, 200))
})
