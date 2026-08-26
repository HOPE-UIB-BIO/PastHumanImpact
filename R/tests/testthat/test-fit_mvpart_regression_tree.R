testthat::test_that("fit_mvpart_regression_tree() prunes and extracts splits", {
  table_complexity <-
    matrix(
      c(0.2, 0.4, 0.1, 0.2),
      nrow = 2,
      dimnames = list(NULL, c("CP", "xerror"))
    )

  fit_backend <-
    purrr::as_mapper(~ list(cptable = table_complexity))

  prune_backend <-
    purrr::as_mapper(~ ..1)

  summary_backend <-
    purrr::as_mapper(~ list(splits = data.frame(index = 250)))

  vec_result <-
    fit_mvpart_regression_tree(
      data_source = data.frame(value = 1:4, age = seq(0, 1500, 500)),
      response_name = "value",
      age_name = "age",
      fit_backend = fit_backend,
      prune_backend = prune_backend,
      summary_backend = summary_backend
    )

  testthat::expect_equal(vec_result, 250)
})
