testthat::test_that("fit_pap_change_point_dataset() returns all components", {
  table_complexity <-
    matrix(
      c(0, 0),
      nrow = 1,
      dimnames = list(NULL, c("CP", "xerror"))
    )

  fit_backend <-
    purrr::as_mapper(~ list(cptable = table_complexity))

  prune_backend <-
    purrr::as_mapper(~ ..1)

  summary_backend <-
    purrr::as_mapper(~ list(splits = data.frame(index = 500)))

  data_levels <-
    data.frame(sample_id = c("a", "b"), age = c(0, 500))

  res_result <-
    fit_pap_change_point_dataset(
      mvrt_cp = 250,
      data_diversity = data.frame(
        sample_id = c("a", "b"),
        richness = c(1, 2)
      ),
      data_levels = data_levels,
      data_roc = data.frame(
        Age = c(0, 500),
        ROC = c(0.1, 0.2),
        Peak = c(FALSE, TRUE)
      ),
      data_dcca = data.frame(
        sample_id = c("a", "b"),
        axis_1 = c(0.1, 0.2)
      ),
      fit_backend = fit_backend,
      prune_backend = prune_backend,
      summary_backend = summary_backend
    )

  testthat::expect_named(
    res_result,
    c("mvrt_cp", "diversity_cp", "roc_cp", "roc_pp", "dcca_cp")
  )
})
