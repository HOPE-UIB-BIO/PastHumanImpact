testthat::test_that("compute_pap_change_points() validates required columns", {
  bad_source <-
    data.frame(
      dataset_id = 1,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    compute_pap_change_points(data_source = bad_source),
    regexp = "missing required columns"
  )
})

testthat::test_that("compute_pap_change_points() validates nested table columns", {
  bad_source <-
    data.frame(
      dataset_id = 1,
      mvrt_cp = I(list(numeric(0))),
      PAP_diversity = I(list(1)),
      levels = I(list(data.frame(sample_id = "s1", age = 100))),
      PAP_roc = I(list(data.frame(Age = 100, ROC = 1, Peak = FALSE))),
      dcca_scores = I(list(data.frame(sample_id = "s1", axis_1 = 0.1))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    compute_pap_change_points(data_source = bad_source),
    regexp = "PAP_diversity"
  )
})

testthat::test_that("compute_pap_change_points() validates seed", {
  data_source <-
    data.frame(
      dataset_id = 1,
      mvrt_cp = I(list(numeric(0))),
      PAP_diversity = I(list(data.frame(sample_id = "s1", n0 = 1))),
      levels = I(list(data.frame(sample_id = "s1", age = 100))),
      PAP_roc = I(list(data.frame(Age = 100, ROC = 1, Peak = FALSE))),
      dcca_scores = I(list(data.frame(sample_id = "s1", axis_1 = 0.1))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    compute_pap_change_points(data_source = data_source, seed = 1.5),
    regexp = "single whole number"
  )

  testthat::expect_error(
    compute_pap_change_points(data_source = data_source, seed = NA_integer_),
    regexp = "single whole number"
  )

  testthat::expect_error(
    compute_pap_change_points(data_source = data_source, seed = c(1L, 2L)),
    regexp = "single whole number"
  )

  testthat::expect_error(
    compute_pap_change_points(data_source = data_source, seed = "1"),
    regexp = "single whole number"
  )
})

testthat::test_that("compute_pap_change_points() reseeds every dataset", {
  data_diversity <-
    data.frame(sample_id = "s1", n0 = 1)

  data_levels <-
    data.frame(sample_id = "s1", age = 100)

  data_roc <-
    data.frame(Age = 100, ROC = 1, Peak = FALSE)

  data_dcca <-
    data.frame(sample_id = "s1", axis_1 = 0.1)

  data_source <-
    data.frame(
      dataset_id = c(1, 2),
      mvrt_cp = I(list(numeric(0), numeric(0))),
      PAP_diversity = I(list(data_diversity, data_diversity)),
      levels = I(list(data_levels, data_levels)),
      PAP_roc = I(list(data_roc, data_roc)),
      dcca_scores = I(list(data_dcca, data_dcca)),
      stringsAsFactors = FALSE
    )

  fit_dataset <-
    purrr::as_mapper(
      ~ list(
        mvrt_cp = numeric(0),
        diversity_cp = data.frame(var_name = character(0), age = numeric(0)),
        roc_cp = stats::runif(n = 3L),
        roc_pp = numeric(0),
        dcca_cp = numeric(0)
      )
    )

  base::set.seed(900723L)

  vec_expected <-
    stats::runif(n = 3L)

  res <-
    compute_pap_change_points(
      data_source = data_source,
      fit_dataset = fit_dataset,
      seed = 900723L
    )

  list_roc_cp <-
    res[["roc_cp"]]

  testthat::expect_equal(list_roc_cp[[1]], vec_expected)
  testthat::expect_equal(list_roc_cp[[2]], vec_expected)
})
