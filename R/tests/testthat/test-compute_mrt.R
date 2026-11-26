testthat::test_that("compute_mrt() validates required columns", {
  bad_input <-
    data.frame(
      dataset_id = 1,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    compute_mrt(data_pollen = bad_input),
    regexp = "percentages_harmonised"
  )
})

testthat::test_that("compute_mrt() validates n_rand", {
  data_pollen <-
    data.frame(
      dataset_id = 1,
      percentages_harmonised = I(list(data.frame(sample_id = "s1", a = 1))),
      levels = I(list(data.frame(sample_id = "s1", age = 100))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    compute_mrt(data_pollen = data_pollen, n_rand = -1),
    regexp = "positive"
  )
})

testthat::test_that("compute_mrt() validates seed", {
  data_pollen <-
    data.frame(
      dataset_id = 1,
      percentages_harmonised = I(list(data.frame(sample_id = "s1", a = 1))),
      levels = I(list(data.frame(sample_id = "s1", age = 100))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    compute_mrt(data_pollen = data_pollen, seed = 1.5),
    regexp = "single whole number"
  )

  testthat::expect_error(
    compute_mrt(data_pollen = data_pollen, seed = NA_integer_),
    regexp = "single whole number"
  )

  testthat::expect_error(
    compute_mrt(data_pollen = data_pollen, seed = c(1L, 2L)),
    regexp = "single whole number"
  )
})

testthat::test_that("compute_mrt() reseeds every dataset", {
  data_counts <-
    data.frame(sample_id = "s1", a = 1)

  data_levels <-
    data.frame(sample_id = "s1", age = 100)

  data_pollen <-
    data.frame(
      dataset_id = c(1, 2),
      percentages_harmonised = I(list(data_counts, data_counts)),
      levels = I(list(data_levels, data_levels)),
      stringsAsFactors = FALSE
    )

  fit_dataset <-
    purrr::as_mapper(
      ~ list(
        partitions = data.frame(sample_id = "s1", MRT_partitions = 1),
        change_points = stats::runif(n = 3L),
        mrt_groups = 1
      )
    )

  base::set.seed(900723L)

  vec_expected <-
    stats::runif(n = 3L)

  res <-
    compute_mrt(
      data_pollen = data_pollen,
      fit_dataset = fit_dataset,
      seed = 900723L
    )

  list_change_points <-
    res[["mvrt_cp"]]

  testthat::expect_equal(list_change_points[[1]], vec_expected)
  testthat::expect_equal(list_change_points[[2]], vec_expected)
})
