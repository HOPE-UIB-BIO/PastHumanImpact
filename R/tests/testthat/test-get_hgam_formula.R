testthat::test_that("get_hgam_formula builds a common trend formula", {
  result <-
    get_hgam_formula(
      x_var = "age",
      y_var = "response",
      group_var = "dataset_id",
      smooth_basis = "tp",
      sel_k = 10,
      n_groups = 3,
      common_trend = TRUE
    )

  testthat::expect_identical(
    result,
    "response ~ s(age, k = 10, bs = 'tp') + s(age, by = dataset_id, bs = 'tp', k = 10, m = 1) + s(dataset_id, bs = 're', k = 3)"
  )
})

testthat::test_that("get_hgam_formula builds a group-specific formula", {
  result <-
    get_hgam_formula(
      x_var = "time",
      y_var = "y",
      group_var = "group",
      smooth_basis = "cr",
      sel_k = 7,
      sel_m = 2,
      n_groups = 4,
      common_trend = FALSE
    )

  testthat::expect_identical(
    result,
    "y ~ s(time, by = group, bs = 'cr', k = 7, m = 2) + s(group, bs = 're', k = 4)"
  )
})

testthat::test_that("get_hgam_formula derives sel_m from common trend", {
  result_true <-
    get_hgam_formula(
      n_groups = 2,
      common_trend = TRUE
    )

  result_false <-
    get_hgam_formula(
      n_groups = 2,
      common_trend = FALSE
    )

  testthat::expect_identical(
    result_true,
    "var ~ s(age, k = 10, bs = 'tp') + s(age, by = dataset_id, bs = 'tp', k = 10, m = 1) + s(dataset_id, bs = 're', k = 2)"
  )

  testthat::expect_identical(
    result_false,
    "var ~ s(age, by = dataset_id, bs = 'tp', k = 10, m = 2) + s(dataset_id, bs = 're', k = 2)"
  )
})

testthat::test_that("get_hgam_formula validates n_groups", {
  testthat::expect_error(
    get_hgam_formula(n_groups = 0),
    regexp = "n_groups"
  )
})