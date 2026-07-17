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
    "var ~ s(age, k = 10, bs = 'cr') + s(age, by = dataset_id, bs = 'cr', k = 10, m = 1) + s(dataset_id, bs = 're', k = 2)"
  )

  testthat::expect_identical(
    result_false,
    "var ~ s(age, by = dataset_id, bs = 'cr', k = 10, m = 2) + s(dataset_id, bs = 're', k = 2)"
  )
})

testthat::test_that("get_hgam_formula validates n_groups", {
  testthat::expect_error(
    get_hgam_formula(n_groups = 0),
    regexp = "n_groups"
  )
})

testthat::test_that("get_hgam_formula builds a stratum fs formula", {
  result <-
    get_hgam_formula(
      x_var = "age_ka",
      y_var = "value",
      group_var = "dataset_id",
      stratum_var = "stratum",
      smooth_basis = "cr",
      sel_k = 8,
      stratum_k = 5,
      model_profile = "stratum_fs"
    )

  testthat::expect_identical(
    result,
    "value ~ s(age_ka, k = 8, bs = 'cr') + s(age_ka, stratum, bs = 'fs', k = 5) + (1 | dataset_id)"
  )
})

testthat::test_that("get_hgam_formula builds a stratum fs dataset slope formula", {
  result <-
    get_hgam_formula(
      x_var = "age_ka",
      y_var = "value",
      group_var = "dataset_id",
      stratum_var = "stratum",
      smooth_basis = "cr",
      sel_k = 8,
      stratum_k = 5,
      model_profile = "stratum_fs_dataset_slope"
    )

  testthat::expect_identical(
    result,
    "value ~ s(age_ka, k = 8, bs = 'cr') + s(age_ka, stratum, bs = 'fs', k = 5) + (1 + age_ka | dataset_id)"
  )
})

testthat::test_that("get_hgam_formula builds a stratum fs dataset fs formula", {
  result <-
    get_hgam_formula(
      x_var = "age_ka",
      y_var = "value",
      group_var = "dataset_id",
      stratum_var = "stratum",
      smooth_basis = "cr",
      sel_k = 8,
      stratum_k = 5,
      group_k = 3,
      model_profile = "stratum_fs_dataset_fs"
    )

  testthat::expect_identical(
    result,
    "value ~ s(age_ka, k = 8, bs = 'cr') + s(age_ka, stratum, bs = 'fs', k = 5) + s(age_ka, dataset_id, bs = 'fs', k = 3)"
  )
})

testthat::test_that("get_hgam_formula builds within-stratum dataset formulas", {
  result_intercept <-
    get_hgam_formula(
      x_var = "age_ka",
      y_var = "value",
      group_var = "dataset_id",
      smooth_basis = "cr",
      sel_k = 8,
      model_profile = "within_stratum_dataset_intercept"
    )

  result_slope <-
    get_hgam_formula(
      x_var = "age_ka",
      y_var = "value",
      group_var = "dataset_id",
      smooth_basis = "cr",
      sel_k = 8,
      model_profile = "within_stratum_dataset_slope"
    )

  result_slope_uncorrelated <-
    get_hgam_formula(
      x_var = "age_ka",
      y_var = "value",
      group_var = "dataset_id",
      smooth_basis = "cr",
      sel_k = 8,
      model_profile = "within_stratum_dataset_slope_uncorrelated"
    )

  result_fs <-
    get_hgam_formula(
      x_var = "age_ka",
      y_var = "value",
      group_var = "dataset_id",
      smooth_basis = "cr",
      sel_k = 8,
      group_k = 3,
      model_profile = "within_stratum_dataset_fs"
    )

  testthat::expect_identical(
    result_intercept,
    "value ~ s(age_ka, k = 8, bs = 'cr') + (1 | dataset_id)"
  )
  testthat::expect_identical(
    result_slope,
    "value ~ s(age_ka, k = 8, bs = 'cr') + (1 + age_ka | dataset_id)"
  )
  testthat::expect_identical(
    result_slope_uncorrelated,
    "value ~ s(age_ka, k = 8, bs = 'cr') + (1 + age_ka || dataset_id)"
  )
  testthat::expect_identical(
    result_fs,
    "value ~ s(age_ka, k = 8, bs = 'cr') + s(age_ka, dataset_id, bs = 'fs', k = 3)"
  )
})
