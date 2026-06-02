testthat::test_that("get_pap_collinearity() returns expected list structure", {
  testthat::skip_if_not_installed("collinear")

  set.seed(900723)

  data_input <-
    tibble::tibble(
      region = rep(c("A", "B"), each = 30),
      n0 = stats::rnorm(60),
      n1 = n0 + stats::rnorm(60, sd = 0.1),
      n2 = stats::rnorm(60),
      roc = stats::rnorm(60),
      dcca_axis_1 = stats::rnorm(60),
      density_diversity = stats::rnorm(60),
      density_turnover = stats::rnorm(60)
    )

  output <-
    get_pap_collinearity(
      data_source = data_input,
      pap_vars = c(
        "n0",
        "n1",
        "n2",
        "roc",
        "dcca_axis_1",
        "density_diversity",
        "density_turnover"
      ),
      group_var = "region",
      preference_order = c(
        "n0",
        "n1",
        "n2",
        "roc",
        "dcca_axis_1",
        "density_diversity",
        "density_turnover"
      ),
      max_cor = 0.8,
      max_vif = 5,
      min_rows = 10,
      quiet = TRUE
    )

  testthat::expect_type(output, "list")
  testthat::expect_true("group_diagnostics" %in% names(output))
  testthat::expect_true("correlation_table" %in% names(output))
  testthat::expect_true("high_collinearity_pairs" %in% names(output))
  testthat::expect_true("selection_table" %in% names(output))
  testthat::expect_true(nrow(output[["group_diagnostics"]]) == 2)
})

testthat::test_that("get_pap_collinearity() validates required inputs", {
  set.seed(900723)

  testthat::expect_error(
    get_pap_collinearity(data_source = "not_a_data_frame"),
    regexp = "must be a data frame"
  )

  data_input <-
    tibble::tibble(
      region = rep("A", 12),
      n0 = stats::rnorm(12),
      n1 = stats::rnorm(12)
    )

  testthat::expect_error(
    get_pap_collinearity(
      data_source = data_input,
      pap_vars = c("n0", "n2"),
      group_var = "region"
    ),
    regexp = "must exist"
  )

  testthat::expect_error(
    get_pap_collinearity(
      data_source = data_input,
      pap_vars = c("n0", "n1"),
      group_var = "region",
      max_cor = 1.2
    ),
    regexp = "between 0 and 1"
  )
})
