testthat::test_that("get_pap_reduced_vars() returns a non-empty vector", {
  data_collinearity <-
    list(
      group_diagnostics = tibble::tibble(
        region = c("A", "B", "C"),
        n_rows = c(20, 20, 20),
        is_eligible = c(TRUE, TRUE, TRUE)
      ),
      selection_table = tibble::tibble(
        region = c("A", "A", "B", "B", "C"),
        n_rows = c(20, 20, 20, 20, 20),
        is_eligible = c(TRUE, TRUE, TRUE, TRUE, TRUE),
        predictor = c("n0", "n1", "n0", "n2", "n0")
      )
    )

  output <-
    get_pap_reduced_vars(
      data_collinearity = data_collinearity,
      min_selected_fraction = 0.5
    )

  testthat::expect_type(output, "character")
  testthat::expect_true(length(output) >= 1)
  testthat::expect_true("n0" %in% output)
})

testthat::test_that("get_pap_reduced_vars() validates required inputs", {
  testthat::expect_error(
    get_pap_reduced_vars(
      data_collinearity = list(group_diagnostics = tibble::tibble())
    ),
    regexp = "must include"
  )

  data_collinearity <-
    list(
      group_diagnostics = tibble::tibble(
        region = c("A"),
        n_rows = c(20),
        is_eligible = c(TRUE)
      ),
      selection_table = tibble::tibble(
        region = c("A"),
        n_rows = c(20),
        is_eligible = c(TRUE),
        predictor = c("n0")
      )
    )

  testthat::expect_error(
    get_pap_reduced_vars(
      data_collinearity = data_collinearity,
      min_selected_fraction = 0
    ),
    regexp = "must be in"
  )
})

testthat::test_that("get_pap_reduced_vars() errors on edge-case selections", {
  data_no_eligible <-
    list(
      group_diagnostics = tibble::tibble(
        region = c("A"),
        n_rows = c(20),
        is_eligible = c(FALSE)
      ),
      selection_table = tibble::tibble(
        region = c("A"),
        n_rows = c(20),
        is_eligible = c(FALSE),
        predictor = c("n0")
      )
    )

  testthat::expect_error(
    get_pap_reduced_vars(
      data_collinearity = data_no_eligible,
      min_selected_fraction = 0.5
    ),
    regexp = "No eligible groups"
  )

  data_no_selected <-
    list(
      group_diagnostics = tibble::tibble(
        region = c("A"),
        n_rows = c(20),
        is_eligible = c(TRUE)
      ),
      selection_table = tibble::tibble(
        region = c("A"),
        n_rows = c(20),
        is_eligible = c(TRUE),
        predictor = c(NA_character_)
      )
    )

  testthat::expect_error(
    get_pap_reduced_vars(
      data_collinearity = data_no_selected,
      min_selected_fraction = 0.5
    ),
    regexp = "No selected PAP variables"
  )
})
