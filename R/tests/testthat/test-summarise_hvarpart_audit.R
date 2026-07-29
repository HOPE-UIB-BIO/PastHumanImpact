testthat::test_that("audit counts models rather than predictor rows", {
  data_importance <-
    tibble::tibble(
      analysis = rep("spatial_spd", 6),
      model_id = rep(c("a", "b", "c"), each = 2),
      predictor = rep(c("human", "climate"), 3),
      is_importance_eligible = rep(c(TRUE, FALSE, FALSE), each = 2),
      exclusion_reason = rep(
        c(NA_character_, "non_positive_total", "missing_result"),
        each = 2
      ),
      has_negative_unique = rep(c(TRUE, FALSE, FALSE), each = 2),
      has_negative_individual = rep(c(TRUE, FALSE, FALSE), each = 2)
    )

  data_audit <-
    summarise_hvarpart_audit(
      data_importance = data_importance,
      group_vars = "analysis"
    )

  testthat::expect_identical(data_audit[["n_models"]], 3L)
  testthat::expect_identical(data_audit[["n_eligible"]], 1L)
  testthat::expect_identical(data_audit[["n_excluded"]], 2L)
  testthat::expect_identical(
    data_audit[["n_non_positive_total"]],
    1L
  )
  testthat::expect_identical(data_audit[["n_missing_result"]], 1L)
  testthat::expect_identical(
    data_audit[["n_negative_individual"]],
    1L
  )
})
