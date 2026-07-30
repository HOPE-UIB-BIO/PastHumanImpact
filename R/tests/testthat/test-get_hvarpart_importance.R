make_hvarpart_result <- function(
  individual = c(-0.02, 0.12),
  unique = c(-0.03, 0.1),
  average_share = c(0.01, 0.02),
  individual_percent = c(-20, 120),
  total = 0.1,
  predictors = c("human", "climate")
) {
  data_hier <-
    data.frame(
      Unique = unique,
      Average.share = average_share,
      Individual = individual,
      check.names = FALSE
    )
  data_hier[["I.perc(%)"]] <- individual_percent
  rownames(data_hier) <- predictors

  res_result <-
    list(
      varhp_output = list(
        Hier.part = data_hier,
        Total_explained_variation = total
      )
    )

  return(res_result)
}

testthat::test_that("get_hvarpart_importance() preserves raw signed fields", {
  data_source <-
    tibble::tibble(
      dataset_id = "core_a",
      varhp = list(make_hvarpart_result())
    )

  res_importance <-
    get_hvarpart_importance(
      data_source = data_source,
      id_cols = "dataset_id"
    )

  testthat::expect_s3_class(res_importance, "tbl_df")
  testthat::expect_identical(nrow(res_importance), 2L)
  testthat::expect_equal(
    res_importance[["individual"]],
    c(-0.02, 0.12)
  )
  testthat::expect_equal(
    res_importance[["individual_percent"]],
    c(-20, 120)
  )
  testthat::expect_true(
    all(res_importance[["is_importance_eligible"]])
  )
  testthat::expect_true(
    all(res_importance[["has_negative_individual"]])
  )
  testthat::expect_true(
    all(is.na(res_importance[["exclusion_reason"]]))
  )
})

testthat::test_that("get_hvarpart_importance() records missing results", {
  data_source <-
    tibble::tibble(
      dataset_id = "failed",
      varhp = list(NULL)
    )

  res_importance <-
    get_hvarpart_importance(
      data_source = data_source,
      id_cols = "dataset_id"
    )

  testthat::expect_identical(nrow(res_importance), 2L)
  testthat::expect_false(
    any(res_importance[["is_importance_eligible"]])
  )
  testthat::expect_identical(
    unique(res_importance[["exclusion_reason"]]),
    "missing_result"
  )
})

testthat::test_that("get_hvarpart_importance() diagnoses invalid models", {
  data_source <-
    tibble::tibble(
      dataset_id = c(
        "missing_predictor",
        "zero_total",
        "non_finite_total",
        "non_finite_individual"
      ),
      varhp = list(
        make_hvarpart_result(
          individual = 0.1,
          unique = 0.08,
          average_share = 0.02,
          individual_percent = 100,
          predictors = "human"
        ),
        make_hvarpart_result(total = 0),
        make_hvarpart_result(total = NA_real_),
        make_hvarpart_result(
          individual = c(NA_real_, 0.12)
        )
      )
    )

  res_importance <-
    get_hvarpart_importance(
      data_source = data_source,
      id_cols = "dataset_id"
    )

  data_reasons <-
    res_importance |>
    dplyr::distinct(
      .data[["dataset_id"]],
      .data[["exclusion_reason"]]
    )

  testthat::expect_identical(
    data_reasons[["exclusion_reason"]],
    c(
      "missing_predictor",
      "non_positive_total",
      "non_finite_total",
      "non_finite_individual"
    )
  )
})

testthat::test_that("get_hvarpart_importance() validates unique model IDs", {
  data_source <-
    tibble::tibble(
      dataset_id = c("core_a", "core_a"),
      varhp = list(
        make_hvarpart_result(),
        make_hvarpart_result()
      )
    )

  testthat::expect_error(
    get_hvarpart_importance(
      data_source = data_source,
      id_cols = "dataset_id"
    ),
    regexp = "uniquely identify"
  )
})

testthat::test_that("get_hvarpart_importance() validates its contract", {
  testthat::expect_error(
    get_hvarpart_importance(
      data_source = tibble::tibble(dataset_id = "core_a"),
      id_cols = "dataset_id"
    ),
    regexp = "nested results"
  )
  testthat::expect_error(
    get_hvarpart_importance(
      data_source = tibble::tibble(
        dataset_id = "core_a",
        varhp = list(NULL)
      ),
      id_cols = "missing_id"
    ),
    regexp = "nested results"
  )
})
