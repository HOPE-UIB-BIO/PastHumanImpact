make_importance_fixture <- function() {
  res_fixture <-
    tibble::tibble(
      region = rep("Europe", 4),
      model_id = rep(c("a", "b"), each = 2),
      predictor = rep(c("human", "climate"), 2),
      individual = c(-0.02, 0.12, 0.2, 0.3),
      total_adjusted_r_squared = c(0.1, 0.1, 0.5, 0.5),
      has_negative_individual = c(TRUE, TRUE, FALSE, FALSE),
      is_importance_eligible = TRUE
    )

  return(res_fixture)
}

testthat::test_that("signed profile pools unmodified contributions", {
  data_summary <-
    summarise_hvarpart_importance(
      data_importance = make_importance_fixture(),
      group_vars = "region",
      profile = "signed"
    )

  testthat::expect_equal(
    data_summary |>
      dplyr::filter(.data[["predictor"]] == "human") |>
      dplyr::pull(.data[["pooled_allocation"]]),
    0.18 / 0.6
  )
  testthat::expect_equal(
    sum(data_summary[["pooled_allocation"]]),
    1
  )
})

testthat::test_that("zero-truncated profile recomputes model totals", {
  data_summary <-
    summarise_hvarpart_importance(
      data_importance = make_importance_fixture(),
      group_vars = "region",
      profile = "zero_truncated"
    )

  testthat::expect_equal(
    data_summary |>
      dplyr::filter(.data[["predictor"]] == "human") |>
      dplyr::pull(.data[["pooled_allocation"]]),
    0.2 / 0.62
  )
  testthat::expect_equal(
    sum(data_summary[["pooled_allocation"]]),
    1
  )
})

testthat::test_that("negative-exclusion profile removes complete models", {
  data_summary <-
    summarise_hvarpart_importance(
      data_importance = make_importance_fixture(),
      group_vars = "region",
      profile = "exclude_negative"
    )

  testthat::expect_equal(
    data_summary |>
      dplyr::filter(.data[["predictor"]] == "human") |>
      dplyr::pull(.data[["pooled_allocation"]]),
    0.4
  )
  testthat::expect_identical(
    data_summary[["n_models"]],
    c(1L, 1L)
  )
})

testthat::test_that("summary rejects duplicate model-predictor rows", {
  data_duplicate <-
    dplyr::bind_rows(
      make_importance_fixture(),
      make_importance_fixture()[1, ]
    )

  testthat::expect_error(
    summarise_hvarpart_importance(
      data_importance = data_duplicate,
      group_vars = "region"
    ),
    regexp = "exactly once"
  )
})
