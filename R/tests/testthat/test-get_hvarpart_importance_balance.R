testthat::test_that(
  "get_hvarpart_importance_balance() calculates bounded differences",
  {
    data_summary <-
      tibble::tibble(
        analysis = rep(c("temporal_spd", "temporal_events"), each = 2L),
        region = "Europe",
        age = 2000,
        predictor = rep(c("human", "climate"), 2L),
        pooled_allocation = c(0.25, 0.75, 0.7, 0.3),
        n_models = 4L
      )

    result <-
      get_hvarpart_importance_balance(
        data_summary = data_summary,
        group_vars = c("analysis", "region", "age")
      )

    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_named(
      result,
      c(
        "analysis",
        "region",
        "age",
        "n_models",
        "human",
        "climate",
        "importance_balance"
      )
    )
    testthat::expect_equal(
      result[["importance_balance"]],
      c(-0.5, 0.4)
    )
  }
)

testthat::test_that(
  "get_hvarpart_importance_balance() rejects incomplete pairs",
  {
    data_summary <-
      tibble::tibble(
        analysis = "temporal_spd",
        region = "Europe",
        age = 2000,
        predictor = "human",
        pooled_allocation = 1
      )

    testthat::expect_error(
      get_hvarpart_importance_balance(
        data_summary = data_summary,
        group_vars = c("analysis", "region", "age")
      ),
      "finite human and climate"
    )
  }
)

testthat::test_that(
  "get_hvarpart_importance_balance() validates unique group keys",
  {
    data_summary <-
      tibble::tibble(
        analysis = "temporal_spd",
        region = "Europe",
        age = 2000,
        predictor = c("human", "human", "climate"),
        pooled_allocation = c(0.25, 0.25, 0.75)
      )

    testthat::expect_error(
      get_hvarpart_importance_balance(
        data_summary = data_summary,
        group_vars = c("analysis", "region", "age")
      ),
      "exactly once"
    )
  }
)
