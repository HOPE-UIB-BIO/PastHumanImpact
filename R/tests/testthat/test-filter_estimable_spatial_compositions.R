testthat::test_that(
  "filter_estimable_spatial_compositions() keeps complete stacks",
  {
    data_composition <-
      tibble::tibble(
        region = "A",
        age = base::rep(c(2000, 2500, 3000, 3500), each = 3),
        predictor = base::rep(c("human", "climate", "space"), times = 4),
        allocation = c(
          0.2, 0.5, 0.3,
          0.2, 0.5, 0.2,
          NA, NA, NA,
          0.1, 0.6, 0.3
        ),
        status = c(
          base::rep("spatial_model_estimated", 9),
          base::rep("insufficient_residual_df", 3)
        )
      )

    res <-
      filter_estimable_spatial_compositions(
        data_composition = data_composition
      )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_named(res, base::names(data_composition))
    testthat::expect_equal(base::unique(dplyr::pull(res, "age")), 2000)
    testthat::expect_equal(base::nrow(res), 3L)
  }
)

testthat::test_that(
  "filter_estimable_spatial_compositions() needs all three predictors",
  {
    data_composition <-
      tibble::tibble(
        region = "A",
        age = 2000,
        predictor = c("human", "climate"),
        allocation = c(0.4, 0.6),
        status = "spatial_model_estimated"
      )

    res <-
      filter_estimable_spatial_compositions(
        data_composition = data_composition
      )

    testthat::expect_equal(base::nrow(res), 0L)
  }
)

testthat::test_that(
  "filter_estimable_spatial_compositions() validates its inputs",
  {
    testthat::expect_error(
      filter_estimable_spatial_compositions(
        data_composition = tibble::tibble(region = "A")
      ),
      regexp = "Composition inputs"
    )
  }
)
