testthat::test_that(
  "compute_pooled_human_climate_allocation() pools three profiles",
  {
    data_units <-
      tibble::tibble(
        human = c(0.2, -0.1, 0.1, -0.1),
        climate = c(0.1, 0.3, 0.1, -0.1),
        space = c(0.1, 0.2, -0.05, 0)
      )

    res <-
      compute_pooled_human_climate_allocation(
        data_units = data_units,
        profile = c("zero_truncated", "signed", "exclude_negative")
      )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_named(
      res,
      c(
        "profile", "pooled_difference", "mean_difference", "n_units",
        "n_human_greater"
      )
    )
    testthat::expect_equal(
      dplyr::pull(res, "profile"),
      c("zero_truncated", "signed", "exclude_negative")
    )
    testthat::expect_equal(
      dplyr::pull(res, "pooled_difference"),
      c(-0.2 / 1.1, -0.3 / 0.75, 0.25),
      tolerance = 1e-10
    )
    testthat::expect_equal(
      dplyr::pull(res, "mean_difference"),
      c(-0.35 / 3, -0.25, 0.25),
      tolerance = 1e-10
    )
    testthat::expect_equal(dplyr::pull(res, "n_units"), c(3L, 4L, 1L))
    testthat::expect_equal(dplyr::pull(res, "n_human_greater"), c(1L, 1L, 1L))
  }
)

testthat::test_that(
  "compute_pooled_human_climate_allocation() handles no retained units",
  {
    data_units <-
      tibble::tibble(human = -0.1, climate = 0.2, space = 0.1)

    res <-
      compute_pooled_human_climate_allocation(
        data_units = data_units,
        profile = "exclude_negative"
      )

    testthat::expect_true(base::is.na(dplyr::pull(res, "pooled_difference")))
    testthat::expect_equal(dplyr::pull(res, "n_units"), 0L)
  }
)

testthat::test_that(
  "compute_pooled_human_climate_allocation() validates its inputs",
  {
    testthat::expect_error(
      compute_pooled_human_climate_allocation(
        data_units = tibble::tibble(human = 0.1),
        profile = "signed"
      ),
      regexp = "Pooled allocation inputs"
    )

    testthat::expect_error(
      compute_pooled_human_climate_allocation(
        data_units = tibble::tibble(human = 0.1, climate = 0.1, space = 0),
        profile = "pure"
      ),
      regexp = "Pooled allocation inputs"
    )
  }
)
