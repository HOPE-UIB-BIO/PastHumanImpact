testthat::test_that(
  "summarise_hvarpart_adjr2_distribution() returns distribution statistics",
  {
    data_values <-
      tibble::tibble(
        group = c("a", "a", "a", "b"),
        adjusted_r_squared = c(-0.2, 0.2, 0.6, 0.4)
      )

    res_summary <-
      summarise_hvarpart_adjr2_distribution(
        data_values = data_values,
        group_vars = "group"
      )

    testthat::expect_equal(res_summary[["n_models"]], c(3L, 1L))
    testthat::expect_equal(
      res_summary[["median_adjusted_r_squared"]],
      c(0.2, 0.4)
    )
    testthat::expect_equal(
      res_summary[["mean_adjusted_r_squared"]],
      c(0.2, 0.4)
    )
    testthat::expect_equal(
      res_summary[["q1_adjusted_r_squared"]],
      c(0, 0.4)
    )
    testthat::expect_equal(
      res_summary[["q3_adjusted_r_squared"]],
      c(0.4, 0.4)
    )
  }
)

testthat::test_that(
  "summarise_hvarpart_adjr2_distribution() validates inputs",
  {
    testthat::expect_error(
      summarise_hvarpart_adjr2_distribution(tibble::tibble()),
      "distribution contract"
    )
  }
)
