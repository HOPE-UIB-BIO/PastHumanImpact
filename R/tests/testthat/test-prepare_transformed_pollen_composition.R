testthat::test_that(
  "prepare_transformed_pollen_composition() preserves identifiers",
  {
    data_percentages <-
      data.frame(
        sample_id = c("a", "b"),
        taxon_a = c(25, 100),
        taxon_b = c(75, 0)
      )

    data_result <-
      prepare_transformed_pollen_composition(
        data_percentages = data_percentages,
        transformation = "hellinger"
      )

    testthat::expect_identical(
      data_result[["sample_id"]],
      c("a", "b")
    )

    testthat::expect_equal(data_result[["taxon_a"]], c(0.5, 1))
  }
)
