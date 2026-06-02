testthat::test_that("summarise_pollen_bin_distribution() returns grouped counts and proportions", {
  data_source <-
    tibble::tibble(
      stage = c("pre", "pre", "pre", "post"),
      region = c("Europe", "Europe", "Asia", "Europe"),
      grain_bin = c("25-149", "25-149", ">=500", ">=500")
    )

  res_table <-
    summarise_pollen_bin_distribution(
      data_source = data_source,
      group_cols = c("stage", "region")
    )

  testthat::expect_s3_class(res_table, "tbl_df")
  testthat::expect_true(all(c("n_samples", "n_samples_total", "prop_samples") %in% names(res_table)))

  data_check <-
    res_table %>%
    dplyr::group_by(stage, region) %>%
    dplyr::summarise(
      prop_sum = sum(prop_samples),
      .groups = "drop"
    )

  testthat::expect_equal(dplyr::pull(data_check, prop_sum), rep(1, nrow(data_check)))
})

testthat::test_that("summarise_pollen_bin_distribution() validates grouping columns", {
  data_source <-
    tibble::tibble(
      stage = "pre",
      grain_bin = "25-149"
    )

  testthat::expect_error(
    summarise_pollen_bin_distribution(
      data_source = data_source,
      group_cols = c("stage", "region")
    ),
    "must exist"
  )
})
