testthat::test_that("get_data_m2() returns grouped m2 summaries", {
  testthat::skip_if_not_installed("vegan")

  data_meta <-
    data.frame(
      dataset_id = c(1L, 2L),
      region = c("north", "north"),
      climatezone = c("temperate", "temperate"),
      stringsAsFactors = FALSE
    )

  data_nested_1 <-
    data.frame(
      age = c(100, 200, 300),
      n0 = c(10, 11, 12),
      n1 = c(9, 10, 11),
      n2 = c(8, 9, 10),
      n1_minus_n2 = c(1, 1, 1),
      n2_divided_by_n1 = c(0.8, 0.9, 0.91),
      n1_divided_by_n0 = c(0.9, 0.91, 0.92),
      roc = c(0.2, 0.3, 0.4),
      dcca_axis_1 = c(0.1, 0.2, 0.25),
      density_diversity = c(0.3, 0.4, 0.5),
      stringsAsFactors = FALSE
    )

  data_nested_2 <-
    data.frame(
      age = c(100, 200, 300),
      n0 = c(12, 13, 14),
      n1 = c(11, 12, 13),
      n2 = c(10, 11, 12),
      n1_minus_n2 = c(1, 1, 1),
      n2_divided_by_n1 = c(0.9, 0.92, 0.93),
      n1_divided_by_n0 = c(0.92, 0.93, 0.94),
      roc = c(0.25, 0.35, 0.45),
      dcca_axis_1 = c(0.15, 0.25, 0.3),
      density_diversity = c(0.35, 0.45, 0.55),
      stringsAsFactors = FALSE
    )

  data_source <-
    data.frame(
      dataset_id = c(1L, 2L),
      data_merge = I(list(data_nested_1, data_nested_2)),
      stringsAsFactors = FALSE
    )

  res_data <-
    get_data_m2(
      data_source = data_source,
      data_meta = data_meta,
      min_samples = 2,
      select_vars = c(
        "age",
        "n0", "n1", "n2", "n1_minus_n2",
        "n2_divided_by_n1", "n1_divided_by_n0",
        "roc", "dcca_axis_1", "density_diversity"
      )
    )

  testthat::expect_true(all(c("region", "climatezone", "m2", "m2_time", "m2_time_df", "PCoA") %in% names(res_data)))
  testthat::expect_equal(nrow(res_data), 1L)
})

testthat::test_that("get_data_m2() validates required columns", {
  bad_source <-
    data.frame(
      dataset_id = 1L,
      stringsAsFactors = FALSE
    )

  data_meta <-
    data.frame(
      dataset_id = 1L,
      region = "north",
      climatezone = "temperate",
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_data_m2(
      data_source = bad_source,
      data_meta = data_meta,
      min_samples = 1,
      select_vars = "age"
    ),
    regexp = "data_merge"
  )
})
