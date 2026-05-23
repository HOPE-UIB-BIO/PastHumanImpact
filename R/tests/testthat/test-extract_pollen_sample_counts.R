testthat::test_that("extract_pollen_sample_counts() expands nested sample data", {
  data_source <-
    tibble::tibble(
      dataset_id = "d1",
      region = "Europe",
      ecozone_koppen_5 = "Cold",
      ecozone_koppen_15 = "Cold_Without_dry_season_Warm_Summer",
      ecozone_koppen_30 = "Cold_Without_dry_season_Warm_Summer",
      pollen_percentage = FALSE,
      levels = list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          age = c(100, 650)
        )
      ),
      counts_harmonised = list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          taxon_a = c(10, 200),
          taxon_b = c(20, 400)
        )
      )
    )

  res_table <-
    extract_pollen_sample_counts(
      data_source = data_source,
      stage_label = "pre_filter",
      time_step = 500
    )

  testthat::expect_s3_class(res_table, "tbl_df")
  testthat::expect_true(all(c("climatezone", "grain_bin", "rowsum") %in% names(res_table)))
  testthat::expect_equal(dplyr::n_distinct(dplyr::pull(res_table, sample_id)), 2)
  testthat::expect_equal(dplyr::pull(res_table, climatezone)[1], "Cold_Without_dry_season_Warm_Summer")
  testthat::expect_equal(dplyr::pull(res_table, grain_bin), c("25-149", ">=500"))
})

testthat::test_that("extract_pollen_sample_counts() validates required columns", {
  data_source <-
    tibble::tibble(
      dataset_id = "d1",
      region = "Europe"
    )

  testthat::expect_error(
    extract_pollen_sample_counts(
      data_source = data_source,
      stage_label = "pre_filter"
    ),
    "missing required columns"
  )
})
