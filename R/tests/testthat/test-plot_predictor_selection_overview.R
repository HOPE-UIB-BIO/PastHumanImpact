testthat::test_that("plot_predictor_selection_overview() returns a heatmap", {
  by_continental_region <- tibble::tibble(
    continental_region = "Europe", group = c("human", "climate"),
    predictor = c("spd_sqrt", "temp_annual"), n_datasets = 10L,
    n_selected = c(10L, 8L), retention_rate = c(1, 0.8)
  )
  by_region <- dplyr::bind_rows(
    by_continental_region |>
      dplyr::select(-"continental_region") |>
      dplyr::mutate(region = "Polar", .before = 1),
    by_continental_region |>
      dplyr::select(-"continental_region") |>
      dplyr::mutate(
        region = "Cold_Without_dry_season_Warm_Summer",
        .before = 1
      )
  )
  result <- plot_predictor_selection_overview(
    selection_frequency_by_continental_region = by_continental_region,
    selection_frequency_by_region = by_region
  )
  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_equal(length(result[["layers"]]), 2L)
  testthat::expect_false(any(is.na(dplyr::pull(result[["data"]], area))))
  testthat::expect_setequal(
    as.character(unique(dplyr::pull(result[["data"]], geography))),
    c("Continental region", "Region")
  )
  testthat::expect_false(
    any(grepl(
      "climate zone",
      as.character(dplyr::pull(result[["data"]], geography)),
      ignore.case = TRUE
    ))
  )
})

testthat::test_that("plot_predictor_selection_overview() validates inputs", {
  testthat::expect_error(
    plot_predictor_selection_overview(data.frame(x = 1), data.frame(x = 1)),
    "contract"
  )
})
