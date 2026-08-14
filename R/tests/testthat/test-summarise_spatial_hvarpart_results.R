testthat::test_that(
  "summarise_spatial_hvarpart_results() exports stable schemas",
  {
    empty_selection <-
      build_empty_dbmem_selection(
        status_value = "no_spatial_signal",
        n_complete = 12L,
        n_candidates = 2L
      )
    result_value <-
      list(
        status = "no_spatial_terms_selected",
        n_samples = 12L,
        human_climate_only_hvarpart = NULL,
        spatial_hvarpart = NULL,
        dbmem = list(
          diagnostics = tibble::tibble(
            spatial_group = "all",
            status = "estimated"
          )
        ),
        selection = empty_selection,
        unique_adjusted_r2 = tibble::tibble(),
        residual_moran = tibble::tibble(),
        remaining_spatial_test = tibble::tibble()
      )
    data_input <-
      tibble::tibble(
        analysis = "temporal_spd",
        region = "Europe",
        age = 2000,
        result = list(result_value)
      )
    result <- summarise_spatial_hvarpart_results(data_input)

    testthat::expect_named(
      result,
      c(
        "status", "selection", "dbmem_diagnostics", "components",
        "unique_adjusted_r2", "residual_moran",
        "remaining_spatial_test"
      )
    )
    testthat::expect_equal(
      result$status$selection_status,
      "no_spatial_signal"
    )
    testthat::expect_equal(
      result$dbmem_diagnostics$spatial_group,
      "all"
    )
  }
)
