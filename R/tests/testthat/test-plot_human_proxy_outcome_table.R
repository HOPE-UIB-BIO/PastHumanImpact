testthat::test_that(
  "plot_human_proxy_outcome_table() draws one panel per scale and window",
  {
    data_summary <-
      tidyr::expand_grid(
        analysis = c("spatial_h1", "temporal_h1"),
        human_proxy = c("spd", "kk10", "hyde_3_5_population"),
        gridded_support = c("spd_buffer", "radius_50km"),
        age_window = c("common_cohort", "native"),
        record_set = c("shared_records", "all_records")
      ) |>
      dplyr::mutate(
        estimate = dplyr::if_else(
          .data[["human_proxy"]] == "kk10",
          0.05,
          -0.3
        ),
        lwr_95 = .data[["estimate"]] - 0.04,
        upr_95 = .data[["estimate"]] + 0.04,
        outcome_class = dplyr::if_else(
          .data[["human_proxy"]] == "kk10",
          "human",
          "climate"
        ),
        class_changes_between_profiles =
          .data[["human_proxy"]] == "kk10" &
            .data[["record_set"]] == "shared_records"
      )

    res <-
      plot_human_proxy_outcome_table(data_summary = data_summary)

    data_built <-
      ggplot2::ggplot_build(res)

    testthat::expect_s3_class(res, "ggplot")
    testthat::expect_equal(base::nrow(data_built[["layout"]][["layout"]]), 4L)
    testthat::expect_equal(base::nrow(res[["data"]]), 32L)
    testthat::expect_false(
      "hyde_3_5_population" %in% dplyr::pull(res[["data"]], "human_proxy")
    )

    data_points <-
      data_built[["data"]] |>
      purrr::keep(.p = ~ "shape" %in% base::names(.x)) |>
      purrr::pluck(1)

    testthat::expect_setequal(
      base::unique(dplyr::pull(data_points, "colour")),
      c(palette_predictors[["human"]], palette_predictors[["climate"]])
    )

    data_flags <-
      data_built[["data"]] |>
      purrr::keep(.p = ~ "label" %in% base::names(.x)) |>
      purrr::pluck(1)

    testthat::expect_equal(base::nrow(data_flags), 8L)
  }
)

testthat::test_that(
  "plot_human_proxy_outcome_table() shows no clear dominance in grey",
  {
    data_summary <-
      tibble::tibble(
        analysis = "spatial_h1",
        human_proxy = "spd",
        gridded_support = "spd_buffer",
        age_window = "native",
        record_set = "all_records",
        estimate = 0.01,
        lwr_95 = -0.02,
        upr_95 = 0.04,
        outcome_class = "no_clear_dominance",
        class_changes_between_profiles = FALSE
      )

    data_built <-
      plot_human_proxy_outcome_table(data_summary = data_summary) |>
      ggplot2::ggplot_build()

    data_points <-
      data_built[["data"]] |>
      purrr::keep(.p = ~ "shape" %in% base::names(.x)) |>
      purrr::pluck(1)

    testthat::expect_equal(dplyr::pull(data_points, "colour"), common_gray)
  }
)

testthat::test_that(
  "plot_human_proxy_outcome_table() validates its inputs",
  {
    testthat::expect_error(
      plot_human_proxy_outcome_table(
        data_summary = tibble::tibble(analysis = "spatial_h1")
      ),
      regexp = "Outcome-table plot inputs"
    )

    testthat::expect_error(
      plot_human_proxy_outcome_table(
        data_summary = tibble::tibble(
          analysis = "spatial_h1",
          human_proxy = "events",
          gridded_support = "spd_buffer",
          age_window = "native",
          record_set = "all_records",
          estimate = 0.1,
          lwr_95 = 0,
          upr_95 = 0.2,
          outcome_class = "human",
          class_changes_between_profiles = FALSE
        )
      ),
      regexp = "Outcome-table plot inputs"
    )
  }
)
