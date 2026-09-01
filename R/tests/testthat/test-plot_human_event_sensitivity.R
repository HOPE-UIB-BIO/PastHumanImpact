testthat::test_that(
  "human-event sensitivity plots preserve proxy and region contracts",
  {
    regions <- c(
      "North America", "Latin America", "Europe", "Asia", "Oceania"
    )
    cohorts <- c("as_coded", "observed_events_only")
    proxies <- c("spd", "spd_events", "events")
    spatial <-
      tidyr::crossing(
        cohort = cohorts,
        region = regions,
        dataset_number = 1:2,
        proxy_variant = proxies
      ) |>
      dplyr::mutate(
        dataset_id = paste(.data[["region"]], .data[["dataset_number"]]),
        status = "estimated",
        signed_difference = rep(c(-0.1, 0, 0.1), length.out = dplyr::n()),
        zero_balance = rep(c(-0.2, 0, 0.2), length.out = dplyr::n())
      )
    summary <-
      tidyr::crossing(
        cohort = cohorts,
        summary_level = "continent",
        region = regions,
        metric = "zero_balance",
        contrast = c("spd_events_minus_spd", "events_minus_spd")
      ) |>
      dplyr::mutate(
        median = 0,
        lower_quartile = -0.05,
        upper_quartile = 0.05
      )
    temporal <-
      tidyr::crossing(
        cohort = cohorts,
        region = regions,
        age = c(2000, 2500),
        proxy_variant = proxies
      ) |>
      dplyr::mutate(
        status = "spatial_model_estimated",
        controlled_human = 0.1,
        zero_allocation_human = 0.2
      )
    younger <-
      temporal |>
      dplyr::filter(.data[["proxy_variant"]] == "events") |>
      dplyr::mutate(age = 500)
    matched <-
      tidyr::crossing(
        cohort = cohorts,
        region = regions,
        age = c(2000, 2500)
      ) |>
      dplyr::mutate(
        three_way_estimable = TRUE,
        controlled_human__spd_events_minus_spd = 0.01,
        controlled_human__events_minus_spd = -0.01,
        zero_allocation_human__spd_events_minus_spd = 0.02,
        zero_allocation_human__events_minus_spd = -0.02
      )

    spatial_plot <- plot_human_event_spatial_comparison(spatial, summary)
    temporal_plots <- plot_human_event_temporal_split_comparison(
      data_all_available = temporal,
      data_event_extension = younger,
      data_matched = matched
    )

    testthat::expect_s3_class(spatial_plot, "patchwork")
    testthat::expect_true(all(
      spatial_plot[[1]][["data"]][["cohort"]] == "as_coded"
    ))
    testthat::expect_true(all(
      spatial_plot[[2]][["data"]][["cohort"]] == "as_coded"
    ))
    testthat::expect_identical(
      names(temporal_plots),
      c("profiles", "changes")
    )
    testthat::expect_true(all(purrr::map_lgl(
      temporal_plots,
      inherits,
      what = "ggplot"
    )))
    testthat::expect_true(
      "Combined" %in%
        spatial_plot[[1]][["scales"]]$get_scales("shape")[["labels"]]
    )
    testthat::expect_true(
      "Combined" %in%
        temporal_plots[["profiles"]][["scales"]]$
          get_scales("colour")[["labels"]]
    )
    testthat::expect_true(
      "Combined minus SPD" %in%
        temporal_plots[["changes"]][["scales"]]$
          get_scales("shape")[["labels"]]
    )
    testthat::expect_identical(
      levels(temporal_plots[["profiles"]][["data"]][["region"]]),
      regions
    )
    testthat::expect_true(any(
      temporal_plots[["profiles"]][["data"]][["age"]] == 500
    ))
    testthat::expect_identical(
      temporal_plots[["profiles"]][["coordinates"]][["limits"]][["y"]],
      c(-0.18, 1)
    )
    testthat::expect_identical(
      temporal_plots[["changes"]][["coordinates"]][["limits"]][["y"]],
      c(-0.32, 0.2)
    )
    profile_arrow_y <-
      temporal_plots[["profiles"]][["layers"]][[6]][["data"]][["y"]]
    change_arrow_y <-
      temporal_plots[["changes"]][["layers"]][[4]][["data"]][["y"]]
    testthat::expect_gte(
      min(profile_arrow_y) -
        temporal_plots[["profiles"]][["coordinates"]][["limits"]][["y"]][[1]],
      0.05
    )
    testthat::expect_gte(
      min(change_arrow_y) -
        temporal_plots[["changes"]][["coordinates"]][["limits"]][["y"]][[1]],
      0.05
    )
  }
)
