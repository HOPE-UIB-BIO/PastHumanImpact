testthat::test_that(
  "plot_h1_temporal_controlled_composition() pairs proxy stacks",
  {
    data_input <-
      tidyr::crossing(
        analysis = c("temporal_spd", "temporal_events"),
        region = "Europe",
        age = c(2000, 2500),
        predictor = c("human", "climate", "space")
      ) |>
      dplyr::mutate(
        allocation = dplyr::case_when(
          .data[["predictor"]] == "human" ~ 0.5,
          .data[["predictor"]] == "climate" ~ 0.3,
          .default = 0.2
        )
      )
    result <- plot_h1_temporal_controlled_composition(data_input)
    built <- ggplot2::ggplot_build(result)

    testthat::expect_s3_class(result, "ggplot")
    testthat::expect_equal(result[["labels"]][["fill"]], "Driver")
    testthat::expect_equal(
      result[["labels"]][["y"]],
      paste0(
        "Relative importance\n",
        "(Zero-truncated hierarchical contribution)"
      )
    )
    testthat::expect_true(
      all(is.na(built[["data"]][[2]][["alpha"]]))
    )
    testthat::expect_setequal(
      unique(built[["data"]][[3]][["linetype"]]),
      c("solid", "dotted")
    )
    testthat::expect_equal(
      result[["scales"]][["get_scales"]]("y")[["position"]],
      "right"
    )
    testthat::expect_equal(
      result[["scales"]][["get_scales"]]("y")[["breaks"]],
      c(0.25, 0.5, 0.75)
    )
    testthat::expect_equal(
      result[["scales"]][["get_scales"]]("fill")[["breaks"]],
      c("human_spd", "human_events", "climate", "space")
    )
    testthat::expect_equal(
      names(result[["facet"]][["params"]][["cols"]]),
      "age_facet"
    )
    testthat::expect_setequal(
      unique(as.character(built[["layout"]][["layout"]][["age_facet"]])),
      c("2.5", "2.0")
    )
    testthat::expect_equal(
      levels(result[["data"]][["region"]]),
      c(
        "North America",
        "Latin America",
        "Europe",
        "Asia",
        "Oceania"
      )
    )
    testthat::expect_s3_class(
      result[["theme"]][["panel.grid"]],
      "element_blank"
    )
    testthat::expect_s3_class(
      result[["theme"]][["panel.border"]],
      "element_blank"
    )
  }
)

testthat::test_that(
  "plot_h1_temporal_controlled_composition() validates colours",
  {
    testthat::expect_error(
      plot_h1_temporal_controlled_composition(
        data_stack = tibble::tibble(),
        event_human_colour = 2
      ),
      "do not satisfy"
    )
  }
)
