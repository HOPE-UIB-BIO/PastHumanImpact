testthat::test_that(
  "plot_human_proxy_spatial_atlas_page() draws all proxy rows",
  {
    proxy_levels <- c("spd_sqrt", "kk10_fraction", "hyde_sqrt")
    proxy_labels <- c(
      "sqrt(SPD)", "KK10 land-use fraction", "sqrt(HYDE population)"
    )
    values <- tidyr::crossing(
      dataset_id = c("a", "b"),
      age = 2000,
      proxy = factor(proxy_levels, levels = proxy_levels)
    ) |>
      dplyr::mutate(
        proxy_label = factor(
          proxy_labels[match(as.character(.data[["proxy"]]), proxy_levels)],
          levels = proxy_labels
        ),
        value = rep(c(0.2, 0.4), each = 3),
        colour_value = .data[["value"]],
        colour_max = 1,
        long = ifelse(.data[["dataset_id"]] == "a", 10, 20),
        lat = ifelse(.data[["dataset_id"]] == "a", 50, 60),
        region = ifelse(.data[["dataset_id"]] == "a", "Europe", "Asia")
      )
    world <- tibble::tibble(
      long = c(-180, 180, 180, -180),
      lat = c(-60, -60, 85, 85),
      group = 1L
    )

    result <- plot_human_proxy_spatial_atlas_page(
      values, age = 2000, world_map = world
    )
    testthat::expect_s3_class(result, "ggplot")

    testthat::expect_error(
      plot_human_proxy_spatial_atlas_page(
        values, age = 2500, world_map = world
      ),
      "must contain all three"
    )
  }
)
