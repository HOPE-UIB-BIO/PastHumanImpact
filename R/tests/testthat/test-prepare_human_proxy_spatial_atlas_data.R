testthat::test_that(
  "prepare_human_proxy_spatial_atlas_data() preserves matched keys and scales",
  {
    nested <- tibble::tibble(
      dataset_id = c("a", "b"),
      data_merge = list(
        tibble::tibble(
          age = c(2000, 2500),
          spd_sqrt = c(0.2, 0.4),
          kk10_fraction = c(0.1, 0.2),
          hyde_sqrt = c(2, 4)
        ),
        tibble::tibble(
          age = c(2000, 2500),
          spd_sqrt = c(0.3, 0.5),
          kk10_fraction = c(0.15, 0.25),
          hyde_sqrt = c(3, 5)
        )
      )
    )
    metadata <- tibble::tibble(
      dataset_id = c("a", "b"),
      long = c(10, 20), lat = c(50, 60),
      region = c("Europe", "Asia")
    )

    result <- prepare_human_proxy_spatial_atlas_data(
      nested, metadata, age_min = 2000, age_max = 2500,
      colour_quantile = 1
    )

    testthat::expect_equal(nrow(result$values), 12L)
    testthat::expect_equal(nrow(result$scales), 3L)
    testthat::expect_equal(nrow(result$coverage), 6L)
    testthat::expect_setequal(
      as.character(result$values$proxy),
      c("spd_sqrt", "kk10_fraction", "hyde_sqrt")
    )
    testthat::expect_true(all(!result$values$colour_capped))
    testthat::expect_equal(
      unique(result$values$long[result$values$dataset_id == "a"]),
      10
    )

    testthat::expect_error(
      prepare_human_proxy_spatial_atlas_data(
        nested,
        dplyr::bind_rows(metadata, metadata[1, ])
      ),
      "one row per `dataset_id`"
    )

    duplicate_nested <- nested
    duplicate_nested$data_merge[[1]] <- dplyr::bind_rows(
      duplicate_nested$data_merge[[1]],
      duplicate_nested$data_merge[[1]][1, ]
    )
    testthat::expect_error(
      prepare_human_proxy_spatial_atlas_data(
        duplicate_nested, metadata
      ),
      "duplicate `dataset_id`-`age` keys"
    )
  }
)
