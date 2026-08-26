testthat::test_that(
  "plot_spatial_moran_profile() builds connected diagnostic panels",
  {
    data_moran <-
      tidyr::crossing(
        profile = c("signed", "zero_truncated"),
        stage = c("unfiltered", "residual"),
        spatial_group = c("All", "Europe")
      ) |>
      dplyr::mutate(
        spatial_scope = dplyr::if_else(
          .data[["spatial_group"]] == "All",
          "global",
          "dbmem_connectivity"
        ),
        distance_km = dplyr::if_else(
          .data[["spatial_group"]] == "All",
          250,
          411
        ),
        moran_i = seq(0.01, 0.08, length.out = dplyr::n()),
        positive_autocorrelation = .data[["stage"]] == "unfiltered"
      )

    res <-
      plot_spatial_moran_profile(
        data_moran = data_moran,
        profile = "signed"
      )

    testthat::expect_s3_class(res, "ggplot")
    testthat::expect_equal(nrow(res[["data"]]), 4L)
    testthat::expect_equal(length(res[["layers"]]), 3L)
  }
)

testthat::test_that(
  "plot_spatial_moran_profile() validates inputs and empty profiles",
  {
    testthat::expect_error(
      plot_spatial_moran_profile(
        data_moran = tibble::tibble(),
        profile = "signed"
      ),
      "do not satisfy"
    )

    data_moran <-
      tibble::tibble(
        profile = "signed",
        stage = "unfiltered",
        spatial_scope = "global",
        spatial_group = "All",
        distance_km = 250,
        moran_i = 0.1,
        positive_autocorrelation = TRUE
      )

    testthat::expect_error(
      plot_spatial_moran_profile(
        data_moran = data_moran,
        profile = "zero_truncated"
      ),
      "No Moran diagnostics"
    )
  }
)
