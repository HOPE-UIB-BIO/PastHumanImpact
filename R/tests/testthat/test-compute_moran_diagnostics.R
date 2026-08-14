testthat::test_that(
  "compute_moran_diagnostics() detects positive spatial pattern",
  {
    data_input <-
      tibble::tibble(
        dataset_id = 1:10,
        long = seq(0, 0.9, by = 0.1),
        lat = 0,
        region = rep(c("a", "b"), each = 5),
        value = seq_len(10)
      )

    result <-
      compute_moran_diagnostics(
        data_source = data_input,
        value_cols = "value",
        distance_km = 25,
        block_col = "region",
        permutations = 99L,
        seed = 900723L
      )

    testthat::expect_equal(nrow(result), 1L)
    testthat::expect_equal(
      dplyr::pull(result, "status"),
      "estimated"
    )
    testthat::expect_gt(dplyr::pull(result, "moran_i"), 0)
    testthat::expect_equal(dplyr::pull(result, "permutations"), 99L)
  }
)

testthat::test_that(
  "compute_moran_diagnostics() reports an unlinked distance graph",
  {
    data_input <-
      tibble::tibble(
        dataset_id = 1:3,
        long = c(0, 10, 20),
        lat = 0,
        value = c(1, 2, 3)
      )

    result <-
      compute_moran_diagnostics(
        data_source = data_input,
        value_cols = "value",
        distance_km = 1,
        permutations = 9L
      )

    testthat::expect_equal(
      dplyr::pull(result, "status"),
      "no_links"
    )
    testthat::expect_true(is.na(dplyr::pull(result, "moran_i")))
  }
)

testthat::test_that(
  "compute_moran_diagnostics() links co-located distinct records",
  {
    data_input <-
      tibble::tibble(
        dataset_id = 1:3,
        long = c(0, 0, 10),
        lat = c(0, 0, 0),
        value = c(1, 1, 3)
      )

    result <-
      compute_moran_diagnostics(
        data_source = data_input,
        value_cols = "value",
        distance_km = 1,
        permutations = 9L
      )

    testthat::expect_equal(dplyr::pull(result, "n_edges"), 1)
    testthat::expect_equal(dplyr::pull(result, "n_connected"), 2L)
  }
)
