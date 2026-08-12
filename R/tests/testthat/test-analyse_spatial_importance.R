testthat::test_that(
  "analyse_spatial_importance() returns adjusted estimates and diagnostics",
  {
    set.seed(900723)
    n_records <- 30L
    data_records <-
      tibble::tibble(
        model_id = stringr::str_c("model_", seq_len(n_records)),
        long = seq(0, 2.9, by = 0.1),
        lat = 45,
        region = rep(c("A", "B"), each = 15),
        climatezone = rep(c("cold", "warm", "cold"), each = 10),
        signed_balance = scale(seq_len(n_records))[, 1],
        signed_weight = 1,
        zero_balance = stats::plogis(scale(seq_len(n_records))[, 1]) *
          2 - 1,
        zero_weight = 1
      )

    result <-
      analyse_spatial_importance(
        data_records = data_records,
        permutations = 19L,
        min_unique_locations = 10L,
        min_residual_df = 5L,
        distance_km = 50,
        seed = 900723L
      )

    testthat::expect_type(result, "list")
    testthat::expect_true(
      all(c("estimates", "moran_diagnostics", "selection") %in%
        names(result))
    )
    testthat::expect_true(
      all(c("signed", "zero_truncated") %in%
        dplyr::pull(result[["estimates"]], "profile"))
    )
    testthat::expect_true(
      all(c("baseline", "residual") %in%
        dplyr::pull(result[["moran_diagnostics"]], "stage"))
    )
  }
)

testthat::test_that(
  "analyse_spatial_importance() validates required columns",
  {
    testthat::expect_error(
      analyse_spatial_importance(
        data_records = tibble::tibble(model_id = "one")
      ),
      "required contract"
    )
  }
)
