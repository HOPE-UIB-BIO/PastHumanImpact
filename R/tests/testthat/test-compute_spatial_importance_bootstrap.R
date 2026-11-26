testthat::test_that(
  "compute_spatial_importance_bootstrap() brackets the observed estimate",
  {
    set.seed(900723)

    n_records <- 30L

    data_records <-
      tibble::tibble(
        model_id = stringr::str_c("model_", base::seq_len(n_records)),
        long = base::seq(0, 2.9, by = 0.1),
        lat = 45,
        region = base::rep(c("A", "B"), each = 15),
        climatezone = base::rep(c("cold", "warm", "cold"), each = 10),
        signed_difference = stats::rnorm(n_records, mean = -0.05, sd = 0.1),
        signed_weight = stats::runif(n_records, min = 0.2, max = 0.8),
        zero_balance = stats::runif(n_records, min = -1, max = 0.6),
        zero_weight = stats::runif(n_records, min = 0.1, max = 0.5)
      )

    res_fit <-
      fit_spatial_importance(
        data_records = data_records,
        permutations = 19L,
        min_unique_locations = 10L,
        min_residual_df = 5L,
        distance_km = 50,
        seed = 900723L
      )

    res <-
      compute_spatial_importance_bootstrap(
        data_records = data_records,
        result_spatial_importance = res_fit,
        profile = "zero_truncated",
        n_bootstrap = 199L,
        seed = 900723L
      )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_named(
      res,
      c(
        "profile", "adjusted_balance", "lwr_95", "upr_95", "n_records",
        "n_bootstrap", "n_failed_replicates"
      )
    )
    testthat::expect_equal(base::nrow(res), 1L)

    value_observed <-
      res_fit[["estimates"]] |>
      dplyr::filter(
        .data[["aggregation_level"]] == "overall",
        .data[["profile"]] == "zero_truncated"
      ) |>
      dplyr::pull("adjusted_balance")

    testthat::expect_equal(
      dplyr::pull(res, "adjusted_balance"),
      value_observed,
      tolerance = 1e-10
    )
    testthat::expect_true(
      dplyr::pull(res, "lwr_95") < dplyr::pull(res, "upr_95")
    )
    testthat::expect_true(
      dplyr::pull(res, "lwr_95") <= value_observed &&
        value_observed <= dplyr::pull(res, "upr_95")
    )
    testthat::expect_equal(dplyr::pull(res, "n_records"), 30L)
    testthat::expect_equal(dplyr::pull(res, "n_failed_replicates"), 0L)
  }
)

testthat::test_that(
  "compute_spatial_importance_bootstrap() is reproducible and local",
  {
    set.seed(900723)

    n_records <- 24L

    data_records <-
      tibble::tibble(
        model_id = stringr::str_c("model_", base::seq_len(n_records)),
        long = base::seq(0, 2.3, by = 0.1),
        lat = 45,
        region = "A",
        climatezone = base::rep(c("cold", "warm"), each = 12),
        signed_difference = stats::rnorm(n_records, sd = 0.1),
        signed_weight = stats::runif(n_records, min = 0.2, max = 0.8),
        zero_balance = stats::runif(n_records, min = -1, max = 1),
        zero_weight = stats::runif(n_records, min = 0.1, max = 0.5)
      )

    res_fit <-
      fit_spatial_importance(
        data_records = data_records,
        permutations = 19L,
        min_unique_locations = 10L,
        min_residual_df = 5L,
        distance_km = 50,
        seed = 900723L
      )

    set.seed(1)

    value_before <-
      stats::runif(1)

    set.seed(1)

    res_first <-
      compute_spatial_importance_bootstrap(
        data_records = data_records,
        result_spatial_importance = res_fit,
        profile = "signed",
        n_bootstrap = 99L,
        seed = 42L
      )

    value_after <-
      stats::runif(1)

    res_second <-
      compute_spatial_importance_bootstrap(
        data_records = data_records,
        result_spatial_importance = res_fit,
        profile = "signed",
        n_bootstrap = 99L,
        seed = 42L
      )

    testthat::expect_equal(res_first, res_second)
    testthat::expect_equal(value_before, value_after)
    testthat::expect_equal(dplyr::pull(res_first, "profile"), "signed")
  }
)

testthat::test_that(
  "compute_spatial_importance_bootstrap() validates its inputs",
  {
    data_records <-
      tibble::tibble(
        region = "A",
        climatezone = "cold",
        zero_balance = 0.1,
        zero_weight = 0.2
      )

    testthat::expect_error(
      compute_spatial_importance_bootstrap(
        data_records = data_records,
        result_spatial_importance = list(),
        profile = "zero_truncated"
      ),
      regexp = "Spatial bootstrap inputs"
    )

    testthat::expect_error(
      compute_spatial_importance_bootstrap(
        data_records = data_records,
        result_spatial_importance = list(),
        profile = "pure"
      )
    )
  }
)
