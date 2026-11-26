testthat::test_that(
  "compute_temporal_allocation_bootstrap() returns ordered intervals",
  {
    set.seed(900723)

    data_units <-
      tidyr::expand_grid(
        region = c("A", "B"),
        age = base::seq(2000, 8000, by = 500)
      ) |>
      dplyr::mutate(
        human = stats::runif(dplyr::n(), min = 0, max = 0.2),
        climate = stats::runif(dplyr::n(), min = 0.1, max = 0.4),
        space = stats::runif(dplyr::n(), min = 0, max = 0.2)
      )

    res <-
      compute_temporal_allocation_bootstrap(
        data_units = data_units,
        profile = c("zero_truncated", "signed"),
        block_length = 3L,
        n_bootstrap = 199L,
        seed = 900723L
      )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_named(
      res,
      c("profile", "lwr_95", "upr_95", "n_bootstrap", "n_failed_replicates")
    )
    testthat::expect_equal(
      dplyr::pull(res, "profile"),
      c("zero_truncated", "signed")
    )
    testthat::expect_true(
      base::all(dplyr::pull(res, "lwr_95") < dplyr::pull(res, "upr_95"))
    )

    value_observed <-
      compute_pooled_human_climate_allocation(
        data_units = data_units,
        profile = "zero_truncated"
      ) |>
      dplyr::pull("pooled_difference")

    testthat::expect_true(
      dplyr::pull(res, "lwr_95")[1] <= value_observed &&
        value_observed <= dplyr::pull(res, "upr_95")[1]
    )
    testthat::expect_equal(dplyr::pull(res, "n_failed_replicates"), c(0L, 0L))
  }
)

testthat::test_that(
  "compute_temporal_allocation_bootstrap() is reproducible and local",
  {
    data_units <-
      tibble::tibble(
        region = "A",
        age = base::seq(2000, 4000, by = 500),
        human = c(0.1, 0.2, 0.05, 0.3, 0.1),
        climate = c(0.2, 0.1, 0.3, 0.1, 0.2),
        space = 0.05
      )

    set.seed(1)

    value_before <-
      stats::runif(1)

    set.seed(1)

    res_first <-
      compute_temporal_allocation_bootstrap(
        data_units = data_units,
        profile = "signed",
        n_bootstrap = 99L,
        seed = 42L
      )

    value_after <-
      stats::runif(1)

    res_second <-
      compute_temporal_allocation_bootstrap(
        data_units = data_units,
        profile = "signed",
        n_bootstrap = 99L,
        seed = 42L
      )

    testthat::expect_s3_class(res_first, "tbl_df")
    testthat::expect_equal(res_first, res_second)
    testthat::expect_equal(value_before, value_after)
  }
)

testthat::test_that(
  "compute_temporal_allocation_bootstrap() keeps one unit per region fixed",
  {
    data_units <-
      tibble::tibble(
        region = c("A", "B"),
        age = 2000,
        human = c(0.1, 0.3),
        climate = c(0.3, 0.1),
        space = 0
      )

    res <-
      compute_temporal_allocation_bootstrap(
        data_units = data_units,
        profile = "zero_truncated",
        n_bootstrap = 19L,
        seed = 1L
      )

    testthat::expect_equal(dplyr::pull(res, "lwr_95"), 0)
    testthat::expect_equal(dplyr::pull(res, "upr_95"), 0)
  }
)

testthat::test_that(
  "compute_temporal_allocation_bootstrap() validates its inputs",
  {
    testthat::expect_error(
      compute_temporal_allocation_bootstrap(
        data_units = tibble::tibble(region = "A"),
        profile = "signed"
      ),
      regexp = "Temporal bootstrap inputs"
    )

    testthat::expect_error(
      compute_temporal_allocation_bootstrap(
        data_units = tibble::tibble(
          region = "A",
          age = 2000,
          human = 0.1,
          climate = 0.1,
          space = 0
        ),
        profile = "signed",
        block_length = 0L
      ),
      regexp = "Temporal bootstrap inputs"
    )
  }
)
