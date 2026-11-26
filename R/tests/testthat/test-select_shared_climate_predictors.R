testthat::test_that(
  "select_shared_climate_predictors() selects climate once per unit",
  {
    set.seed(900723)

    vec_a <-
      stats::rnorm(12)

    data_unit_one <-
      tibble::tibble(
        a = vec_a,
        b = vec_a + stats::rnorm(12, sd = 0.01),
        c = stats::rnorm(12),
        human = stats::rnorm(12)
      )

    data_unit_two <-
      tibble::tibble(
        a = stats::rnorm(12),
        b = stats::rnorm(12),
        c = 1,
        human = stats::rnorm(12)
      )

    data_units <-
      tibble::tibble(
        window_id = "w",
        dataset_id = c("1", "2"),
        data_merge = list(data_unit_one, data_unit_two)
      )

    res <-
      select_shared_climate_predictors(
        data_units = data_units,
        unit_columns = c("window_id", "dataset_id"),
        climate_candidates = c("a", "b", "c")
      )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_named(
      res,
      c(
        "window_id", "dataset_id", "climate_status", "climate_predictors",
        "climate_set", "selection_error", "selection_audit"
      )
    )
    testthat::expect_equal(base::nrow(res), 2L)

    res_direct <-
      select_local_hvarpart_predictor_group(
        data_source = data_unit_one,
        candidates = c("a", "b", "c"),
        preference = c("a", "b", "c"),
        group_name = "climate"
      )

    testthat::expect_equal(
      dplyr::pull(res, "climate_predictors")[[1]],
      res_direct[["selected"]]
    )
    testthat::expect_equal(dplyr::pull(res, "climate_set")[1], "a + c")
    testthat::expect_false(
      "c" %in% dplyr::pull(res, "climate_predictors")[[2]]
    )
    testthat::expect_equal(
      dplyr::pull(res, "climate_status"),
      c("eligible_for_design_check", "eligible_for_design_check")
    )
  }
)

testthat::test_that(
  "select_shared_climate_predictors() reports units without climate",
  {
    data_units <-
      tibble::tibble(
        dataset_id = "1",
        data_merge = list(tibble::tibble(a = c(1, 1, 1), b = c(2, 2, 2)))
      )

    res <-
      select_shared_climate_predictors(
        data_units = data_units,
        unit_columns = "dataset_id",
        climate_candidates = c("a", "b")
      )

    testthat::expect_equal(
      dplyr::pull(res, "climate_status"),
      "missing_climate_predictor"
    )
    testthat::expect_true(base::is.na(dplyr::pull(res, "climate_set")))
    testthat::expect_length(dplyr::pull(res, "climate_predictors")[[1]], 0L)
  }
)

testthat::test_that(
  "select_shared_climate_predictors() validates its inputs",
  {
    data_units <-
      tibble::tibble(
        dataset_id = c("1", "1"),
        data_merge = list(
          tibble::tibble(a = 1:3),
          tibble::tibble(a = 1:3)
        )
      )

    testthat::expect_error(
      select_shared_climate_predictors(
        data_units = data_units,
        unit_columns = "dataset_id",
        climate_candidates = "a"
      ),
      regexp = "climate-selection inputs"
    )

    testthat::expect_error(
      select_shared_climate_predictors(
        data_units = dplyr::slice(data_units, 1),
        unit_columns = "region",
        climate_candidates = "a"
      ),
      regexp = "climate-selection inputs"
    )
  }
)
