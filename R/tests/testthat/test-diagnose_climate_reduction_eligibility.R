testthat::test_that(
  "diagnose_climate_reduction_eligibility() compares climate sets",
  {
    set.seed(900723)

    data_record <-
      tibble::tibble(
        age = base::seq(2000, 7500, by = 500),
        r1 = stats::rnorm(12),
        r2 = stats::rnorm(12),
        spd = stats::rnorm(12),
        a = stats::rnorm(12),
        b = stats::rnorm(12),
        c = stats::rnorm(12),
        d = stats::rnorm(12)
      )

    data_designs <-
      tibble::tibble(
        dataset_id = c("1", "2"),
        model_id = "spd_run",
        selection_status = c(
          "eligible_for_design_check",
          "missing_human_predictor"
        ),
        data_merge = list(data_record, data_record),
        predictor_vars = list(
          list(human = "spd", climate = c("a", "c")),
          list(human = character(), climate = c("a", "c"))
        )
      )

    res <-
      diagnose_climate_reduction_eligibility(
        data_designs = data_designs,
        response_vars = c("r1", "r2"),
        climate_candidates = c("a", "b", "c", "d"),
        min_unique_ages = 10L,
        min_residual_df = 6L
      )

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_named(
      res,
      c(
        "dataset_id", "model_id", "climate_set", "n_climate", "status",
        "residual_df", "n_unique_ages"
      )
    )
    testthat::expect_equal(base::nrow(res), 4L)

    data_record_one <-
      res |>
      dplyr::filter(.data[["dataset_id"]] == "1")

    testthat::expect_equal(
      dplyr::pull(data_record_one, "climate_set"),
      c("selected", "all_candidates")
    )
    testthat::expect_equal(dplyr::pull(data_record_one, "n_climate"), c(2L, 4L))
    testthat::expect_equal(
      dplyr::pull(data_record_one, "residual_df"),
      c(7L, 5L)
    )
    testthat::expect_equal(
      dplyr::pull(data_record_one, "status"),
      c("estimable", "insufficient_residual_df")
    )

    data_record_two <-
      res |>
      dplyr::filter(.data[["dataset_id"]] == "2")

    testthat::expect_equal(
      dplyr::pull(data_record_two, "status"),
      c("missing_human_predictor", "missing_human_predictor")
    )
    testthat::expect_true(
      base::all(base::is.na(dplyr::pull(data_record_two, "residual_df")))
    )
  }
)

testthat::test_that(
  "diagnose_climate_reduction_eligibility() flags too few ages",
  {
    data_designs <-
      tibble::tibble(
        dataset_id = "1",
        model_id = "spd_run",
        selection_status = "eligible_for_design_check",
        data_merge = list(
          tibble::tibble(
            age = c(2000, 2500, 3000),
            r1 = c(1, 2, 3),
            spd = c(1, 3, 2),
            a = c(2, 1, 3)
          )
        ),
        predictor_vars = list(list(human = "spd", climate = "a"))
      )

    res <-
      diagnose_climate_reduction_eligibility(
        data_designs = data_designs,
        response_vars = "r1",
        climate_candidates = "a"
      )

    testthat::expect_equal(
      dplyr::pull(res, "status"),
      c("insufficient_unique_ages", "insufficient_unique_ages")
    )
  }
)

testthat::test_that(
  "diagnose_climate_reduction_eligibility() validates its inputs",
  {
    testthat::expect_error(
      diagnose_climate_reduction_eligibility(
        data_designs = tibble::tibble(dataset_id = "1"),
        response_vars = "r1"
      ),
      regexp = "Climate-reduction eligibility inputs"
    )
  }
)
