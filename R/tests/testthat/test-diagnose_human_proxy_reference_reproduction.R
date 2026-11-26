testthat::test_that(
  "diagnose_human_proxy_reference_reproduction() classifies records",
  {
    data_reference <-
      tibble::tibble(
        dataset_id = c("1", "2", "3", "4", "5"),
        human_contribution = c(0.2, 0.1, 0.1, 0.1, 0.1),
        climate_contribution = c(0.1, 0.3, 0.2, 0.2, 0.2),
        control_contribution = 0.1,
        zero_balance = c(1 / 3, -0.5, -1 / 3, -1 / 3, -1 / 3),
        signed_difference = c(0.1, -0.2, -0.1, -0.1, -0.1),
        dominance = c("Human", "Climate", "Climate", "Climate", "Climate")
      )

    data_reference_status <-
      tibble::tibble(
        dataset_id = c("1", "2", "3", "4", "5"),
        residual_df = c(6, 7, 4, 6, 6)
      )

    data_records <-
      tibble::tibble(
        dataset_id = c("1", "2"),
        human = c(0.2, 0.101),
        climate = c(0.1, 0.3),
        time = 0.1,
        zero_balance = c(1 / 3, -0.5),
        signed_difference = c(0.1, -0.2)
      )

    data_status <-
      tibble::tibble(
        dataset_id = c("1", "2", "3", "5"),
        status = c(
          "estimated", "estimated", "insufficient_residual_df",
          "model_error"
        ),
        residual_df = c(6, 7, 4, 6)
      )

    res <-
      diagnose_human_proxy_reference_reproduction(
        data_records = data_records,
        data_status = data_status,
        data_reference = data_reference,
        data_reference_status = data_reference_status,
        data_cohort = tibble::tibble(dataset_id = c("1", "2", "3", "5")),
        min_residual_df = 5L,
        tolerance = 1e-8
      )

    testthat::expect_named(res, c("records", "summary"))
    testthat::expect_equal(
      dplyr::pull(res[["records"]], "outcome"),
      c(
        "reproduced", "differs", "excluded_residual_df",
        "not_in_common_cohort", "unexplained"
      )
    )
    testthat::expect_equal(
      dplyr::pull(res[["records"]], "new_dominance"),
      c("Human", "Climate", NA, NA, NA)
    )

    data_summary <-
      res[["summary"]]

    testthat::expect_equal(base::nrow(data_summary), 1L)
    testthat::expect_equal(dplyr::pull(data_summary, "n_reference"), 5L)
    testthat::expect_equal(dplyr::pull(data_summary, "n_reference_human"), 1L)
    testthat::expect_equal(dplyr::pull(data_summary, "n_compared"), 2L)
    testthat::expect_equal(dplyr::pull(data_summary, "n_reproduced"), 1L)
    testthat::expect_equal(dplyr::pull(data_summary, "n_differs"), 1L)
    testthat::expect_equal(dplyr::pull(data_summary, "n_unexplained"), 1L)
    testthat::expect_equal(dplyr::pull(data_summary, "n_human_new"), 1L)
    testthat::expect_equal(
      dplyr::pull(data_summary, "max_abs_difference"),
      0.001,
      tolerance = 1e-10
    )
    testthat::expect_false(dplyr::pull(data_summary, "is_reproduced"))
  }
)

testthat::test_that(
  "diagnose_human_proxy_reference_reproduction() names balanceless fits",
  {
    data_reference <-
      tibble::tibble(
        dataset_id = c("1", "2"),
        human_contribution = c(0.2, 0.1),
        climate_contribution = c(0.1, 0.3),
        control_contribution = 0.1,
        zero_balance = c(1 / 3, -0.5),
        signed_difference = c(0.1, -0.2),
        dominance = c("Human", "Climate")
      )

    res <-
      diagnose_human_proxy_reference_reproduction(
        data_records = tibble::tibble(
          dataset_id = "1",
          human = 0.2,
          climate = 0.1,
          time = 0.1,
          zero_balance = 1 / 3,
          signed_difference = 0.1
        ),
        data_status = tibble::tibble(
          dataset_id = c("1", "2"),
          status = c("estimated", "estimated_residual_temporal_dependence"),
          residual_df = c(6, 7)
        ),
        data_reference = data_reference,
        data_reference_status = tibble::tibble(
          dataset_id = c("1", "2"),
          residual_df = c(6, 7)
        ),
        data_cohort = tibble::tibble(dataset_id = c("1", "2"))
      )

    testthat::expect_equal(
      dplyr::pull(res[["records"]], "outcome"),
      c("reproduced", "estimated_without_balance")
    )
    testthat::expect_equal(
      dplyr::pull(res[["summary"]], "n_estimated_without_balance"),
      1L
    )
    testthat::expect_false(dplyr::pull(res[["summary"]], "is_reproduced"))
  }
)

testthat::test_that(
  "diagnose_human_proxy_reference_reproduction() confirms reproduction",
  {
    data_reference <-
      tibble::tibble(
        dataset_id = "1",
        human_contribution = 0.2,
        climate_contribution = 0.1,
        control_contribution = 0.1,
        zero_balance = 1 / 3,
        signed_difference = 0.1,
        dominance = "Human"
      )

    res <-
      diagnose_human_proxy_reference_reproduction(
        data_records = tibble::tibble(
          dataset_id = "1",
          human = 0.2,
          climate = 0.1,
          time = 0.1,
          zero_balance = 1 / 3,
          signed_difference = 0.1
        ),
        data_status = tibble::tibble(
          dataset_id = "1",
          status = "estimated",
          residual_df = 6
        ),
        data_reference = data_reference,
        data_reference_status = tibble::tibble(
          dataset_id = "1",
          residual_df = 6
        ),
        data_cohort = tibble::tibble(dataset_id = "1")
      )

    testthat::expect_true(dplyr::pull(res[["summary"]], "is_reproduced"))

    res_empty <-
      diagnose_human_proxy_reference_reproduction(
        data_records = tibble::tibble(
          dataset_id = character(),
          human = numeric(),
          climate = numeric(),
          time = numeric(),
          zero_balance = numeric(),
          signed_difference = numeric()
        ),
        data_status = tibble::tibble(
          dataset_id = character(),
          status = character(),
          residual_df = numeric()
        ),
        data_reference = data_reference,
        data_reference_status = tibble::tibble(
          dataset_id = "1",
          residual_df = 6
        ),
        data_cohort = tibble::tibble(dataset_id = "2")
      )

    testthat::expect_false(dplyr::pull(res_empty[["summary"]], "is_reproduced"))
  }
)

testthat::test_that(
  "diagnose_human_proxy_reference_reproduction() validates its inputs",
  {
    testthat::expect_error(
      diagnose_human_proxy_reference_reproduction(
        data_records = tibble::tibble(dataset_id = "1"),
        data_status = tibble::tibble(dataset_id = "1"),
        data_reference = tibble::tibble(dataset_id = "1"),
        data_reference_status = tibble::tibble(dataset_id = "1"),
        data_cohort = tibble::tibble(dataset_id = "1")
      ),
      regexp = "Reproduction inputs"
    )
  }
)
