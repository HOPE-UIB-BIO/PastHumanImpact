testthat::test_that(
  "summarise_spatial_importance_sensitivity() covers all checks",
  {
    data_records <-
      tibble::tibble(
        model_id = stringr::str_c("m", 1:6),
        region = rep(c("A", "B"), each = 3),
        climatezone = rep(c("cold", "warm"), 3),
        signed_balance = c(-0.5, -0.4, -0.3, 0.1, 0.2, 0.3),
        signed_weight = 1,
        zero_balance = c(-1, -0.8, -0.6, 0.2, 0.4, 0.6),
        zero_weight = 1
      )
    data_thinning <-
      tidyr::crossing(
        distance_km = c(250, 500),
        repetition = 1:2,
        model_id = c("m1", "m3", "m4", "m6")
      )

    result <-
      summarise_spatial_importance_sensitivity(
        data_records = data_records,
        data_thinning = data_thinning
      )

    testthat::expect_true(
      all(c(
        "baseline",
        "thinning",
        "leave_region_out",
        "leave_climatezone_out"
      ) %in% dplyr::pull(result, "sensitivity_type"))
    )
    testthat::expect_true(
      all(c("overall", "region", "region_climatezone") %in%
        dplyr::pull(result, "aggregation_level"))
    )
    testthat::expect_true(
      all(is.finite(
        dplyr::pull(
          dplyr::filter(result, .data[["sensitivity_type"]] == "baseline"),
          "absolute_deviation"
        )
      ))
    )
  }
)

testthat::test_that(
  "summarise_spatial_importance_sensitivity() validates its ledger",
  {
    testthat::expect_error(
      summarise_spatial_importance_sensitivity(
        data_records = tibble::tibble(model_id = "m1"),
        data_thinning = tibble::tibble(model_id = "m1")
      ),
      "required contract"
    )
  }
)
