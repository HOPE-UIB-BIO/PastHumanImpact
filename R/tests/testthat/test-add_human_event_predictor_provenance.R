testthat::test_that(
  "add_human_event_predictor_provenance() records regional predictors",
  {
    source <- tibble::tibble(
      region = c("Europe", "All"),
      age = c(2000, NA_real_)
    )

    result <- add_human_event_predictor_provenance(
      data_source = source,
      data_meta = tibble::tibble(dataset_id = character(), region = character()),
      proxy_variant = "spd_events"
    )

    testthat::expect_identical(
      result[["retained_human_predictors"]][[1]],
      "spd;fi;fc;ec;cc"
    )
    testthat::expect_identical(
      result[["reference_category"]][[1]],
      "bi"
    )
    testthat::expect_match(
      result[["retained_human_predictors"]][[2]],
      "regional_model_audit"
    )
  }
)
