testthat::test_that(
  "prepare_human_event_sensitivity_inputs() preserves availability cohorts",
  {
    regions <- c(
      "Asia",
      "Europe",
      "North America",
      "Latin America",
      "Oceania"
    )
    predictors <-
      tibble::tibble(
        dataset_id = letters[1:5],
        data_merge = rep(list(tibble::tibble(age = 2000)), 5L)
      )
    events <-
      tibble::tibble(
        dataset_id = letters[1:5],
        have_events = c(TRUE, TRUE, FALSE, TRUE, FALSE)
      )
    metadata <-
      tibble::tibble(dataset_id = letters[1:5], region = regions)

    result <- prepare_human_event_sensitivity_inputs(
      data_predictors = predictors,
      events_temporal_subset = events,
      data_meta = metadata
    )

    testthat::expect_equal(
      nrow(result[["cohorts"]][["data_predictors"]][[1]]),
      5L
    )
    testthat::expect_equal(
      nrow(result[["cohorts"]][["data_predictors"]][[2]]),
      3L
    )
    testthat::expect_equal(nrow(result[["model_audit"]]), 30L)
    testthat::expect_identical(
      result[["availability_summary"]][["n_datasets"]],
      c(2L, 3L, 5L)
    )
  }
)

testthat::test_that(
  "human-event audit never assigns another region's predictors",
  {
    predictors <-
      tibble::tibble(
        dataset_id = "asia",
        data_merge = list(tibble::tibble(age = 2000))
      )
    events <-
      tibble::tibble(dataset_id = "asia", have_events = TRUE)
    metadata <-
      tibble::tibble(dataset_id = "asia", region = "Asia")

    audit <-
      prepare_human_event_sensitivity_inputs(
        data_predictors = predictors,
        events_temporal_subset = events,
        data_meta = metadata
      )[["model_audit"]] |>
      dplyr::filter(.data[["proxy_variant"]] == "events")

    testthat::expect_true(
      all(audit[["retained_human_predictors"]] == "fi;fc;ei")
    )
    testthat::expect_false(
      any(stringr::str_detect(
        audit[["retained_human_predictors"]],
        "ec|cc|es|weak|medium|strong|bi|no_impact"
      ))
    )
  }
)
