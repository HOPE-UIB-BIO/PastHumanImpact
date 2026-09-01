testthat::test_that(
  "prepare_human_event_young_timebins() separates chronology cohorts",
  {
    data_timebins <- tibble::tibble(
      age = c(500, 1500, 2000),
      region = "Europe",
      data_merge = list(
        tibble::tibble(dataset_id = c("a", "b"), value = 1:2),
        tibble::tibble(dataset_id = c("a", "b"), value = 3:4),
        tibble::tibble(dataset_id = c("a", "b"), value = 5:6)
      ),
      n_samples = 2L
    )
    chronologies <- tibble::tibble(
      dataset_id = c("a", "b"),
      have_events = c(TRUE, FALSE)
    )

    all_data <- prepare_human_event_young_timebins(
      data_timebins,
      chronologies,
      cohort = "as_coded"
    )
    nonzero <- prepare_human_event_young_timebins(
      data_timebins,
      chronologies,
      cohort = "observed_events_only"
    )

    testthat::expect_identical(all_data[["age"]], c(500, 1500))
    testthat::expect_true(all(all_data[["n_samples"]] == 2L))
    testthat::expect_true(all(nonzero[["n_samples"]] == 1L))
    testthat::expect_true(all(purrr::map_lgl(
      nonzero[["data_merge"]],
      ~ identical(.x[["dataset_id"]], "a")
    )))
  }
)

testthat::test_that(
  "prepare_human_event_young_timebins() rejects unknown cohorts",
  {
    testthat::expect_error(
      prepare_human_event_young_timebins(
        tibble::tibble(
          age = 500,
          region = "Europe",
          data_merge = list(tibble::tibble(dataset_id = "a"))
        ),
        tibble::tibble(dataset_id = "a", have_events = TRUE),
        cohort = "unknown"
      ),
      "contract"
    )
  }
)
