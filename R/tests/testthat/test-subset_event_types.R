testthat::test_that("subset_event_types() builds region-specific events", {
  data_source_meta <-
    data.frame(
      dataset_id = c(1, 2),
      region = c("Asia", "Oceania"),
      age_min = c(0, 0),
      age_max = c(2, 2),
      stringsAsFactors = FALSE
    )

  data_source_dummy_time <-
    data.frame(age = c(0, 1, 2))

  data_source_events <-
    data.frame(
      dataset_id = 1,
      data = I(list(data.frame(
        age = c(0, 1, 2),
        bi = c(0, 0, 1),
        fi = c(1, 0, 0),
        fc = c(0, 0, 0),
        ei = c(0, 1, 0)
      ))),
      stringsAsFactors = FALSE
    )

  res_events <-
    subset_event_types(
      data_source_events = data_source_events,
      data_source_meta = data_source_meta,
      data_source_dummy_time = data_source_dummy_time
    )

  testthat::expect_identical(
    dplyr::pull(res_events, have_events),
    c(TRUE, FALSE)
  )

  testthat::expect_identical(
    purrr::pluck(res_events, "events", 1),
    data.frame(
      age = c(0, 1, 2),
      bi = c(0, 0, 1),
      fi = c(1, 0, 0),
      fc = c(0, 0, 0),
      ei = c(0, 1, 0)
    )
  )

  testthat::expect_identical(
    purrr::pluck(res_events, "events", 2),
    data.frame(
      age = c(0, 1, 2),
      no_impact = c(1, 1, 1),
      weak = c(0, 0, 0),
      medium = c(0, 0, 0),
      strong = c(0, 0, 0)
    )
  )
})

testthat::test_that("subset_event_types() keeps only allowed columns per region", {
  data_source_meta <-
    data.frame(
      dataset_id = 10,
      region = "Europe",
      age_min = 0,
      age_max = 2,
      stringsAsFactors = FALSE
    )

  data_source_dummy_time <-
    data.frame(age = c(0, 1, 2))

  data_source_events <-
    data.frame(
      dataset_id = 10,
      data = I(list(data.frame(
        age = c(0, 1, 2),
        bi = c(1, 0, 0),
        fi = c(0, 1, 0),
        fc = c(0, 0, 1),
        ec = c(0, 0, 1),
        cc = c(1, 0, 0),
        extra = c(9, 9, 9)
      ))),
      stringsAsFactors = FALSE
    )

  res_events <-
    subset_event_types(
      data_source_events = data_source_events,
      data_source_meta = data_source_meta,
      data_source_dummy_time = data_source_dummy_time
    )

  data_europe <-
    dplyr::pull(res_events, events)[[1]]

  testthat::expect_identical(
    names(data_europe),
    c("age", "bi", "fi", "fc", "ec", "cc")
  )
})

testthat::test_that("subset_event_types() errors for unsupported regions", {
  data_source_meta <-
    data.frame(
      dataset_id = 1,
      region = "Atlantis",
      age_min = 0,
      age_max = 2,
      stringsAsFactors = FALSE
    )

  data_source_dummy_time <-
    data.frame(age = c(0, 1, 2))

  data_source_events <-
    data.frame(
      dataset_id = 1,
      data = I(list(data.frame(age = c(0, 1, 2), bi = c(1, 0, 0))))
    )

  testthat::expect_error(
    subset_event_types(
      data_source_events = data_source_events,
      data_source_meta = data_source_meta,
      data_source_dummy_time = data_source_dummy_time
    ),
    regexp = "Unsupported region"
  )
})