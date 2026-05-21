testthat::test_that("get_events_as_binary() creates binary columns from event ages", {
  data_source_events <-
    data.frame(
      dataset_id = 1,
      events_age = I(list(data.frame(
        name = c("event_a", "event_b"),
        age = c(1, 2)
      ))),
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      region = "Europe",
      levels = I(list(data.frame(age = c(0, 1, 2, 3))))
    )

  res_data <-
    get_events_as_binary(
      data_source_events = data_source_events,
      data_source_pollen = data_source_pollen
    )

  testthat::expect_identical(dplyr::pull(res_data, dataset_id), 1)
  testthat::expect_identical(dplyr::pull(res_data, region), "Europe")
  testthat::expect_identical(
    purrr::pluck(res_data, "events_binary", 1),
    data.frame(
      age = c(0, 1, 2, 3),
      event_a = c(1, 1, 0, 0),
      event_b = c(1, 1, 1, 0)
    )
  )
})

testthat::test_that("get_events_as_binary() sets zeros for NA event age", {
  data_source_events <-
    data.frame(
      dataset_id = 2,
      events_age = I(list(data.frame(name = c("event_a", "event_b"), age = c(2, NA))))
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 2,
      region = "Asia",
      levels = I(list(data.frame(age = c(0, 1, 2, 3))))
    )

  res_data <-
    get_events_as_binary(
      data_source_events = data_source_events,
      data_source_pollen = data_source_pollen
    )

  data_binary <-
    dplyr::pull(res_data, events_binary)[[1]]

  testthat::expect_identical(dplyr::pull(data_binary, event_b), c(0, 0, 0, 0))
})

testthat::test_that("get_events_as_binary() handles single-level ages", {
  data_source_events <-
    data.frame(
      dataset_id = 3,
      events_age = I(list(data.frame(name = "event_x", age = 100)))
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 3,
      region = "Europe",
      levels = I(list(data.frame(age = 100)))
    )

  res_data <-
    get_events_as_binary(
      data_source_events = data_source_events,
      data_source_pollen = data_source_pollen
    )

  data_binary <- dplyr::pull(res_data, events_binary)[[1]]
  testthat::expect_equal(nrow(data_binary), 1L)
  testthat::expect_identical(dplyr::pull(data_binary, event_x), 1)
})