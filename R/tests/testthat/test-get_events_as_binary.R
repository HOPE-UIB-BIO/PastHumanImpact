testthat::test_that("get_events_as_binary() returns nested binary event tables", {
  testthat::skip_if_not_installed("REcopol")

  data_source_events <-
    data.frame(
      dataset_id = 1,
      region = "Asia",
      events_age = I(list(data.frame(
        name = c("bi", "fi"),
        age = c(120, 80)
      ))),
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      levels = I(list(data.frame(age = c(150, 100, 50)))),
      stringsAsFactors = FALSE
    )

  res_binary <-
    get_events_as_binary(
      data_source_events = data_source_events,
      data_source_pollen = data_source_pollen,
      verbose = FALSE
    )

  testthat::expect_identical(
    names(res_binary),
    c("dataset_id", "region", "events_binary")
  )
  testthat::expect_identical(
    dplyr::pull(res_binary, dataset_id),
    1
  )
  testthat::expect_identical(
    dplyr::pull(res_binary, region),
    "Asia"
  )

  data_binary <-
    dplyr::pull(res_binary, events_binary)[[1]]

  testthat::expect_true(all(c("age", "bi", "fi") %in% names(data_binary)))
  testthat::expect_true(
    all(
      unlist(data_binary[c("bi", "fi")]) %in% c(0, 1)
    )
  )
})

testthat::test_that("get_events_as_binary() validates required input columns", {
  testthat::expect_error(
    get_events_as_binary(
      data_source_events = data.frame(dataset_id = 1, region = "Asia"),
      data_source_pollen = data.frame(
        dataset_id = 1,
        levels = I(list(data.frame(age = c(1, 2))))
      )
    ),
    regexp = "must contain dataset_id, region, and events_age"
  )
})

testthat::test_that("get_events_as_binary() validates nested list-column tables", {
  data_source_events <-
    data.frame(
      dataset_id = 1,
      region = "Asia",
      events_age = I(list("not_a_table")),
      stringsAsFactors = FALSE
    )

  data_source_pollen <-
    data.frame(
      dataset_id = 1,
      levels = I(list(data.frame(age = c(150, 100, 50)))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_events_as_binary(
      data_source_events = data_source_events,
      data_source_pollen = data_source_pollen,
      verbose = FALSE
    ),
    regexp = "events_age"
  )
})