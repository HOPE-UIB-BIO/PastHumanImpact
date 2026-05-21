testthat::test_that("get_events_for_modelling pivots nested events to var groups", {
  data_source_events <-
    data.frame(
      events_updated = I(list(data.frame(
        dataset_id = c(1, 1),
        age = c(100, 200),
        weak = c(1, 0),
        strong = c(0, 1),
        no_impact = c(0, 0)
      ))),
      stringsAsFactors = FALSE
    )

  result <-
    get_events_for_modelling(data_source_events)

  vec_var_name <-
    result[["var_name"]]

  data_weak <-
    purrr::pluck(result, "data_to_fit", 1)

  data_strong <-
    purrr::pluck(result, "data_to_fit", 2)

  data_no_impact <-
    purrr::pluck(result, "data_to_fit", 3)

  testthat::expect_identical(vec_var_name, c("weak", "strong", "no_impact"))
  testthat::expect_identical(
    as.data.frame(data_weak),
    data.frame(
      dataset_id = c(1, 1),
      age = c(100, 200),
      value = c(1, 0)
    )
  )
  testthat::expect_identical(
    as.data.frame(data_strong),
    data.frame(
      dataset_id = c(1, 1),
      age = c(100, 200),
      value = c(0, 1)
    )
  )
  testthat::expect_identical(
    as.data.frame(data_no_impact),
    data.frame(
      dataset_id = c(1, 1),
      age = c(100, 200),
      value = c(0, 0)
    )
  )
})

testthat::test_that("get_events_for_modelling() drops NA values", {
  data_source_events <-
    data.frame(
      events_updated = I(list(data.frame(
        dataset_id = c(1, 1),
        age = c(100, 200),
        weak = c(1, NA),
        strong = c(0, 1)
      ))),
      stringsAsFactors = FALSE
    )

  result <- get_events_for_modelling(data_source_events)

  data_weak <-
    dplyr::filter(result, var_name == "weak") |>
    dplyr::pull(data_to_fit) |>
    .subset2(1L)

  testthat::expect_equal(nrow(data_weak), 1L)
  testthat::expect_identical(dplyr::pull(data_weak, age), 100)
})

testthat::test_that("get_events_for_modelling() returns expected var groups", {
  data_source_events <-
    data.frame(
      events_updated = I(list(data.frame(
        dataset_id = c(1, 1, 1),
        age = c(100, 200, 300),
        weak = c(1, 0, 1),
        strong = c(0, 1, 0),
        no_impact = c(0, 0, 0)
      ))),
      stringsAsFactors = FALSE
    )

  result <- get_events_for_modelling(data_source_events)

  testthat::expect_identical(
    sort(dplyr::pull(result, var_name)),
    c("no_impact", "strong", "weak")
  )
  testthat::expect_true(all(purrr::map_lgl(dplyr::pull(result, data_to_fit), ~ all(c("dataset_id", "age", "value") %in% names(.x)))))
})