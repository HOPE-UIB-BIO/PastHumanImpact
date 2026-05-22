testthat::test_that("get_events_spd_combined() validates required columns", {
  bad_events <-
    data.frame(
      dataset_id = 1,
      stringsAsFactors = FALSE
    )

  data_spd <-
    data.frame(
      dataset_id = 1,
      data = I(list(data.frame(age = 100, value = 1, stringsAsFactors = FALSE))),
      stringsAsFactors = FALSE
    )

  data_meta <-
    data.frame(
      dataset_id = 1,
      age_min = 0,
      age_max = 12000,
      stringsAsFactors = FALSE
    )

  dummy_time <-
    data.frame(
      age = seq(0, 12000, by = 500),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_events_spd_combined(
      data_source_events = bad_events,
      data_source_spd = data_spd,
      data_source_meta = data_meta,
      data_source_dummy_time = dummy_time
    ),
    regexp = "dataset_id|events"
  )
})

testthat::test_that("get_events_spd_combined() validates dummy age column", {
  data_events <-
    data.frame(
      dataset_id = 1,
      events = I(list(data.frame(age = 100, bi = 1, stringsAsFactors = FALSE))),
      stringsAsFactors = FALSE
    )

  data_spd <-
    data.frame(
      dataset_id = 1,
      data = I(list(data.frame(age = 100, value = 1, stringsAsFactors = FALSE))),
      stringsAsFactors = FALSE
    )

  data_meta <-
    data.frame(
      dataset_id = 1,
      age_min = 0,
      age_max = 12000,
      stringsAsFactors = FALSE
    )

  bad_dummy <-
    data.frame(
      time = seq(0, 12000, by = 500),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_events_spd_combined(
      data_source_events = data_events,
      data_source_spd = data_spd,
      data_source_meta = data_meta,
      data_source_dummy_time = bad_dummy
    ),
    regexp = "must contain `age`|must contain age"
  )
})
