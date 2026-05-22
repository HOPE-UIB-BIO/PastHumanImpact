testthat::test_that("get_climate_indices() validates source structure", {
  bad_source <-
    list(
      data = data.frame(dataset_id = 1, stringsAsFactors = FALSE)
    )

  testthat::expect_error(
    get_climate_indices(
      data_source = bad_source,
      time_ref = data.frame(time_id = 1, age = 100)
    ),
    regexp = "dataset_id|climate"
  )
})

testthat::test_that("get_climate_indices() validates time_ref type", {
  source_data <-
    list(
      data = data.frame(
        dataset_id = 1,
        climate = I(list(data.frame(time_id = 1, variable = "bio1", value = 1))),
        stringsAsFactors = FALSE
      )
    )

  testthat::expect_error(
    get_climate_indices(
      data_source = source_data,
      time_ref = list(time_id = 1)
    ),
    regexp = "time_ref"
  )
})
