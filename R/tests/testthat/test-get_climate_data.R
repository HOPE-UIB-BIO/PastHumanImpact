testthat::test_that("get_climate_data() validates xy required columns", {
  bad_xy <-
    data.frame(
      dataset_id = 1,
      lat = 10,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_climate_data(xy = bad_xy),
    regexp = "dataset_id|long|lat"
  )
})

testthat::test_that("get_climate_data() validates variables_selected type", {
  xy <-
    data.frame(
      dataset_id = 1,
      long = 5,
      lat = 10,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_climate_data(variables_selected = 1, xy = xy),
    regexp = "character vector"
  )
})
