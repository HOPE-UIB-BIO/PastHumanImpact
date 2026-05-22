testthat::test_that("get_polygons() validates required columns", {
  bad_source <-
    data.frame(
      dataset_id = 1,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_polygons(data_source = bad_source),
    regexp = "dataset_id|long|lat"
  )
})

testthat::test_that("get_polygons() validates n_points", {
  data_source <-
    data.frame(
      dataset_id = 1,
      long = 10,
      lat = 50,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_polygons(data_source = data_source, n_points = 2),
    regexp = "n_points"
  )
})
