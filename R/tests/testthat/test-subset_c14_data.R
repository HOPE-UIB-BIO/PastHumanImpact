testthat::test_that("subset_c14_data() validates required c14 columns", {
  bad_c14 <-
    data.frame(
      LabID = "X1",
      stringsAsFactors = FALSE
    )

  polygons <-
    data.frame(dataset_id = 1, stringsAsFactors = FALSE)

  meta <-
    data.frame(
      dataset_id = 1,
      long = 10,
      lat = 50,
      curve_name = "intcal20",
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    subset_c14_data(
      data_source_c14 = bad_c14,
      data_source_polygons = polygons,
      data_source_meta = meta
    ),
    regexp = "LabID|Age|Error"
  )
})

testthat::test_that("subset_c14_data() validates required meta columns", {
  c14 <-
    data.frame(
      LabID = "X1",
      lat = 50,
      long = 10,
      Age = 1000,
      Error = 30,
      stringsAsFactors = FALSE
    )

  polygons <-
    data.frame(dataset_id = 1, stringsAsFactors = FALSE)

  bad_meta <-
    data.frame(
      dataset_id = 1,
      long = 10,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    subset_c14_data(
      data_source_c14 = c14,
      data_source_polygons = polygons,
      data_source_meta = bad_meta
    ),
    regexp = "curve_name|lat"
  )
})
