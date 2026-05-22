testthat::test_that("get_map_region() validates raster columns", {
  bad_raster <-
    data.frame(
      x = 1,
      y = 2,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_map_region(rasterdata = bad_raster),
    regexp = "climatezone"
  )
})

testthat::test_that("get_map_region() validates alpha type", {
  bad_raster <-
    data.frame(
      x = 1,
      y = 2,
      climatezone = "A",
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_map_region(rasterdata = bad_raster, sel_alpha = "0.5"),
    regexp = "single numeric"
  )
})
