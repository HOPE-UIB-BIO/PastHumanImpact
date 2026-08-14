testthat::test_that("build_region_map() validates raster columns", {
  bad_raster <-
    data.frame(
      x = 1,
      y = 2,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    build_region_map(rasterdata = bad_raster),
    regexp = "climatezone"
  )
})

testthat::test_that("build_region_map() validates alpha type", {
  bad_raster <-
    data.frame(
      x = 1,
      y = 2,
      climatezone = "A",
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    build_region_map(rasterdata = bad_raster, sel_alpha = "0.5"),
    regexp = "single numeric"
  )
})
