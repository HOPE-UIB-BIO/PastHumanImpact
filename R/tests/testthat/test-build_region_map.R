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

testthat::test_that("build_region_map() draws borders without deprecation", {
  raster <-
    data.frame(
      x = c(10, 20),
      y = c(50, 55),
      climatezone = factor(c("Polar", "Temperate")),
      stringsAsFactors = FALSE
    )

  # Deprecations are signalled once per session, so turn them into errors.
  withr::local_options(lifecycle_verbosity = "error")

  result <- build_region_map(rasterdata = raster)

  testthat::expect_s3_class(result, "ggplot")
})
