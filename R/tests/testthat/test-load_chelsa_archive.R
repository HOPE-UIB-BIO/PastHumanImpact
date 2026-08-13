testthat::test_that("load_chelsa_archive() validates required columns", {
  bad_md <-
    data.frame(
      id = 1,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    load_chelsa_archive(md = bad_md),
    regexp = "file|url"
  )
})

testthat::test_that("load_chelsa_archive() validates skip_existing type", {
  md <-
    data.frame(
      file = "x.tif",
      url = "https://example.com/x.tif",
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    load_chelsa_archive(md = md, skip_existing = "yes"),
    regexp = "single logical"
  )
})
