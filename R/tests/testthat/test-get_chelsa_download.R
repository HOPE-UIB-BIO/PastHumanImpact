testthat::test_that("get_chelsa_download() validates required columns", {
  bad_md <-
    data.frame(
      id = 1,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_chelsa_download(md = bad_md),
    regexp = "file|url"
  )
})

testthat::test_that("get_chelsa_download() validates skip_existing type", {
  md <-
    data.frame(
      file = "x.tif",
      url = "https://example.com/x.tif",
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_chelsa_download(md = md, skip_existing = "yes"),
    regexp = "single logical"
  )
})
