testthat::test_that("get_chelsa_trace21k_urls generates distinct rows with correct URL structure", {
  # Minimal call: 1 bio variable, 1 time step
  result <- get_chelsa_trace21k_urls(
    variables  = "bio",
    bio_var    = 1L,
    month_var  = 1L,
    time_var   = 0L
  )

  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_true("url" %in% names(result))
  testthat::expect_true("file" %in% names(result))

  # URL should start with CHELSA base and contain BIO01
  testthat::expect_true(
    grepl("^https://", dplyr::pull(result, url)[[1]])
  )
  testthat::expect_true(
    grepl("bio01", dplyr::pull(result, file)[[1]], ignore.case = TRUE)
  )
})

testthat::test_that("get_chelsa_trace21k_urls generates correct count for non-bio variable", {
  result <- get_chelsa_trace21k_urls(
    variables  = "tasmin",
    bio_var    = 1L,
    month_var  = c(1L, 2L),
    time_var   = c(0L, -1L)
  )

  # 2 months x 2 time steps = 4 distinct rows
  testthat::expect_equal(nrow(result), 4L)
  testthat::expect_true(
    all(grepl("tasmin", dplyr::pull(result, file)))
  )
})
